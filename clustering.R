library(readr)
library(MCMCpack)
library(plyr)

#importing and normalizing data
s_s_df <- read_csv("top_uni_tri.csv")
dtm <- as.matrix(s_s_df[,-(1:2)])
rownames(dtm) <- paste("PR", 1:1338)
dtm_norm <- dtm
for(z in 1:nrow(dtm_norm)){
    dtm_norm[z,]<- dtm_norm[z,]/sum(dtm_norm[z,])
}

#plotting kmeans objective fxn as clusters 2 -> n-1
clust_seq <- seq(2, 1337, 100)

counter <- 1
obj <- c()
for (i in clust_seq) {
  obj[counter] <- kmeans(dtm_norm, i)$tot.withinss
  counter <- counter + 1
}
plot(clust_seq, obj, type = "b", xlab="number of clusters")

#kmeans w/ 6 clusters
set.seed(202436)
n.clust <- 6
k_cluster<- kmeans(dtm_norm, centers = n.clust)
table(k_cluster$cluster)

#top ten words from Diffk / cluster
key_words<- matrix(NA, nrow=n.clust, ncol=10)
for(z in 1:n.clust){
  key_words[z,]<- colnames(dtm_norm)[order(k_cluster$center[z,], decreasing=T)[1:10]]
}
key_words

#mixture of multinomials with 6 components 
#Thanks to J. Grimmer for his help w/ mix_mult()
mix_mult<- function(X, k, tol, seed){
  ##initializing parameters
  set.seed(seed)
  pis<- rdirichlet(1, alpha = rep(100, k))
  thetas<- matrix(NA, nrow=k, ncol=ncol(X))
  for(z in 1:k){
    thetas[z,]<- rdirichlet(1, alpha=rep(100, ncol(X)))
  }
  rs<- matrix(NA, nrow=nrow(X),ncol=k)
  a<- 0
  t<- 1 
  ##computing the expected value
  e.log<- function(X, pis, thetas, rs){
    log.pis<- log(pis)
    log.thetas<- log(thetas)
    score<- 0
    for(z in 1:nrow(X)){
      part1<- rs[z,]*log.pis
      part2<- 0
      for(j in 1:k){
        part2<- part2 + sum(rs[z,j]*X[z,]*log(thetas[j,] + .000001))
      }
      score<- score + sum(part1) + part2
    }
    return(score)
  }
  ##iterating while 
  while(a==0){
    for(i in 1:nrow(X)){
      for(j in 1:k){
        denom<- thetas[j,]^{-X[i,]}
        nums<- thetas[-j,]
        new_num<- 0
        for(l in 1:nrow(nums)){
          new_num<- new_num + (pis[l]/pis[j])*prod(nums[l,]^{X[i,]}*denom)}
        rs[i,j]<- ifelse(is.na(1/(1 + new_num))==F,1/(1 + new_num), 0)
      }
    }
    e.old<- e.log(X, pis,thetas,  rs)
    ##maximizing parameters
    thetas<- t(rs)%*%X
    for(z in 1:k){
      thetas[z,]<- (thetas[z,] )/(sum(thetas[z,] ) )
    }
    pis<- apply(rs, 2, sum)/sum(rs)
    t<- t + 1
    if(t>1){
      e.new<- e.log(X, pis, thetas, rs)
      change<- e.new - e.old
      print(abs(change))
      if(abs(change)<tol){
        a<- 1}
    }
  }
  out<- list(thetas, pis, rs)
  names(out)<- c('thetas', 'pis', 'rs')
  return(out)
}   
k<- 6
test<- mix_mult(dtm, k, 1e-5, 13950431)


#kmeans / mix of multinomials confusion matrix
table(apply(test$rs, 1, which.max), k_cluster$cluster)
