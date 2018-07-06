library(stm)

#importing data
df_dtm <- read_csv("top_uni_tri.csv")
dtm <- as.matrix(df_dtm[,-(1:2)])

#converting to list of DFs for STM 
for_stm <- list()
for(a in 1:nrow(dtm)) { 
  words <- c()
  counts <- c()
  indexes <- which(dtm[a,] != 0)
  if (length(indexes)!=0) {
    for (b in 1:length(indexes)){
      index <- indexes[b]
      words[b] <- index
      counts[b] <- dtm[a, index]
    }
  for_stm[[a]] <- rbind(words, counts)
  }
}

#fitting structural topic model
stm_model <- stm(for_stm, colnames(dtm), 8)
#autolabels
labelTopics(stm_model) 
