#importing data
df_dtm <- read_csv("top_uni_tri.csv")
dtm <- as.matrix(df_dtm[,-(1:2)])

#applying prcomp fxn
pr_dtm <- prcomp(dtm, scale = TRUE)

#plot of variance explained/component
eigs <- pr_dtm$sdev ** 2
proportions <- eigs/sum(eigs)
cumulative <- cumsum(eigs)/sum(eigs)
plot(1:1338, proportions, type = "l")

#plot of 2d embedding labelled with filenames
bp_labels = list()
for (i in 1:1338) {
  bp_labels[i] = paste(df_dtm$speaker[i], df_dtm$date[i])
}
plot(pr_dtm$x[,1], pr_dtm$x[,2], pch="")
text(pr_dtm$x[237:1338,1], pr_dtm$x[237:1338,2], labels = bp_labels[237:1338], cex = 0.6, col = "red")
text(pr_dtm$x[1:236,1], pr_dtm$x[1:236,2], labels = bp_labels[1:236], cex = 0.6, col = "blue")

#largest principle components for each
sort(pr_dtm$rotation[,1], decreasing = TRUE)[1:20]
sort(pr_dtm$rotation[,2], decreasing = TRUE)[1:20]
