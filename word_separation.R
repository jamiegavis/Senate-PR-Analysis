library(readr)
library(MCMCpack)
library(plyr)

#loading data, rows 1:236 are Sessions
df_dtm <- read_csv("top_uni_tri.csv")
dtm <- as.matrix(df_dtm[,-(1:2)])
unigrams<- read_csv("top_unigrams.csv")
trigrams<- read_csv("top_trigrams.csv")


#word separation w/ independent linear discriminant 
#IDL requires a DTM w/ rates not frequencies- so we divide each word frequency by the # words in the document [sum of that row]
uni_rate_mat <- as.matrix(unigrams[,-(1:2)])
tri_rate_mat <- as.matrix(trigrams[,-(1:2)])
for (i in 1:1338){
  uni_rate_mat[i,] <- uni_rate_mat[i,] / sum(uni_rate_mat[i,])
  tri_rate_mat[i,] <- tri_rate_mat[i,] / sum(tri_rate_mat[i,])
}
#mu:
u_muShelby <- colMeans(uni_rate_mat[1:236,], na.rm = TRUE)
u_muSessions <- colMeans(uni_rate_mat[237:1338,], na.rm = TRUE)
t_muShelby <- colMeans(tri_rate_mat[1:236,], na.rm = TRUE)
t_muSessions <- colMeans(tri_rate_mat[237:1338,], na.rm = TRUE)
#variance:
u_varShelby <- apply(uni_rate_mat[1:236,], 2, var)
u_varSessions <- apply(uni_rate_mat[237:1338,], 2, var) 
t_varShelby <- apply(tri_rate_mat[1:236,], 2, var)
t_varSessions <- apply(tri_rate_mat[237:1338,], 2, var)
#thetas (weights):
u_theta <- (u_muSessions - u_muShelby) / (u_varSessions + u_varSessions)
t_theta <- (t_muSessions - t_muShelby) / (t_varSessions + t_varSessions)
#graphing top 10 words by independent linear discriminant 
#y-value is filler to make graph more legible. 
show_theta <- c(sort(u_theta)[1:10], sort(u_theta, decreasing=T)[1:10])
plot(show_theta, 1:20, pch="", main="Independent Linear Discriminant")
text(show_theta, 1:20, label=names(show_theta))


#word separation w/ standardized mean difference
u_std_m_diff <- (u_muSessions - u_muShelby) / sqrt(u_varSessions + u_varShelby)
t_std_m_diff <- (t_muSessions - t_muShelby) / sqrt(t_varSessions + t_varShelby)
#top 10 for each graphed, y axis only for legibility 
show_mean <- c(sort(u_std_m_diff)[1:10], sort(u_std_m_diff, decreasing=T)[1:10])
plot(show_mean, 1:20, pch="", main="Standardized Mean Difference")
text(show_mean, 1:20, label=names(show_mean))


#word separation w/ standardized log odds
#back to frequency matrices 
uni_mat <- as.matrix(unigrams[,-(1:2)])
tri_mat <- as.matrix(trigrams[,-(1:2)])
#N_Speaker = total number of words for that speaker
u_N_Sessions <- sum(colSums(uni_mat[1:236,]))
u_N_Shelby <- sum(colSums(uni_mat[237:1338,]))
t_N_Sessions <- sum(colSums(tri_mat[1:236,]))
t_N_Shelby <- sum(colSums(tri_mat[237:1338,]))
#pi
alpha <- 1
u_piSessions <- (colSums(uni_mat[1:236,]) + alpha) / (u_N_Sessions + (ncol(uni_mat) * alpha))
u_piShelby <- (colSums(uni_mat[237:1338,]) + alpha) / (u_N_Shelby + (ncol(uni_mat) * alpha))
t_piSessions <- (colSums(tri_mat[1:236,]) + alpha) / (t_N_Sessions + (ncol(tri_mat) * alpha))
t_piShelby <- (colSums(tri_mat[237:1338,]) + alpha) / (t_N_Shelby + (ncol(tri_mat) * alpha))
#log odds ratio
u_std_log_odds <- log(u_piSessions / (1 - u_piSessions)) - log(u_piShelby / (1 - u_piShelby))
t_std_log_odds <- log(t_piSessions / (1 - t_piSessions)) - log(t_piShelby / (1 - t_piShelby))
#var(log odds ratio)
u_var_log_odds <- 1 / (colSums(uni_mat[1:236,]) + alpha) + 
  1 / (colSums(uni_mat[237:1338,]) + alpha)
t_var_log_odds <- 1 / (colSums(tri_mat[1:236,]) + alpha) + 
  1 / (colSums(tri_mat[237:1338,]) + alpha)
#standardized log odds
u_s_l_odds <- u_std_log_odds / sqrt(u_var_log_odds)
t_s_l_odds <- t_std_log_odds / sqrt(t_var_log_odds)
#top 10 for each graphed, y axis only for legibility 
show_odds <- c(sort(u_s_l_odds)[1:10], sort(u_s_l_odds, decreasing=T)[1:10])
plot(show_odds, 1:20, pch="", main="Standardized Log Odds")
text(show_odds, 1:20, label=names(show_odds))

