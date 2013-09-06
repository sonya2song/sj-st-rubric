rm(list=ls())
setwd("~/Dropbox/code/sj-st-rubric/round2/")
library("ggplot2")
library("plyr")
# read in data
t = read.csv("tina.csv", stringsAsFactors=F)
k = read.csv("keith.csv", stringsAsFactors=F)
s = read.csv("sonya.csv", stringsAsFactors=F)

# count up total scores
sum_vars = function(df) {
  df$score = rowSums(df[,grep("V[0-9]", names(df))])
  return(df)
}
t = sum_vars(t)
k = sum_vars(k)
s = sum_vars(s)

# keith and sonya
cat("keith and sonya's correlation is:", cor(as.numeric(as.factor(k$score)), as.numeric(as.factor(t$score))))

# keith and tinas
cat("keith and tina's correlation is:", cor(t$score, k$score))

# keith and tinas
cat("tina and sonya's correlation is:", cor(t$score, s$score))

# sonyas top three
cat("sonya's top three is:")
s[order(s$score, decreasing=TRUE), ][c("HEADLINE", "score")]

# tinas top three
cat("tina's top three is:")
t[order(t$score, decreasing=TRUE), ][c("HEADLINE", "score")]

# keiths top three
cat("keith's top three is:")
k[order(k$score, decreasing=TRUE), ][c("HEADLINE", "score")]

#sonyas bottom three
cat("sonya's bottom three is:")
s[order(s$score), ]$HEADLINE[1:3]

# tinas bottom three
cat("tina's bottom three is:")
t[order(t$score), ]
t[order(t$score), ][,c("HEADLINE", "score")]

# keiths bottom three
cat("keith's bottom three is:")
k[order(k$score), ]$HEADLINE[1:3]

# let's analyze everything
k$Coder = "keith"
s$Coder = "sonya"
t$Coder = "tina"
all = rbind(k, s, t)

# question score
ggplot(all, aes(as.factor(ID), score)) + 
  geom_point(position=position_jitter(width=0.1), 
             aes(colour = Coder),
             size=20,
             shape="*") +
  xlab("Article") +
  ylab("SJ-Score") 

# principal components
pc_data = all[,grep("V[0-9]|score", names(all))]
summary(glm(score ~ V7, data=pc_data))
pc <- princomp(pc_data)
pc$scores
pc$loadings 
biplot(pc)

#lets analyze each question
transpose_data = function(df) {
  df = t(df[,grep("V[0-9]", names(df))])
  colnames(df) = 10:19
  return(df)
}



