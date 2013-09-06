setwd("~/Dropbox/code/sj-st-rubric/round1")
b = read.csv("brian.csv", as.is=T)
s = read.csv("sonya.csv", as.is=T)

# count up total scores
sum_vars = function(df) {
  df$score = rowSums(df[,grep("V[0-9]", names(df))])
  return(df)
}
b = sum_vars(b)
s = sum_vars(s)

# correlation of coding

code_cor = data.frame(
  TOT = cor(b$score, s$score),
  V1 = cor(b$V1, s$V1),
  V2 = cor(b$V2, s$V2),
  V3 = cor(b$V3, s$V3),
  V4 = cor(b$V4, s$V4),
  V5 = cor(b$V5, s$V5),
  V6 = cor(b$V6, s$V6),
  V7 = cor(b$V7, s$V7),
  V8 = cor(b$V8, s$V8),
  V9 = cor(b$V9, s$V9)
)

#brians top three
b[order(b$score, decreasing=TRUE), ]$HEADLINE[1:3]

# sonyas top three
s[order(s$score, decreasing=TRUE), ]$HEADLINE[1:3]

#brians bottom three
b[order(b$score), ]$HEADLINE[1:3]

# sonyas bottom three
s[order(s$score), ]$HEADLINE[1:3]

# question-level correlations
code_cor