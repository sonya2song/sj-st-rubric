setwd('~/Dropbox/code/sj-st-rubric/round3/')
df <- read.csv('responses.csv', as.is=TRUE)
head(df)
names(df) <- c(
    "coder",
    "v1",
    "v2",
    "v3",
    "v4",
    "v5",
    "v6",
    "v7",
    "v8",
    "v9",
    "url",
    "content",
    "annotations",
    "ts",
    "headline",
    "check"
  )

head(df)