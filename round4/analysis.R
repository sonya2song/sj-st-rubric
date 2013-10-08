df <- read.csv('responses.csv', as.is=TRUE, encoding="UTF-8", header=T)
library('rjson')
library('plyr')
library(doMC)
library('stringr')
library('hash')
registerDoMC(2)
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

for (i in 1:nrow(df)) {
  print(1)
  text <- df[1, ]$content
  tag <- df[1, ]$annotations

  tags <- data.frame(matrix(unlist(fromJSON(tag)), ncol=2, byrow=T), stringsAsFactors=F)
  colnames(tags) <- c('label', 'text')
  tags$text
  tag_text <- gsub('<br>', '', tags$text)
  length(tag_text)
  tag_label <- tags$label
  tag_label
  
  #   text = df[1,]$content
  text <- gsub('<br>', '', text)
  text
  text_length <- nchar(text)
  
  #   tags_length = hash(keys=c(keys="solution","result","problem"),values=rep(0,3))
  #   tags_length
  
  tag_result <- data.frame(label=tags$label,start=rep(0,nrow(tags)),length=rep(0,nrow(tags)),end=rep(0,nrow(tags)))
  summary(tag_result)
  tag_stats <- data.frame(label=tags$label,start=rep(0,nrow(tags)),length=rep(0,nrow(tags)),end=rep(0,nrow(tags)))
  summary(tag_result)
  
  for (j in 1:nrow(tags)) {
    start_pattern <- substring(tag_text[j], 1, 20)
    start <- str_locate(text, start_pattern)[1]
    tag_length <- nchar(tag_text[j])
    end <- start + tag_length
    temp <- tags$label[j]
    tag_result[j,] <- c(temp,start,tag_length,end)
  }
  
  tag_result
  str(tag_result)
  tag_result$start <- as.numeric(tag_result$start)
  tag_result$length <- as.numeric(tag_result$length)
  tag_result$end <- as.numeric(tag_result$end)
  
  tag_result$label
  per_tag <- data.frame(tapply(tag_result$length, tag_result$label, sum) / text_length * 100)
  str(per_tag)
  per_tag["solution",]
  
  first_tag <- data.frame(tapply(tag_result$start, tag_result$label, min) / text_length * 100)
  str(first_tag)
  first_tag["solution",]
  
  tag_stats <- data.frame(
    per_solution = per_tag["solution",], 
    per_problem = per_tag["problem",],
    per_result = per_tag["result",],
    first_solution = first_tag["solution",],
    first_problem = first_tag["problem",],
    first_result = first_tag["result",]
  )
  
  str(tag_stats)
  tag_df <- rbind(tag_df, tag_stats)
}

df <- cbind(df, tag_stats)

head(df)
sample_json <- df[1,]$annotations
text <- df$content[1]
sample_json[[1]]
tag_scores(df$annotations[1], text)
start_pattern <- substring(sample_json[[1]]$text, 1, 20)
start_pattern



nchar(text)
aregexec(pattern=, text=sample_content)
sample_content

sample_json[[1]]$text