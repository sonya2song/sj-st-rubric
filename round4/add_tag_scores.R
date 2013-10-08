# setwd('~/Dropbox/code/sj-st-rubric/round3/')
df <- read.csv('responses.csv', as.is=TRUE)
# rm(list=ls())
library('rjson')
library('plyr')

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

extract_tag_info <- function(tag, text) {
  
  text <- df[1, ]$content
  tags <- df[1, ]$annotations
  json_tags = fromJSON(tags)
  text <- iconv(gsub('<br>', '', text), "UTF-8")
  text_length <- nchar(text)
  
  json_tags$text
  
  tag_text <- gsub('<br>', '', tag$text)
  text <- gsub('<br>', '', text)
  tag_label <- tag$tag
  tag_length <- nchar(tag_text) 
  start_pattern <- substring(tag_text, 1, 20)
  start <- str_locate(text, start_pattern)[1]
  end <- start + tag_length

  return(
    data.frame(
      tag = tag_label,
      length = tag_length,
      tag_start = start,
      tag_end = end,
      stringsAsFactors=FALSE
    )
  )
}


tag_scores <- function(tags, text) {
  text <- df[1, ]$content
  tags <- df[1, ]$annotations
  json_tags = fromJSON(tags)
  text <- iconv(gsub('<br>', '', text), "UTF-8")
  text_length <- nchar(text)

  # generate tag_df
  tag_df <- ldply(json_tags, extract_tag_info, text)
   
  solution_df = tag_df[tag_df$tag=='solution',]
  problem_df = tag_df[tag_df$tag=='problem',]
  response_df = tag_df[tag_df$tag=='result',]
  
  return(
    data.frame(
      per_solution = sum(solution_df$length) / text_length * 100,
      per_problem = sum(problem_df$length) / text_length * 100,
      per_response = sum(response_df$length) / text_length * 100,
      first_mention = tag_df$tag[which.min(tag_df$tag_start)],
      min_pos_solution = min(solution_df$tag_start) / text_length * 100,
      min_pos_problem = min(problem_df$tag_start) / text_length * 100 ,
      min_pos_response = min(response_df$tag_start) / text_length * 100,
      avg_pos_solution = mean(solution_df$tag_start) / text_length * 100,
      avg_pos_problem = mean(problem_df$tag_start) / text_length * 100 ,
      avg_pos_response = mean(response_df$tag_start) / text_length * 100,
      stringsAsFactors=FALSE
      )
    )
}

add_tag_scores <- function(df) {
  new_df = data.frame()
  for (i in 1:nrow(df)) {
    print(i)
    text <- df[i, ]$content
    tags <- df[i, ]$annotations
    
    tag_df <- tag_scores(tags, text)
    new_df <- rbind(new_df, cbind(df[i,], tag_df))
  }
  return(new_df)
}

tag_df <- add_tag_scores(df)

write.csv(tag_df, 'responses_with_tags.csv', row.names=FALSE)