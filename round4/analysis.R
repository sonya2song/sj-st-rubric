df <- read.csv('responses.csv', as.is=TRUE, encoding="UTF-8", header=T)
library('rjson')
library('plyr')
library(doMC)
library('stringr')
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

extract_tag_info <- function(tag, text) {
  tag = fromJSON(df[1,]$annotations)
  tags <- data.frame(matrix(unlist(tag), ncol=2, byrow=T), stringsAsFactors=F)
  colnames(tags) <- c('label', 'text')
  tags$text
  tag_text <- gsub('<br>', '', tags$text)
  length(tag_text)
  tag_label <- tags$label
  tag_label
  
  text = df[1,]$content
  text <- gsub('<br>', '', text)
  text
  
  tags_length = vector(mode='list', length=3)
  tags_length
  names(tags_length) = c('solution', 'problem', 'result')
  tags_length['solution'] <- 0
  tags_length['problem'] <- 0
  tags_length['result'] <- 0
  tags_length
  
  for (i in 1:nrows(tags)) {
    start_pattern <- substring(tag_text[i], 1, 20)
    start <- str_locate(text, start_pattern)[1]
    tag_length <- nchar(tag_text[i])
    end <- start + tag_length
    c(start, tag_length, end)  
    tags[1]$label
    tags_length["tags[1]$label"] 
    = tag_length + tags_length[tags[i]$label]
  }
    
  

  tag_results = data.frame(
    tag = tag_label,
    length = tag_length,
    tag_start = start,
    tag_end = end,
    stringsAsFactors=FALSE
  )
  
  return(tag_results)
}


tag_scores <- function(tags, text) {
  tags = df$annotations[1]
  json_tags = fromJSON(tags)
  text <- iconv(gsub('<br>', '', text), "UTF-8")
  text
  text_length <- nchar(text)
  
  # generate tag_df
#   tag_df <- ldply(sample_json, extract_tag_info, text)
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

for (i in 1:nrow(df)) {
  print(i)
  text <- df[i, ]$content
  tags <- df[i, ]$annotations
  
  tag_df <- tag_scores(tags, text)
  df[i, ] <- cbind(df[i,], tag_df)
}
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