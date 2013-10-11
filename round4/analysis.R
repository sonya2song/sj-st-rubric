df <- read.csv('responses_v10.csv', encoding="UTF-8", header=T, as.is=T)
library('rjson')
library('stringr')
library('ggplot2')
library('reshape2')
library('colorspace')
library('Hmisc')
names(df) <- c(
    "coder",
    "v01",
    "v02",
    "v03",
    "v04",
    "v05",
    "v06",
    "v07",
    "v08",
    "v09",
    "v10",
    "url",
    "content",
    "annotations",
    "ts",
    "headline",
    "check"
  )

# str(df)

sj <- c(0, 1, 0, 1, 1, 0, 0, 1, 0, 1)
qj <- 1 - sj

df[df=="Yes" | df=="Clearly"] <- 2
df[df=="Somewhat"] <- 1
df[df=="No"] <- 0

for (i in 2:11){
  df[,i]<-as.numeric(df[,i])  
}
summary(df[,2:11])

tag_df <- data.frame()
# str(tag_df)

for (i in 1:nrow(df)) {
#   i <- 24
  print(i)
  text <- df[i, ]$content
  tag <- df[i, ]$annotations

  tags <- data.frame(matrix(unlist(fromJSON(tag)), ncol=2, byrow=T), stringsAsFactors=F)
  colnames(tags) <- c('label', 'text')
  tag_text <- gsub('<br>', '', tags$text)
  tag_label <- tags$label
  
  text <- gsub('<br>', '', text)
  text_length <- nchar(text)
  
  #   tags_length = hash(keys=c(keys="solution","result","problem"),values=rep(0,3))
  #   tags_length
  
  tag_result <- data.frame(label=tags$label,start=rep(0,nrow(tags)),length=rep(0,nrow(tags)),end=rep(0,nrow(tags)))
#   summary(tag_result)
  tag_stats <- data.frame(label=tags$label,start=rep(0,nrow(tags)),length=rep(0,nrow(tags)),end=rep(0,nrow(tags)))
#   summary(tag_result)
  
  for (j in 1:nrow(tags)) {
#     j <- 4
    print(j)
    start_pattern <- substring(tag_text[j], 1, 30)
    print(start_pattern)
    start_pattern <- iconv(start_pattern, "UTF-8", "latin1", "?")
    start_pattern <- gsub("^[^0-9a-zA-Z]*", "", start_pattern, perl=F)    
    print(start_pattern)
    start_pattern <- strsplit(start_pattern, "\\?")[[1]][1]
#     start_pattern <- gsub("\\?*$", "", start_pattern, perl=F)
#     start_pattern <- substring(start_pattern, 1, str_locate(start_pattern, fixed("?"))[1]-1)
    print(start_pattern)
#   escape special chars in the pattern, such as ? etc. 
#     str_locate(text, fixed(start_pattern))
    start <- str_locate(text, fixed(start_pattern))[1]
    tag_length <- nchar(tag_text[j])
    end <- start + tag_length
    temp <- tags$label[j]
    tag_result[j,] <- c(temp,start,tag_length,end)
    print (c(j, temp, start, start_pattern))
  }
  
  tag_result <- rbind(tag_result, data.frame(label=c("solution","result","problem"), start=rep(text_length,3), length=rep(0,3), end=rep(text_length,3)))
  
  tag_result$start <- as.numeric(tag_result$start)
  tag_result$length <- as.numeric(tag_result$length)
  tag_result$end <- as.numeric(tag_result$end)
  print(tag_result)
  
  per_tag <- data.frame(tapply(tag_result$length, tag_result$label, sum) / text_length * 100)
  min_tag <- data.frame(tapply(tag_result$start, tag_result$label, min) / text_length * 100) 
  print(per_tag)
  print(min_tag)
#   first_mention <- rownames(min_tag)[which(min_tag==min(min_tag[!is.na(min_tag),]))]
  first_mention <- rownames(min_tag)[which(min_tag==min(min_tag))]
  print (first_mention)

  tag_stats <- data.frame(
    per_solution = per_tag["solution",], 
    per_result = per_tag["result",],
    per_problem = per_tag["problem",],
    min_solution = min_tag["solution",],
    min_problem = min_tag["problem",],
    min_result = min_tag["result",],
    first_mention = first_mention
  )
  
#   str(tag_stats)
#   str(tag_df)
  tag_df <- rbind(tag_df, tag_stats)
}

df <- cbind(df, tag_df)
sj_score <- data.matrix(df[,2:11]) %*% cbind(sj,qj)
# summary(sj_score)
df <- cbind(df, sj_score)

write.csv(df, "responses_with_tags.csv", row.names=F)


#### solutions journalism vs. single items ####

names(df)
# df_num <- df[,c(2:11, 18:23, 25, 26)]
df_num <- df[,c(2:11, 25, 26)]
str(df_num)

df_num_m <-melt(cor(df_num))
names(df_num_m) <- c("Var1", "Var2", "Correlation")
qplot(x=Var1, y=Var2, data=df_num_m, fill=Correlation, geom="tile")+
  xlab("")+
  ylab("")+
  ggtitle("Correlation Matrix of Rubric Parameters")+
  theme(
    plot.title = element_text(size=22, face="bold"), 
#     legend.position="top", 
    legend.title = element_text(size=18, face="bold"), 
    legend.text = element_text(size = 18),
    axis.title.x=element_text(face="bold", size=18), 
    axis.title.y=element_text(face="bold", size=18), 
    axis.text.y=element_text(size=15, vjust=.5)
  )



#### solutions journalism vs. quality of journalism (Inter-coder reliability) ####

sj_score <- data.frame(coder=df$coder, headline=df$headline, score=df$sj, dimension=rep("Solutions Journalism",nrow(df)))
qj_score <- data.frame(coder=df$coder, headline=df$headline, score=df$qj, dimension=rep("Quality of Journalism",nrow(df)))
score <- rbind(sj_score, qj_score)
# score

jitter <- .3

ggplot(score, aes(y=headline))+
  geom_point(aes(x=score, color=coder, shape=dimension, size=dimension), position=position_jitter(width=jitter, height=0))+
  scale_shape_manual(values=c(18,21))+
  scale_size_manual(values=c(15,10))+
  ggtitle("Inter-coder Reliability (Solutions Journalism vs. Quality of Journalism)")+
  theme(
    plot.title = element_text(size=22, face="bold"), 
    legend.position="top", 
    legend.title = element_text(size=18, face="bold"), 
    legend.text = element_text(size = 18),
    axis.title.x=element_text(face="bold", size=18), 
    axis.title.y=element_text(face="bold", size=18), 
    axis.text.y=element_text(size=15, vjust=.5)
    )


#### percentage of sj vs. position of first solution tag ####

sj_per <- data.frame(coder=df$coder, per_solution=df$per_solution, min_solution=df$min_solution, score=df$sj, dimension=rep("Solutions Journalism",nrow(df)))
qj_per <- data.frame(coder=df$coder, per_solution=df$per_solution, min_solution=df$min_solution, score=df$qj, dimension=rep("Quality of Journalism",nrow(df)))
per <- rbind(sj_per, qj_per)
df[,c('coder','per_sj')]
per$per_solution_int <- 0
per[per$coder == 'David Bornstein',]$per_solution_int <- floor(per[per$coder == 'David Bornstein',]$per_solution/max(per[per$coder == 'David Bornstein',]$per_solution)*6)
per[per$coder == 'Keith',]$per_solution_int <- floor(per[per$coder == 'Keith',]$per_solution/max(per[per$coder == 'Keith',]$per_solution)*6)
per$percentage_of_solution <- factor(per$per_solution_int)
per[per$coder == 'David Bornstein',]$per_solution_int
per[per$coder == 'Keith',]$per_solution_int

ggplot(per, aes(x=min_solution))+
  geom_point(aes(y=score, color=coder, shape=dimension, size=percentage_of_solution), position=position_jitter(height=jitter, width=0))+
  scale_shape_manual(values=c(18,21))+
  scale_size_manual(values=1:7*4)+
  ggtitle("Position of First Solution Tag vs. Score & Percentage of Solutions in Text")+
  xlab("Position of First Solution Tag")+
  ylab("Solutions Journalism or Quality of Journalism")+
  theme(
    plot.title = element_text(size=22, face="bold"), 
    legend.position="top", 
    legend.title = element_text(size=18, face="bold"), 
    legend.text = element_text(size = 18),
    axis.title.x=element_text(face="bold", size=18), 
    axis.title.y=element_text(face="bold", size=18), 
    axis.text.y=element_text(size=15, vjust=.5)
  )

#### scatterplot SJ vs. QJ ####
ggplot(df, aes(sj, qj))+
  geom_point(aes(color=coder), shape="*", size=24)+
  ggtitle("Solutions Journalism vs. Quality of Journalism")+
  xlab("Solutions Journalism")+
  ylab("Quality of Journalism")+
  theme(
    plot.title = element_text(size=22, face="bold"), 
    legend.position="top", 
    legend.title = element_text(size=18, face="bold"), 
    legend.text = element_text(size = 18),
    axis.title.x=element_text(face="bold", size=18), 
    axis.title.y=element_text(face="bold", size=18), 
    axis.text.y=element_text(size=15, vjust=.5)
  )

rcorr(cbind(df$sj, df$qj), type='pearson')

sj_diff <- (unlist(tapply(df$sj, df$headline, function(x) {abs(x[1]-x)})))
print (sj_diff[1:14*2])
order(sj_diff[1:14*2])
names(sj_diff[1:14*2])[order(sj_diff[1:14*2])]
cbind(no=1:14, disagreement=sj_diff[1:14*2][order(sj_diff[1:14*2])])

qj_diff <- (unlist(tapply(df$qj, df$headline, function(x) {abs(x[1]-x)})))
print (qj_diff[1:14*2])
order(qj_diff[1:14*2])
names(qj_diff[1:14*2])[order(qj_diff[1:14*2])]
cbind('#'=1:14, disagreement=qj_diff[1:14*2][order(qj_diff[1:14*2])])
