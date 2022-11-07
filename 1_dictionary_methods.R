####
# This file illustrates dictionary methods and sentiment analysis on FOMC minutes
####

setwd("~/Documents/GitHub/intro-text-analysis-econ")
rm(list = ls())

require(stringr)
require(tidyverse)
require(SentimentAnalysis)
require(ggplot2)


############### Minutes (pre 2014) ############### 
fedminutes_df <- read.csv("data/fedminutes_clean.csv", stringsAsFactors = FALSE)







## Add an LM sentiment measure (loop else vector memory is exhausted)
fedminutes.df$sentiment <- NA
pb = txtProgressBar(min = 1, max = nrow(fedminutes.df), initial = 1) 
for (ii in 1:nrow(fedminutes.df)){
  para <- fedminutes.df$paragraph[ii]
  sentiment <- analyzeSentiment(para,rules=list("SentimentLM"=list(ruleSentiment,loadDictionaryLM())))
  fedminutes.df$sentiment[ii] <- sentiment[1,1]
  setTxtProgressBar(pb,ii)
}

fedminutes.df$sentiment[which(is.na(fedminutes.df$sentiment))] <- 0

sent_df <- aggregate(fedminutes.df[,c("sentiment")], FUN = mean, by = 
                       list(quarter = fedminutes.df$quarter))
sent_df$quarter <- as.Date(sent_df$quarter)
ggplot(sent_df) + theme_bw() + 
  geom_line(aes(x = quarter, y = x))





fedminutes.df <- fedminutes.df[, c("unique_id", "meeting_id", "year", "month", "quarter", "document", 
                                   "pub_date", "meet_date", "source", "paragraph", "sentiment")]


# Write the clean Federal Reserve minutes to a file
clean_filename = paste0(clean_dir, "fedminutes_all.csv")
write.csv(fedminutes.df, file = clean_filename, fileEncoding = "utf-8", row.names = FALSE)



############### End ############### 
