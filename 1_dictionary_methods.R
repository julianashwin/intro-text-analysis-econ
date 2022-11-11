####
# This file illustrates dictionary methods and sentiment analysis on FOMC minutes
####

setwd("~/Documents/GitHub/intro-text-analysis-econ")
rm(list = ls())

require(stringr)
require(tidyverse)
require(tidytext)
require(ggplot2)
require(ggwordcloud)
require(lubridate)

std <- function(x){
  x <- (x - mean(x, na.rm = T))/sd(x, na.rm = T)
  return(x)
}

############### Examples of dictionaries ############### 
affin <- get_sentiments("afinn")
loughran <- get_sentiments("loughran")


############### Import data ############### 
# Minutes
fedminutes_df <- read.csv("data/fedminutes_all.csv", stringsAsFactors = FALSE)
# GDP growth 
gdp_df <- read.csv("data/GDPC1.csv", stringsAsFactors = FALSE) %>%
  rename(quarter = DATE, gdp = GDPC1) %>%
  mutate(gdp_lag = lag(gdp), 
         growth = 100*(log(gdp) - log(gdp_lag)),
         quarter = as.Date(quarter))


# Separate out words
fedminutes_words <- fedminutes_df %>%
  mutate(paragraph = str_remove_all(paragraph, "[0-9]+")) %>%
  select(date, paragraph) %>%
  unnest_tokens(word, paragraph) %>%
  group_by(date) %>% count(word, sort = F) %>% ungroup() 
fedminutes_words

# Identify words from list
fedminutes_sentiment <- fedminutes_words %>%
  inner_join(get_sentiments("loughran"))

# Look at the most relevant words in each category
fedminutes_total <- fedminutes_sentiment %>%
  group_by(word, sentiment) %>%
  summarise(n = sum(n)) %>%
  filter(n > 25)
fedminutes_total
# Plot frequent words
ggplot(fedminutes_total, aes(label = word, size = n)) +
  facet_wrap(.~sentiment) +
  geom_text_wordcloud() +
  theme_minimal()

# Aggregate to meeting level 
fedminutes_agg <- fedminutes_sentiment %>%
  pivot_wider(id_cols = c(date, word), names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(quarter = floor_date(as.Date(date), unit = "quarter")) %>%
  group_by(quarter) %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(polarity = (positive - negative)/(1+positive+negative),
         quarter = as.Date(quarter)) %>%
  inner_join(gdp_df)
  
# Captures the GFC well, but not the Covid shock - not a conventional economic crisis!
ggplot(filter(fedminutes_agg,quarter >= "2000-01-01")) + theme_bw() + 
  geom_line(aes(x = quarter, y = growth, color = "GDP")) +
  geom_line(aes(x = quarter, y = std(polarity), color = "loughran")) 




############### Sentiment analysis packages ############### 
# Aggregate the minutes to quarterly level 
fedminutes_qly <- fedminutes_df %>%
  mutate(quarter = floor_date(as.Date(date), unit = "quarter")) %>%
  select(quarter, paragraph) %>%
  group_by(quarter) %>%
  summarise(text = paste(paragraph, collapse = "\n"))
  
# Vader 
require(vader)

text <- "Julian is a nice person"
get_vader(text)
text <- "Julian is a very nice person"
get_vader(text)
text <- "Julian is not a very nice person"
get_vader(text)

fedminutes_qly$vader <- NA
pb = txtProgressBar(min = 1, max = nrow(fedminutes_qly), initial = 1) 
for (ii in 1:nrow(fedminutes_qly)){
  fedminutes_qly$vader[ii] <- get_vader(fedminutes_qly$text[ii])[2]
  setTxtProgressBar(pb,ii)
}

# SentimentAnalysis package
require(SentimentAnalysis)

sentiment_df <- analyzeSentiment(fedminutes_df$paragraph)







############### End ############### 
