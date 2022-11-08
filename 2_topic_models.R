####
# This file illustrates topic models on FOMC minutes
####

setwd("~/Documents/GitHub/intro-text-analysis-econ")
rm(list = ls())


require(doc2vec)
require(tokenizers.bpe)
require(udpipe)

############### Import data ############### 
# GDP growth 
gdp_df <- read.csv("data/GDPC1.csv", stringsAsFactors = FALSE) %>%
  rename(quarter = DATE, gdp = GDPC1) %>%
  mutate(gdp_lag = lag(gdp), 
         growth = 100*(log(gdp) - log(gdp_lag)),
         quarter = as.Date(quarter))
# Minutes
fedminutes_df <- read.csv("data/fedminutes_clean.csv", stringsAsFactors = FALSE) %>%
  mutate(quarter = floor_date(as.Date(date), unit = "quarter")) %>%
  inner_join(gdp_df)


############### Embedding ############### 

embedding_df <- fedminutes_df %>%
  rename(doc_id = unique_id,
         text = paragraph) %>%
  mutate(text = str_squish(gsub("[^[:alpha:]]", " ", tolower(text))))

if (FALSE){
  model <- paragraph2vec(x = embedding_df, type = "PV-DBOW", dim = 100, iter = 20, 
                         min_count = 5, lr = 0.05, threads = 4)
  saveRDS(model, file = "data/doc2vec_model.rds")
} else {
  model <- readRDS("data/doc2vec_model.rds")
}
predict(model, newdata = c("increase", "inflation", "growth"), 
        type = "nearest", which = "word2word", top_n = 5)

# Extract word level embeddings
word_embeddings <- as.matrix(model, which = "words")
rownames(words) <- summary(model, which = "words")
plot(words["decline",], type = "l")
lines(words["increase",], color = "red")


doc_embeddings <- as.matrix(model, which = "docs")
rownames(doc_embeddings)
doc_pca <- data.frame(prcomp(doc_embeddings)$x)
doc_pca$doc_id <- rownames(doc_pca)

embedding_df <- merge(embedding_df, doc_pca, by = "doc_id")
embedding_df$Period <- "pre-2008"
embedding_df$Period[which(embedding_df$quarter<="2008-01-01")] <- "2008-present"
embedding_df$Recession <- as.numeric(embedding_df$growth<=0)

ggplot(embedding_df) + theme_bw() + 
  geom_point(aes(x = PC3, y = PC4, color = Period), alpha = 0.2)

summary(lm(PC4~ Recession, embedding_df))


