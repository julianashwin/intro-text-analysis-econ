####
# This file illustrates topic models on FOMC minutes
####

setwd("~/Documents/GitHub/intro-text-analysis-econ")
rm(list = ls())


require(ggplot2)
require(forcats)
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
fedminutes_df <- read.csv("data/fedminutes_all.csv", stringsAsFactors = FALSE) %>%
  mutate(quarter = floor_date(as.Date(date), unit = "quarter")) %>%
  inner_join(gdp_df) %>%
  filter(quarter > "2000-01-01")


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
rownames(word_embeddings) <- summary(model, which = "words")
plot(word_embeddings["decline",], type = "l")
lines(word_embeddings["increase",], col = "red")
lines(word_embeddings["canada",], col = "blue")

cor(word_embeddings["decline",], word_embeddings["increase",])
cor(word_embeddings["decline",], word_embeddings["canada",])

doc_embeddings <- as.matrix(model, which = "docs")
rownames(doc_embeddings)
doc_pca <- data.frame(prcomp(doc_embeddings)$x)
doc_pca$doc_id <- rownames(doc_pca)

embedding_df <- merge(embedding_df, doc_pca, by = "doc_id")
embedding_df$Period <- "pre-2008"
embedding_df$Period[which(embedding_df$quarter>="2008-01-01")] <- "2008-present"
embedding_df$Recession <- as.numeric(embedding_df$growth<=0)

ggplot(embedding_df) + theme_bw() + 
  geom_point(aes(x = PC1 , y = PC2, color = Period), alpha = 0.2)

summary(lm(PC4 ~ Recession, embedding_df))



############### Clean for topic model ############### 

require(tm)

fedminutes_df$paragraph[2000]
# All lower case
fedminutes_df$paragraph_clean <- tolower(fedminutes_df$paragraph)
fedminutes_df$paragraph_clean[2000]
# Remove all non alphabet
fedminutes_df$paragraph_clean <- gsub("[^[:alpha:]]", " ", fedminutes_df$paragraph_clean)
fedminutes_df$paragraph_clean[2000]
# Remove stopwords
fedminutes_df$paragraph_clean <- sapply(tm_map(Corpus(VectorSource(unlist(fedminutes_df$paragraph_clean))), 
                                   removeWords, stopwords("english")), as.character)
fedminutes_df$paragraph_clean[2000]
# Stem words
fedminutes_df$paragraph_clean <- sapply(tm_map(Corpus(VectorSource(unlist(fedminutes_df$paragraph_clean))), 
               stemDocument), as.character)
fedminutes_df$paragraph_clean[2000]
# Squish whitespace
fedminutes_df$paragraph_clean <- str_squish(fedminutes_df$paragraph_clean)
fedminutes_df$paragraph_clean[2000]

# Remove any paragraphs that are now empty
fedminutes_df <- filter(fedminutes_df, nchar(paragraph_clean) > 2)






############### Estimate topic model ############### 

require(topicmodels)

# Create DTM
corpus <- Corpus(VectorSource(unlist(fedminutes_df$paragraph_clean)))
mins_dtm <- DocumentTermMatrix(corpus, control = list(minsWordLength = 3))
# Word count in each doc
fedminutes_df$wordcount <- rowSums(as.matrix(mins_dtm))
ggplot(fedminutes_df, aes(x = wordcount)) + geom_histogram()
# Look at term frequencies
mins_vocab <- mins_dtm$dimnames$Terms
vocab_df <- data.frame(term = mins_vocab, tf = colSums(as.matrix(mins_dtm)))
# List of words to remove
remove_words <- vocab_df$term[which(vocab_df$tf < 5)]
# Remove words
fedminutes_df$paragraph_clean <- sapply(tm_map(Corpus(VectorSource(unlist(fedminutes_df$paragraph_clean))), 
                                               removeWords, remove_words), as.character)
fedminutes_df$paragraph_clean[2000]

# Export the clean paragraph data
write.csv(fedminutes_df, "data/fedminutes_clean.csv", row.names = FALSE)


# Convert to DocumentTermMatrix again with the smaller vocab
corpus <- Corpus(VectorSource(unlist(fedminutes_df$paragraph_clean)))
mins_dtm <- DocumentTermMatrix(corpus, control = list(minsWordLength = 3))
mins_dtm$dimnames$Docs <- fedminutes_df$unique_id
mins_dtm

# Estimate LDA by Gibbs sampling 
set.seed(1234)
lda_gibbs <- LDA(mins_dtm, k = 20, method = "Gibbs",
                 control = list(verbose = 1000, burnin = 1000, thin = 10, iter = 5000))
if (FALSE){
  saveRDS(lda_gibbs, file = "data/mins_lda_20.rds")
} else {
  lda_gibbs <- readRDS(file = "data/mins_lda_20.rds")
}




############### Topic-vocabulary distribution ############### 
word_topic_dist <- tidy(lda_gibbs, matrix = "beta")
word_topic_dist

top_terms <- word_topic_dist %>%
  group_by(topic) %>%
  top_n(10,beta) %>%
  ungroup() %>%
  arrange(topic, -beta) 
top_terms$term[duplicated(top_terms$term)] <- paste0(top_terms$term[duplicated(top_terms$term)], " ")
top_terms$term[duplicated(top_terms$term)] <- paste0(top_terms$term[duplicated(top_terms$term)], " ")
top_terms$term[duplicated(top_terms$term)] <- paste0(top_terms$term[duplicated(top_terms$term)], " ")
top_terms$term[duplicated(top_terms$term)] <- paste0(top_terms$term[duplicated(top_terms$term)], " ")
top_terms$term <- factor(top_terms$term, ordered = TRUE, 
                         levels = rev(unique(top_terms$term)))

# Plot these top ten terms for each topic
ggplot(top_terms) + theme_bw() + 
  facet_wrap(~ topic, scales = "free") +
  geom_bar(aes(x = term, y = beta, fill = factor(topic)), 
           stat = "identity", show.legend = FALSE) +
  coord_flip()


############### Topic-document distribution ############### 
doc_topic_dist <- tidy(lda_gibbs, matrix = "gamma") %>%
  rename(unique_id = document) %>%
  inner_join(select(fedminutes_df, unique_id, quarter, growth)) %>%
  filter(quarter >= "2000-01-01") %>%
  group_by(topic, quarter) %>%
  summarise(gamma = mean(gamma), growth = mean(growth))
  
ggplot(doc_topic_dist) + theme_bw() + facet_wrap(~ topic, scales = "free") + 
  geom_line(aes(x = quarter, y = gamma))



############### Some examples ############### 

topic11_words <- filter(word_topic_dist, topic == 11) %>%
  arrange(topic, -beta) %>%
  top_n(30,beta) %>%
  mutate(beta = beta/sum(beta))

ggplot(topic11_words, aes(label = term, size = beta)) +
  geom_text_wordcloud() +
  theme_minimal()

topic11_docs <- filter(doc_topic_dist, topic == 11)

ggplot(topic11_docs, aes(x = quarter, y = gamma)) +
  geom_line() + theme_minimal()












