#4.11
#a) carefully, read, word, statement, notice, 
#   bank, sent, missed, Yoyodyne, Bank, NOT, especially, overseas
#
#b) care, read, word, statement, notic, bank, sent, miss, yoyodyn, bank, not, especi, oversea
#4.12
#a) OPEC, Production, Oil prices, Emergency meeting, Quota, 
#   Market, Demand, Analysts, Excess oil supply, Agreement
#
library("tm")
library("wordcloud")
data("crude")

second_document <- content(crude[[2]])
corpus_b <- Corpus(VectorSource(second_document))
dtm_b <- TermDocumentMatrix(corpus_b)
top_words_b<- head(sort(rowSums(as.matrix(dtm_b)), decreasing = TRUE), 20)
wordcloud(names(top_words_b), top_words_b)

corpus_c <- Corpus(VectorSource(second_document))
corpus_c <- tm_map(corpus_c, removeWords, stopwords("english"))
corpus_c <- tm_map(corpus_c, removePunctuation)
corpus_c <- tm_map(corpus_c, removeNumbers)
dtm_c <- TermDocumentMatrix(corpus_c)
top_words_c <- head(sort(rowSums(as.matrix(dtm_c)), decreasing = TRUE), 20)
wordcloud(names(top_words_c), top_words_c)

corpus_d <- Corpus(VectorSource(crude$content))
dtm_d <- TermDocumentMatrix(corpus_d, control = list(weighting = weightTfIdf))
dtm_combd <- as.matrix(dtm_d)[rownames(as.matrix(dtm_d)) %in% rownames(as.matrix(dtm_b)), 2]
top_words_d <- head(sort(dtm_combd, decreasing = TRUE), 20)
wordcloud(names(top_words_d), top_words_d)

corpus_e <- Corpus(VectorSource(crude$content))
corpus_e <- tm_map(corpus_e, removeWords, stopwords("english"))
corpus_e <- tm_map(corpus_e, removePunctuation)
corpus_e <- tm_map(corpus_e, removeNumbers)
dtm_e <- TermDocumentMatrix(corpus_e, control = list(weighting = weightTfIdf))
dtm_combe <- as.matrix(dtm_e)[rownames(as.matrix(dtm_e)) %in% rownames(as.matrix(dtm_c)), 2]
top_words_e <- head(sort(dtm_combe, decreasing = TRUE), 20)
wordcloud(names(top_words_e), top_words_e)
