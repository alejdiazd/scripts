
### Not run

library(rtweet)
library(tidyverse)
library(tidytext)
library(waffle)
library(tm)
library(stopwords)
library(wordcloud2)
library(syuzhet)
library(stringi)
library(parallel)
library(udpipe)
library(kableExtra)
library(knitr)
library(lubridate)
library(plotly)
library(udpipe)
library(leaflet)
library(ggrepel)
library(sf)

getwd()

uno <- read.csv("twitter_scjn_abr21_may23.csv")

names(uno)

# TWEETS OVER TIME


ts_plot(uno, 
  by="week", color="deepskyblue3") +
  labs(x = "created_at", y = "Count",
       title = "Frequency of SCJN tweets over time", 
       subtitle = "Tweets per weekly frequency") +
   xlab("")+theme_minimal()+ylim(0,150)


ts_plot(uno, 
  by="day", color="deepskyblue3") +
  labs(x = "created_at", y = "Count",
       title = "Frequency of SCJN tweets over time", 
       subtitle = "Tweets per day frequency") +
   xlab("")+theme_minimal()+ylim(0,105)


dim(uno)


ggplot(uno, aes(x=wday(created_at, label = TRUE))) + 
  labs(y = "Count", x = "Day", 
       title = "What is the frequency of SCJN tweets over time?", 
       subtitle = "Tweets per day of the week") +
    geom_bar(aes(fill = ..count..)) +
  theme_minimal()




# MOST USED HASHTAGS

data.frame(text=unlist(uno$hashtags.0)) %>% 
  filter(text != "NA" ) %>%
  count(text, sort = TRUE) %>%
  top_n(20) %>%
  mutate(text = reorder(text, n)) %>%
  ggplot(aes(x = text, y = n)) +
  geom_col(aes(fill = n)) +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
    x = "Hashtags",
    title = "Most frequent hashtags",
    subtitle = "SCJN Twitter account") +
  geom_text(aes(label = n), size = 3, hjust = 1.5, color = "white")+
   theme_minimal()



data.frame(text=unlist(uno$hashtags.0)) %>% 
  filter(text != "null" ) %>%
  count(text, sort = TRUE) %>%
  mutate(text = reorder(text, n)) %>%
  select(word=text, freq=n) %>% 
  wordcloud2()




# MOST USED WORDS

uno %>% 
  filter(lemma != "im", lemma != "w", lemma != "pm") %>%
  count(lemma, sort = TRUE) %>%
  top_n(30) %>%
  mutate(lemma = reorder(lemma, n)) %>%
  ggplot(aes(x = lemma, y = n)) +
  geom_col(aes(fill = n)) +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Words",
       title = "What are the most frequent words in my Twitter account?",
       subtitle = "Top 30 most used words")+
  geom_text(aes(label = n), size = 3, hjust = 1.5, color = "white")


cl = makeCluster(detectCores()-1)

clusterExport(cl = cl, c("get_sentiment", "get_sent_values",
     "get_nrc_sentiment", 
    "get_nrc_values", "parLapply"))

# SENTIMENT ANALYSIS

tweetSentimentNRC = get_nrc_sentiment(uno$full_text,
                 language = "spanish", cl=cl)

stopCluster(cl) 


# SENTIMENTS LABELING

tweetSentimentNRC = cbind(uno, tweetSentimentNRC)

tweetSentimentNRC %>% 
  filter(rowSums(tweetSentimentNRC[,-c(1,2)]) > 0) %>% 
  head()




# SENTIMENTS FREQUENCY

sentimentScores = data.frame(colSums(tweetSentimentNRC %>% 
                                       filter(lemma!="general") %>% 
                                       select(-token,-lemma)))
names(sentimentScores) = "Score"
sentimentScores = cbind("sentiment"= rownames(sentimentScores), sentimentScores)
sentimentScores = sentimentScoresggplot(data=sentimentScores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  xlab("Sentimients")+ylab("Scores")+
  ggtitle("What are the sentiments on my Twitter account?", 
          "Sentiments based on Score")+
  theme(axis.text.x = element_text(angle=45),
        legend.position = "none")




########################

# topic model for uno$full_text

head(uno$full_text)


library(tm)
library(topicmodels)
library(RTextTools)
library(wordcloud)
library(pdftools) 

library(readtext)
library(stringr)
library(readr)

library(tidytext)
library(ggplot2)
library(dplyr)

library(text2vec)

library(quanteda)

library(LDAvis)


scjn <- corpus(uno$full_text)

doc.corpus <- scjn
summary(doc.corpus)

doc.tokens <- tokens(doc.corpus)

doc.tokens <- tokens(doc.tokens, remove_punct = TRUE, 
                     remove_numbers = TRUE)


doc.tokens <- tokens_select(doc.tokens, 
  stopwords('spanish'),selection='remove')

doc.tokens <- tokens_tolower(doc.tokens)


# Reemplazar todos los caracteres que no 
# sean letras o números con espacios en blanco

doc.tokens <- gsub("[^[:alnum:]]", " ", doc.tokens)


## pero si solo se quiere eliminar la puntuación del texto

doc.tokens <- gsub("[[:punct:]]", "", doc.tokens )


# convertir caracteres UTF-8 en caracteres ASCII y 
# eliminar cualquier caracter no convertible

#doc.tokens <- iconv(doc.tokens, "UTF-8", "ASCII", sub="")



##############

doc.dfm.final <- dfm(doc.tokens)

doc_dtm = as(doc.dfm.final, 'CsparseMatrix')



lda_model2 = LDA$new(n_topics = 10, 
        doc_topic_prior = 0.1, 
        topic_word_prior = 0.01)



# fit the model
doc_topic_distr2= lda_model2$fit_transform(x = doc_dtm, 
                  n_iter = 1000,
                convergence_tol = 0.0001, 
                n_check_convergence = 25,
                progressbar = T)


lda_model2$get_top_words(n = 10, topic_number = 1:10, lambda = 1)

  
lda_model2$plot()

topfeatures(doc.dfm.final, 5)

topfeatures(doc.dfm.final, 20)

View(kwic(doc.tokens, "presidente", window = 3))

head(kwic(doc.tokens, "presidente", window = 3))




########################


inspect(scjn)


######################


scjn <- tm_map(scjn, stripWhitespace)
scjn <- tm_map(scjn, removePunctuation)
scjn <- tm_map(scjn, tolower)

scjn <- tm_map(scjn, removeWords, stopwords("spanish"))

wordcloud(scjn, max.words=40, scale=c(5,0.5),
   colors=brewer.pal(8,"Dark2"))




library(topicmodels)


ap_topics <- tidy(ap_lda, matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()




ap_topics <- tidy(ap_lda, matrix = "gamma")


# convert to the sparse matrix representation using Matrix package
shakes_dtm = as(shakes_dtm, 'CsparseMatrix')

# setup the model
lda_model = LDA$new(n_topics = 10,
       doc_topic_prior = 0.1, 
       topic_word_prior = 0.01)


# fit the model
doc_topic_distr= lda_model$fit_transform(x = shakes_dtm, 
                  n_iter = 1000,
                convergence_tol = 0.0001, 
                n_check_convergence = 25,
                progressbar = FALSE)


lda_model$get_top_words(n = 10,
       topic_number = 1:10,
       lambda = 1)


# top-words could be sorted by “relevance” which also takes into account
# frequency of word in the corpus (0 < lambda < 1)
lda_model$get_top_words(n = 10,
       topic_number = 1:10, 
       lambda = 0.2)




lda_model$plot()



#################

library(stringr)
library(readr)


va <- pdftools::pdf_text("manuscript.pdf")

va_txt <- stringr::str_c(va, collapse="")

readr::write_lines(va_txt, "va.txt")





















