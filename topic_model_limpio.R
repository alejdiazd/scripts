
### Not run

### LDA 

library(tidytext)
library(tidyverse)
library(ggplot2)

library(SnowballC)
library(tm)
library(topicmodels)
library(RTextTools) 
library(RColorBrewer)
library(wordcloud)


# Podemos utilizar las funciones de la libreria "tm" 
# para limpiar y preparar los datos

corpus <- Corpus(DirSource("C:/textitos", encoding = "UTF-8")) # los textos


corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
#corpus <- tm_map(corpus, stemDocument)  ## no usar, corta hasta raiz



library(textclean)

# function from textclean to remove curly quotes 
corpus <- tm_map(corpus, replace_curly_quote)
#corpus <- tm_map(corpus, replace_non_ascii)

# quitar palabras innecesarias
corpus<- tm_map(corpus, removeWords, 
 c("sólo","cada","puede","así","hace","toda","martínez","febrero","sino",
   "enero","debe","diócesis","arquidiócesis","mons","san","podemos","año",
   "txt","b","xvi","cfr","juan","martes","parte","pueden","tan","domingo",
   "antequeraoaxaca","años","importante","jueves","manera","mayo","felipe",
   "arzobispo","obispo","obispos","mismo","diciembre","lunes","además",
   "muchas","pues","hacia","gran","través","modo","dice","arce","dos","dijo",
   "especialmente","guadalajara","aguilar","rodrigo","esquivel","lópez",
   "rogelio","viernes","acapulco","embargo","sánchez","misma","vez","solo",
   "miércoles","carlos","quintero","arizmendi","cabrera","n"))


wordcloud(corpus, scale=c(5,0.5), max.words=40, 
  random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, 
  colors=brewer.pal(8, "Dark2"))



### como si fuera DTM

inspect(DocumentTermMatrix(corpus,
    list(dictionary = c("iglesia", "dios", "vida",
    "obispo","cristo", "pobres", "familia","jesucristo"))))



findFreqTerms(DocumentTermMatrix(corpus), 30)


library(tidyverse)
library(tokenizers)


## preprocess...

text<- paste(corpus)
words <- tokenize_words(text)

words

length(words)

tab <- table(words[[1]])
tab <- data_frame(word = names(tab), count = as.numeric(tab))
tab


sentences <- tokenize_sentences(text)
sentences

stopwords("spa")

library(qdap)

### grafica de barras 
plot(freq_terms(text, 20))
 
# plot(discourse_map(corpus, 30))

install.packages("lda")
library(lda)
install.packages("ldatuning")
library(ldatuning)

minimumFrequency <- 5
DTM <- DocumentTermMatrix(corpus, 
     control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTM)

sel_idx <- slam::row_sums(DTM) > 0
DTM <- DTM[sel_idx, ]
corpus <- corpus[sel_idx, ]


## fitting topics and metrics

result <- ldatuning::FindTopicsNumber(
  DTM,
  topics = seq(from = 10, to = 20, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)


## metrics and topics

FindTopicsNumber_plot(result)


# number of topics
K <- 10
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel <- LDA(DTM, K, method="Gibbs", 
                  control=list(iter = 500, verbose = 25))


# have a look a some of the results (posterior distributions)
tmResult <- posterior(topicModel)
# format of the resulting object
attributes(tmResult)

nTerms(DTM)


beta <- tmResult$terms   # get beta from results
dim(beta) 

rowSums(beta) 

nDocs(DTM)               # size of collection


theta <- tmResult$topics 
dim(theta)  

terms(topicModel, 10)  ### palabras dentro de cada gran tema




library(tidytext)

# Podemos utilizar las funciones de la libreria "topicmodels" 
# para crear un modelo de temas 
#### tiene que ser dtm (DocumentTermMatrix)

## topic model with 2 topics

ap_lda <- LDA(DTM, k = 2, control = list(seed = 1234))
ap_lda      


## The per-topic-per-word probabilities, 
## called betas 
## from the model

ap_topics <- tidy(ap_lda, matrix = "beta")  

ap_topics   

ggplot(ap_topics, aes(x=term, y=beta))+
   geom_point()



### two topics graph

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


# The terms that had the greatest difference in between topic 1 and topic 2.
## This can be estimated based on the log ratio of the two
# a log ratio is useful because it makes the difference symmetrical 


library(tidyr)

beta_wide <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_wide

ggplot(beta_wide, aes(y=term,x=log_ratio))+
  geom_col(show.legend = FALSE)  



### Each of these values is an estimated proportion of words 
### from that document that are generated from that topic

## per-document-per-topic probabilities, 
## called   gamma  
## with the matrix = "gamma" argument

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents   # proportions by topic from each document

ggplot(ap_documents, aes(x=document, y=gamma))+
   geom_point()+facet_wrap(.~topic)+coord_flip()

write.csv(ap_documents, file="ap_documents.csv") ### para guardar

hist(ap_documents$gamma)
summary(ap_documents$gamma)


DTM

### Semantic Coherence & Exclusivity

#K <- c(2,6)
#fit <- searchK(DTM, K = K, verbose = TRUE)



topics(ap_lda)  

## each word by topic (gamma, beta )

posterior(ap_lda)  

names(posterior(ap_lda))



#######################################

library(LDAvis)
library(text2vec)


## LDA visualization desde cero

library(tm)

corpus <- gsub("'", "", corpus)  # remove apostrophes
corpus <- gsub("[[:punct:]]", " ", corpus)  # replace punctuation with space
corpus <- gsub("[[:cntrl:]]", " ", corpus)  # replace control characters with space
corpus <- gsub("^[[:space:]]+", "", corpus) # remove whitespace at beginning of documents
corpus <- gsub("[[:space:]]+$", "", corpus) # remove whitespace at end of documents
corpus <- tolower(corpus)  # force to lowercase

###

# tokenize on space and output as a list:
doc.list <- strsplit(corpus, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}

documents <- lapply(doc.list, get.terms)


###

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents  
W <- length(vocab)  # number of terms in the vocab  
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document 
N <- sum(doc.length)  # total number of tokens in the data 
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus


####

# MCMC and model tuning parameters:
K <- 10         ## topics
G <- 1000       ## Gibbs sampler iterations
alpha <- 0.02   ## document-topic distributions
eta <- 0.02     ## topic-term distributions


# Fit the model:
library(lda)
set.seed(357)


fit <- lda.collapsed.gibbs.sampler(documents = documents, 
                 K = K, vocab = vocab, 
              num.iterations = G, alpha = alpha, 
              eta = eta, initial = NULL, burnin = 0,
              compute.log.likelihood = TRUE)

###

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))


###


b_messages <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

library(LDAvis)
install.packages('servr')


# create the JSON object to feed the visualization:
json <- createJSON(phi = b_messages$phi, 
                   theta = b_messages$theta, 
                   doc.length = b_messages$doc.length, 
                   vocab = b_messages$vocab, 
                   term.frequency = b_messages$term.frequency)


serVis(json, out.dir = 'vis', open.browser = T)  ## abre internet con la viz

### End