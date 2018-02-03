##### Assignment 5 ######
###### Part 2 ########

install.packages("quanteda")
install.packages("stm")
install.packages("tm")
install.packages("NLP")
install.packages("openNLP")
install.packages("ggplot2")
install.packages("ggdendro")
install.packages("cluster")
install.packages("fpc")

library(quanteda)
library(stm)
library(tm)
library(NLP)
library(openNLP)
library(ggplot2)
library(ggdendro)
library(cluster)
library(fpc) 

mycorpus<- read.csv("C:/ME/NYU/Fall Sem 2016 - I/Business Analytics/Assignments/Assignment 5/Speeches.csv",
                       header = TRUE, stringsAsFactors = FALSE)

dim(mycorpus) 
names(mycorpus)   # names of the headers
head(mycorpus)
str(mycorpus)


#create a corpus with metadata
require(quanteda)
speechcorpus<- corpus(mycorpus$Transcript,
                      docnames=mycorpus$Speech_No)

#explore the corpus
names(speechcorpus)   #to explore the output of the corpus function: "documents" "metadata"  "settings"  "tokens" 
summary(speechcorpus)  #summary of corpus
head(speechcorpus)


#clean corpus: removes punctuation, digits, converts to lower case
help(tokenize)
speechcorpus<- toLower(speechcorpus, keepAcronyms = FALSE) 
speechcleancorpus <- tokenize(speechcorpus, 
                        removeNumbers=TRUE,  
                        removePunct = TRUE,
                        removeSeparators=TRUE,
                        removeTwitter=FALSE,
                        verbose=TRUE)

#explore the clean corpus
head(speechcleancorpus)    # text into token form

#create document feature matrix from clean corpus + stem
help(dfm)
dfm.simple<- dfm(speechcleancorpus,
                 toLower = TRUE, 
                 ignoredFeatures =stopwords("english"), 
                 verbose=TRUE, 
                 stem=FALSE)
head(dfm.simple) #explore output of dfm


#to display most frequent terms in dfm
topfeatures<-topfeatures(dfm.simple, n=100)
topfeatures

help(topfeatures)

#to create a custom dictionary  list of stop words
swlist = c("is", "to", "we", "a", "my", "much","get","even","just")
dfm.stem<- dfm(speechcleancorpus, toLower = TRUE, 
               ignoredFeatures = c(swlist, stopwords("english")),
               verbose=TRUE, 
               stem=TRUE)
topfeatures.stem<-topfeatures(dfm.stem, n=50)
topfeatures.stem

#########################
### WORD CLOUD ########
#########################

install.packages("wordcloud")
library(wordcloud)
set.seed(142)   #keeps cloud' shape fixed
dark2 <- brewer.pal(8, "Set1")   
freq<-topfeatures(dfm.stem, n=500)

help("wordcloud")
wordcloud(names(freq), 
          freq, max.words=100, 
          scale=c(3, .1), 
          colors=brewer.pal(8, "Set1"))


#Sentiment Analysis
help(dfm)
mydict <- dictionary(list(negative = c("illegal*", "islamic*", "failed*","violence", "war"),
                          postive = c("united_states", "country", "new_jobs", "american*", "great")))
dfm.sentiment <- dfm(speechcleancorpus, dictionary = mydict)
topfeatures(dfm.sentiment)
View(dfm.sentiment)


#control parameters here
speechcorpus<- corpus(mycorpus$Transcript,
                      docnames=mycorpus$Speech_No)

#Cleaning corpus
stop_words <- stopwords("SMART")
## additional junk words showing up in the data
stop_words <- c(stop_words, "said", "the", "also", "say", "just", "like","for", "im","life",
                "us", "can", "may", "now", "year", "many*", "on")
stop_words <- tolower(stop_words)


speechcorpus <- gsub("'", "", speechcorpus) # remove apostrophes
speechcorpus <- gsub("[[:punct:]]", " ", speechcorpus)  # replace punctuation with space
speechcorpus <- gsub("[[:cntrl:]]", " ", speechcorpus)  # replace control characters with space
speechcorpus <- gsub("^[[:space:]]+", "", speechcorpus) # remove whitespace at beginning of documents
speechcorpus <- gsub("[[:space:]]+$", "", speechcorpus) # remove whitespace at end of documents
speechcorpus <- gsub("[^a-zA-Z -]", " ", speechcorpus) # allows only letters
speechcorpus <- tolower(speechcorpus)  # force to lowercase

## get rid of blank docs
speechcorpus <- speechcorpus[speechcorpus != ""]

# tokenize on space and output as a list:
doc.list <- strsplit(speechcorpus, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)


# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
term.table <- term.table[names(term.table) != ""]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

#############
# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (1)
W <- length(vocab)  # number of terms in the vocab (1741)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (56196)
term.frequency <- as.integer(term.table) 

# MCMC and model tuning parameters:
K <- 10
G <- 3000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
## display runtime
t2 - t1  

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

speech_for_LDA <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

library(LDAvis)
library(servr)

# create the JSON object to feed the visualization:
json <- createJSON(phi = speech_for_LDA$phi, 
                   theta = speech_for_LDA$theta, 
                   doc.length = speech_for_LDA$doc.length, 
                   vocab = speech_for_LDA$vocab, 
                   term.frequency = speech_for_LDA$term.frequency)

serVis(json, out.dir = 'vis', open.browser = TRUE)
