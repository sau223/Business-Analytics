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

precorpus<- read.csv("C:/ME/NYU/Fall Sem 2016 - I/Business Analytics/Assignments/Assignment 5/Forbes.csv", 
                     header = TRUE, stringsAsFactors = FALSE)
dim(precorpus) 
names(precorpus)   # names of the headers
head(precorpus)
str(precorpus)

### Mission Statements Analysis ######

#create a corpus with metadata
require(quanteda)
help(corpus)
missionstatementcorpus<- corpus(precorpus$Mission_Statements,
                    docnames=precorpus$Company_ID)

#explore the corpus
names(missionstatementcorpus)   #to explore the output of the corpus function: "documents" "metadata"  "settings"  "tokens" 
summary(missionstatementcorpus)  #summary of corpus
head(missionstatementcorpus)

#clean corpus: removes punctuation, digits, converts to lower case
help(tokenize)
missionstatementcorpus<- toLower(missionstatementcorpus, keepAcronyms = FALSE) 
missioncleancorpus <- tokenize(missionstatementcorpus, 
                        removeNumbers=TRUE,  
                        removePunct = TRUE,
                        removeSeparators=TRUE,
                        removeTwitter=FALSE,
                        verbose=TRUE)

#explore the clean corpus
head(missioncleancorpus)    # text into token form

#create document feature matrix from clean corpus + stem
help(dfm)
dfm.simple<- dfm(missioncleancorpus,
                 toLower = TRUE, 
                 ignoredFeatures =stopwords("english"), 
                 verbose=TRUE, 
                 stem=FALSE)
head(dfm.simple) #explore output of dfm


#to display most frequent terms in dfm
topfeatures<-topfeatures(dfm.simple, n=30)
topfeatures

#to create a custom dictionary  list of stop words
swlist = c("will", "use", "can")
dfm.stem<- dfm(missioncleancorpus, toLower = TRUE, 
               ignoredFeatures = c(swlist, stopwords("english")),
               verbose=TRUE, 
               stem=TRUE)
topfeatures.stem<-topfeatures(dfm.stem, n=20)
topfeatures.stem

#exploration in context
kwic(missioncleancorpus, "data", 2)
kwic(missioncleancorpus , "customer", 2)

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
          freq, max.words=200, 
          scale=c(3, .1), 
          colors=brewer.pal(8, "Set1"))


#specifying a correlation limit of 0.5   
dfm.tm<-convert(dfm.stem, to="tm")
findAssocs(dfm.tm, 
           c("data", "peopl", "success"), 
           corlimit=0.7)

### Core Values Analysis ######

#create a corpus with metadata
require(quanteda)
help(corpus)
corevaluescorpus<- corpus(precorpus$Core_Values,
                          docnames=precorpus$Company_ID)

#explore the corpus
names(corevaluescorpus)   #to explore the output of the corpus function: "documents" "metadata"  "settings"  "tokens" 
summary(corevaluescorpus)  #summary of corpus
head(corevaluescorpus)

#clean corpus: removes punctuation, digits, converts to lower case
help(tokenize)
corevaluescorpus<- toLower(corevaluescorpus, keepAcronyms = FALSE) 
corecleancorpus <- tokenize(corevaluescorpus, 
                        removeNumbers=TRUE,  
                        removePunct = TRUE,
                        removeSeparators=TRUE,
                        removeTwitter=FALSE,
                        verbose=TRUE)

#explore the clean corpus
head(corecleancorpus)    # text into token form

#create document feature matrix from clean corpus + stem
help(dfm)
dfm.simple<- dfm(corecleancorpus,
                 toLower = TRUE, 
                 ignoredFeatures =stopwords("english"), 
                 verbose=TRUE, 
                 stem=FALSE)
head(dfm.simple) #explore output of dfm


#to display most frequent terms in dfm
topfeatures<-topfeatures(dfm.simple, n=30)
topfeatures

#to create a custom dictionary  list of stop words
swlist = c("s", "t", "saw", "don", "re")
dfm.stem<- dfm(corecleancorpus, toLower = TRUE, 
               ignoredFeatures = c(swlist, stopwords("english")),
               verbose=TRUE, 
               stem=TRUE)
topfeatures.stem<-topfeatures(dfm.stem, n=50)
topfeatures.stem

#exploration in context
kwic(corecleancorpus, "business", 2)
kwic(corecleancorpus , "customer", 2)

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
          freq, max.words=200, 
          scale=c(3, .1), 
          colors=brewer.pal(8, "Set1"))


#specifying a correlation limit of 0.5  
dfm.tm<-convert(dfm.stem, to="tm")
findAssocs(dfm.tm, 
           c("compani", "custom", "busi"), 
           corlimit=0.5)
