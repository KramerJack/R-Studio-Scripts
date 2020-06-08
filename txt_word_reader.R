#text mining example tutorial

#librarys needed
install.packages("tm")
library(tm)
install.packages("pdftools")
library(pdftools)
install.packages("wordcloud")
library(wordcloud)
install.packages("tabulizer")
library(tabulizer)
library(dplyr)


#pdf location
file_location <- "http://files.libertyfund.org/files/788/0084_LFeBk.pdf"

file_location


#load pdf file
txt <- pdf_text(file_location)

cat(txt[2])

#create corpus
txt_corpus <- Corpus(VectorSource(txt))

#clear corpus
txt_corpus <- tm_map(txt_corpus, tolower)
txt_corpus <- tm_map(txt_corpus, removePunctuation)
txt_corpus <- tm_map(txt_corpus, stripWhitespace)
#remove stop
head(stopwords('en'))
txt_corpus = tm_map(txt_corpus, removeWords, stopwords("en"))

#view content of corpus
txt_corpus$content

#create document term matrix
dtm <- DocumentTermMatrix(txt_corpus)
dtm <- as.matrix(dtm)
dtm <- t(dtm)


#sum number of occurance of each of the words
number_occurances <- rowSums(dtm)
number_occurances <- sort(number_occurances, decreasing = TRUE)

#plot wordCloud
wordcloud(head(names(number_occurances), 30), head(number_occurances, 30), scale=c(2,1))

#Interminal comands
View(dtm)
head(number_occurances)















