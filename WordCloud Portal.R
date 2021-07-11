##-- Set Data ---###
setwd("D:/DATA1")
library(readr)
library(ggplot2)
library(ggpubr)
library(dplyr)
library("scales")

-###--- INSTALL PACKAGE INTUK WORDCLOUD---###
  #install.packages("tidytext")
  #install.packages("textclean")
  #install.packages("wordcloud")
  #install.packages("RColorBrewer")
  #install.packages("dplyr")
  ##install.packages("tm")
  ##install.packages("TSstudio")
  ##install.packages("plyr")
  ##install.packages("mlbench")
  
  #---- WORDCLOUD FOR PORTAL BERITA  ---###
analisis2 <- read.csv("PORTAL POSITIF.csv")
analisis2 <- read.csv(" PORTAL NEGATIF.csv")


## SET TO CORPUS ##
corpus <- Corpus(VectorSource(analisis1$Message))
#--- text cleaning-----#
corpus <- tm_map(corpus,content_transformer(tolower))

corpus <- tm_map(corpus, removeWords,stopwords("english"))
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,stripWhitespace)

#
corpus <- tm_map(corpus,removeWords,c("dan","untuk","dari","selengkapnya","#sahabatdikbud","pada","adalah","sahabatdikbud","yang","dalam","itu","dapat","ini","secara","dengan","tahun,","hanya","oleh","yuk","sudah","tentang","sebagai","merupakan","memiliki","akan","atau","keputusan"))
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(1234)
wordcloud(d$word,d$freq, random.order=FALSE , max.freq = 1,min.freq = 2, rot.per =0.35, scale =c(4,.5),max.words =100, colors = brewer.pal(8,"Dark2"))
title(main = "world cloud Portal Berita",font.main = 2.1, cex.main = 2.0)