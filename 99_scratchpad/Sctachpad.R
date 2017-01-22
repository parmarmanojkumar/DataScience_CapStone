#Author : Manojkumar PArmar
#Date: 23/01/2017
#DataScience Capstone Week1 Quiz.

#load libraries
library(datasets)
library(tm)
library(SnowballC)
library(RWeka)
library(ggplot2)

#cleanup environmant
rm(list = ls())
cat("\014")

# load data
load("./00_Data/All_Read.RData")

#bwsrc1<-"https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
#download.file(bwsrc1, destfile="./00_Data/final/en_US/en_bws.txt")
tm_pre_process <- function(data){
        library(tm)
        
        # Create patterns to elimina special code and other patterns
        # URLs
        urlPat<-function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)
        # Emails
        emlPat<-function(x) gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", x)
        # Twitter tags
        tt<-function(x) gsub("RT |via", "", x)
        # Twitter Usernames
        tun<-function(x) gsub("[@][a - zA - Z0 - 9_]{1,15}", "", x)
        #Remove profane words
        pwdat<-read.table("./00_Data/final/en_US/en_bws.txt", header=FALSE, sep="\n", strip.white=TRUE)
        names(pwdat)<-"Profane Words"
        
        corpusTitle = Corpus(VectorSource(data))
        corpusTitle = tm_map(corpusTitle, tolower)
        corpusTitle = tm_map(corpusTitle, PlainTextDocument)
        corpusTitle = tm_map(corpusTitle, removePunctuation)
        corpusTitle = tm_map(corpusTitle, removeNumbers)
        corpusTitle = tm_map(corpusTitle, urlPat)
        corpusTitle = tm_map(corpusTitle, emlPat)
        corpusTitle = tm_map(corpusTitle, tt)
        corpusTitle = tm_map(corpusTitle, tun)
        corpusTitle = tm_map(corpusTitle, removeWords, pwdat[,1])
        corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
        corpusTitle = tm_map(corpusTitle, stemDocument)
        #corpusTitle = tm_map(corpusTitle, stripWhitespace)
        corpusTitle
}
tokenizer <- function(corpusTitle, tokenCount){
        token <- NGramTokenizer(corpusTitle, 
                                Weka_control(min = tokenCount, 
                                             max = tokenCount,
                                             delimiters = " \\r\\n\\t.,;:\"()?!"
                                             )
                                )
        token
}
gramTopCount <- function(corpusTitle, tokenCount, TopCount){
        token <- tokenizer(corpusTitle,tokenCount)
        gram <- data.frame(table(token))
        gram <- gram[order(gram$Freq, decreasing = T),]
        gram <- gram[1:TopCount,]
        rownames(gram) <- NULL
        colnames(gram) <- c("Word","Frequency")
        g <- ggplot(gram, aes(x=reorder(Word, - Frequency),y=Frequency)) + 
                geom_bar(stat="Identity", fill="orange") +
                geom_text(aes(label=Frequency), hjust=1, angle = 90) + 
                xlab("Word")+
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
        print(g)
        gram
}

corpus = tm_pre_process(c(contentTweet[1:10000], contentBlog[1:10000], contentNews[1:10000]))
unigram <- gramTopCount(corpus, 1, 100)
digram <- gramTopCount(corpus, 2, 25)
trigram <- gramTopCount(corpus, 3, 25)
