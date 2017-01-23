#Author : Manojkumar PArmar
#Date: 23/01/2017
#DataScience Capstone Week1 Quiz.

#load libraries
library(datasets)
library(tm)
library(SnowballC)
library(RWeka)
library(ggplot2)
library(gridExtra)
library(wordcloud)

#cleanup environmant
rm(list = ls())
cat("\014")

# load data
#load("./00_Data/All_Read.RData")

#bwsrc1<-"https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
#download.file(bwsrc1, destfile="./00_Data/final/en_US/en_bws.txt")
tm_pre_process <- function(data) {
        library(tm)
        
        # Create patterns to elimina special code and other patterns
        # URLs
        urlPat <- function(x)
                gsub("(ftp|http)(s?)://.*\\b", "", x)
        # Emails
        emlPat <-
                function(x)
                        gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", x)
        # Twitter tags
        tt <- function(x)
                gsub("RT |via", "", x)
        # Twitter Usernames
        tun <- function(x)
                gsub("[@][a - zA - Z0 - 9_]{1,15}", "", x)
        #Remove profane words
        pwdat <-
                read.table(
                        "./00_Data/final/profane_words/en_bws.txt",
                        header = FALSE,
                        sep = "\n",
                        strip.white = TRUE
                )
        names(pwdat) <- "Profane Words"
        pwdat1 <-
                read.csv("./00_Data/final/profane_words/Terms-to-Block.csv")
        pwdat1 <- pwdat1[-(1:3), 2]
        pwdat1 <- gsub(",", "", pwdat1)
        stpWrdList <- c(as.character(pwdat[, 1]),
                        pwdat1,
                        stopwords("english"))
        
        corpusTitle = Corpus(VectorSource(data))
        corpusTitle = tm_map(corpusTitle, tolower)
        corpusTitle = tm_map(corpusTitle, PlainTextDocument)
        corpusTitle = tm_map(corpusTitle, removePunctuation)
        corpusTitle = tm_map(corpusTitle, removeNumbers)
        corpusTitle = tm_map(corpusTitle, urlPat)
        corpusTitle = tm_map(corpusTitle, emlPat)
        corpusTitle = tm_map(corpusTitle, tt)
        corpusTitle = tm_map(corpusTitle, tun)
        corpusTitle = tm_map(corpusTitle, removeWords, stpWrdList)
        corpusTitle = tm_map(corpusTitle, stemDocument)
        corpusTitle = tm_map(corpusTitle, stripWhitespace)
        corpusTitle
}
tokenizer <- function(corpusTitle, tokenCount) {
        token <- NGramTokenizer(
                corpusTitle,
                Weka_control(
                        min = tokenCount,
                        max = tokenCount,
                        delimiters = " \\r\\n\\t.,;:\"()?!"
                )
        )
        token
}

gramPlot <- function(gram) {
        g <-
                ggplot(gram, aes(x = reorder(Word, -Frequency), y = Frequency)) +
                geom_bar(stat = "Identity", fill = "orange") +
                geom_text(aes(label = Frequency),
                          hjust = 1,
                          angle = 90) +
                xlab("Word") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
        g
        
}

gramPlotQuad <-
        function(tweetgram,
                 bloggram,
                 newsgram,
                 partgram,
                 title = "NoGram",
                 TopCount = 10) {
                gtweet <-
                        ggplot(head(tweetgram, TopCount),
                               aes(x = reorder(Word, -Frequency), y = Frequency)) +
                        geom_bar(stat = "Identity", fill = "#1dcaff") +
                        geom_text(aes(label = Frequency),
                                  hjust = 1,
                                  angle = 90) +
                        xlab("Word") +
                        ggtitle(paste("Tweets", title)) +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1))
                gblog <-
                        ggplot(head(bloggram, TopCount),
                               aes(x = reorder(Word, -Frequency), y = Frequency)) +
                        geom_bar(stat = "Identity", fill = "orange") +
                        geom_text(aes(label = Frequency),
                                  hjust = 1,
                                  angle = 90) +
                        xlab("Word") +
                        ggtitle(paste("Blogs", title)) +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1))
                gnews <-
                        ggplot(head(newsgram, TopCount),
                               aes(x = reorder(Word, -Frequency), y = Frequency)) +
                        geom_bar(stat = "Identity", fill = "#87F717") +
                        geom_text(aes(label = Frequency),
                                  hjust = 1,
                                  angle = 90) +
                        xlab("Word") +
                        ggtitle(paste("News", title)) +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1))
                gall <-
                        ggplot(head(partgram, TopCount),
                               aes(x = reorder(Word, -Frequency), y = Frequency)) +
                        geom_bar(stat = "Identity", fill = "#F6358A") +
                        geom_text(aes(label = Frequency),
                                  hjust = 1,
                                  angle = 90) +
                        xlab("Word") +
                        ggtitle(paste("All Source", title)) +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1))
                grid.arrange(gtweet,
                             gblog,
                             gnews,
                             gall,
                             ncol = 2,
                             nrow = 2)
                
        }

gramTopCount <- function(corpusTitle, tokenCount, TopCount = 0) {
        token <- tokenizer(corpusTitle, tokenCount)
        gram <- data.frame(table(token))
        gram <- gram[order(gram$Freq, decreasing = T),]
        rownames(gram) <- NULL
        colnames(gram) <- c("Word", "Frequency")
        if (TopCount > 0) {
                gram <- gram[1:TopCount,]
                print(gramPlot(gram))
        }
        gram
}

# set.seed(1)
# contentTweet <- sample(contentTweet, length(contentTweet)* 0.01)
# contentBlog <- sample(contentBlog, length(contentBlog) * 0.015)
# contentNews <- sample(contentNews, length(contentNews) * 0.015)
# contentPart <-c(contentTweet, contentBlog, contentNews)
# load("./00_Data/Reduced_Data.RData")

#for milestone report use this distribution
# for final model build from each 50000 or 100000 data.
# corpusTweet <- tm_pre_process(contentTweet)
# corpusNews <- tm_pre_process(contentNews)
# corpusBlog <- tm_pre_process(contentBlog)
# corpusPart <- tm_pre_process(contentPart)
# starttime <- Sys.time()
# load("./00_Data/processed_data.RData")
# loadtime <- Sys.time()

# blog.unigram <- gramTopCount(corpusBlog, 1)
# blog.digram <- gramTopCount(corpusBlog, 2)
# blog.trigram <- gramTopCount(corpusBlog, 3)
# blog.quadgram <- gramTopCount(corpusBlog, 4)
# blogtime <- Sys.time()
#
# news.unigram <- gramTopCount(corpusNews, 1)
# news.digram <- gramTopCount(corpusNews, 2)
# news.trigram <- gramTopCount(corpusNews, 3)
# news.quadgram <- gramTopCount(corpusNews, 4)
# newstime <- Sys.time()
#
# tweet.unigram <- gramTopCount(corpusTweet, 1)
# tweet.digram <- gramTopCount(corpusTweet, 2)
# tweet.trigram <- gramTopCount(corpusTweet, 3)
# tweet.quadgram <- gramTopCount(corpusTweet, 4)
# tweettime <- Sys.time()
#
# part.unigram <- gramTopCount(corpusPart, 1)
# part.digram <- gramTopCount(corpusPart, 2)
# part.trigram <- gramTopCount(corpusPart, 3)
# part.quadgram <- gramTopCount(corpusPart, 4)
# parttime <- Sys.time()

##ploting
g1 <- gramPlotQuad(
        tweetgram = tweet.unigram,
        bloggram = blog.unigram,
        newsgram = news.unigram,
        partgram = part.unigram,
        title = "UniGram"
)
g2 <- gramPlotQuad(
        tweetgram = tweet.digram,
        bloggram = blog.digram,
        newsgram = news.digram,
        partgram = part.digram,
        title = "BiGram"
)
g3 <- gramPlotQuad(
        tweetgram = tweet.trigram,
        bloggram = blog.trigram,
        newsgram = news.trigram,
        partgram = part.trigram,
        title = "TriGram"
)
g4 <- gramPlotQuad(
        tweetgram = tweet.quadgram,
        bloggram = blog.quadgram,
        newsgram = news.quadgram,
        partgram = part.quadgram,
        title = "QuadGram"
)

#other Plots

cloudPlotDuo <- function(unigram, digram, Type = "None") {
        par(mfrow = c(1, 2))
        wordcloud(
                as.character(unigram$Word),
                unigram$Frequency,
                min.freq = 100,
                max.words = 100,
                colors = brewer.pal(11, "Paired"),
                random.order = F,
                random.color = T,
                rot.per = .15,
                scale = c(4, 0.5),
                main = paste("WordCloud of", Type, "UniGram")
        )
        wordcloud(
                as.character(digram$Word),
                digram$Frequency,
                min.freq = 25,
                max.words = 75,
                colors = brewer.pal(11, "Paired"),
                random.order = F,
                random.color = T,
                rot.per = .15,
                scale = c(4, 0.5),
                main = paste("WordCloud of", Type, "BiGram")
        )
        mtext(
                paste("WordCloud of UniGram & Bigram of", Type),
                side = 3,
                line = -2,
                outer = TRUE
        )
        par(mfrow = c(1, 1))
}
cloudPlotDuo(tweet.unigram, tweet.digram, "Tweets")
cloudPlotDuo(blog.unigram, blog.digram, "Blogs")
cloudPlotDuo(news.unigram, news.digram, "News")
cloudPlotDuo(part.unigram, part.digram, "All Source")

cloudPlotQuad <-
        function(tweetgram,
                 bloggram,
                 newsgram,
                 partgram,
                 Type = "UniGram") {
                par(mfrow = c(2, 2))
                wordcloud(
                        as.character(tweetgram$Word),
                        tweetgram$Frequency,
                        min.freq = 10,
                        max.words = 75,
                        colors = brewer.pal(11, "Paired"),
                        random.order = F,
                        random.color = T,
                        rot.per = .15,
                        scale = c(4, 0.5)
                )
                wordcloud(
                        as.character(bloggram$Word),
                        bloggram$Frequency,
                        min.freq = 10,
                        max.words = 75,
                        colors = brewer.pal(11, "Paired"),
                        random.order = F,
                        random.color = T,
                        rot.per = .15,
                        scale = c(4, 0.5)
                )
                wordcloud(
                        as.character(newsgram$Word),
                        newsgram$Frequency,
                        min.freq = 10,
                        max.words = 75,
                        colors = brewer.pal(11, "Paired"),
                        random.order = F,
                        random.color = T,
                        rot.per = .15,
                        scale = c(4, 0.5)
                )
                wordcloud(
                        as.character(partgram$Word),
                        partgram$Frequency,
                        min.freq = 10,
                        max.words = 75,
                        colors = brewer.pal(11, "Paired"),
                        random.order = F,
                        random.color = T,
                        rot.per = .15,
                        scale = c(4, 0.5)
                )
                mtext(
                        paste0("WordCloud of ", Type, "'s"),
                        side = 2,
                        line = -2,
                        outer = TRUE,
                        col = "blue",
                        cex = 1.4
                )
                mtext(
                        "News                                          Tweet",
                        side = 2,
                        line = -3,
                        outer = TRUE,
                        col = "black",
                        cex = 1.2
                )
                mtext(
                        "All Source                                     Blog",
                        side = 4,
                        line = -2,
                        outer = TRUE,
                        col = "black",
                        cex = 1.2
                )
                
                par(mfrow = c(1, 1))
        }

w1u <- cloudPlotQuad(
        tweetgram = tweet.unigram,
        bloggram = blog.unigram,
        newsgram = news.unigram,
        partgram = part.unigram,
        Type = "UniGram"
)
w2u <- cloudPlotQuad(
        tweetgram = tweet.digram,
        bloggram = blog.digram,
        newsgram = news.digram,
        partgram = part.digram,
        Type = "BiGram"
)
