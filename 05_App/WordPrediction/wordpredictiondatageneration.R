#Clean up the environment
rm(list = ls())
#clean memory function to run garbage collector, free up memory
cleanMemory <- function() {
        for (i in 1:10)
                gc(reset = T)
}

## Supportive functions
#sampling function
sampletext <- function(textbody, portion) {
        taking <- sample(1:length(textbody),
                         length(textbody) * portion)
        Sampletext <- textbody[taking]
        Sampletext
}

#Data Cleaning , the corpus was converted to lowercase, strip white space, and removed punctuation and numbers.
cleansing <- function (textcp) {
        textcp <- tm_map(textcp, content_transformer(tolower))
        textcp <- tm_map(textcp, stripWhitespace)
        textcp <- tm_map(textcp, removePunctuation)
        textcp <- tm_map(textcp, removeNumbers)
        textcp
}

#Generate twitter data from given twitter corpus
generateTwitterData <- function() {
        #load necessary libraies
        library(ggplot2)
        library(RWeka)
        library(R.utils)
        library(dplyr)
        library(parallel)
        library(wordcloud)
        library(parallel)
        library(RWeka)
        library(R.utils)
        library(tm)
        #Read corpus file
        con1 <- file("./00_Data/final/en_US/en_US.twitter.txt", open = "rb")
        twitter <-
                readLines(con1, skipNul = TRUE, encoding = "UTF-8")
        close(con1)
        #sample data
        set.seed(26122017)
        portion <- 0.1
        sampleTwitter <- sampletext(twitter, portion)

        # write sampled texts into text files for further analysis
        writeLines(
                sampleTwitter,
                "./05_App/WordPrediction/sampletwitter10p/SampleTwitter10p.txt"
        )
        
        #load corpus for processing
        sampleTwitter <-
                VCorpus(
                        DirSource(
                                "./05_App/WordPrediction/sampletwitter10p/",
                                encoding = "UTF-8"
                        )
                )
        
        # clean up text and store in object for processing
        sampleTwitter <- cleansing(sampleTwitter)
        saveRDS(
                sampleTwitter,
                "./05_App/WordPrediction/intermediate_data/cleansampletwitter10p.RData"
        )
}
#Generate normal data from given all corpus
generateNormalData <- function() {
        #load necessary libraies
        library(ggplot2)
        library(RWeka)
        library(R.utils)
        library(dplyr)
        library(parallel)
        library(wordcloud)
        library(parallel)
        library(RWeka)
        library(R.utils)
        library(tm)
        
        #Read corpus files
        con1 <- file("./00_Data/final/en_US/en_US.twitter.txt", open = "rb")
        twitter <-
                readLines(con1, skipNul = TRUE, encoding = "UTF-8")
        close(con1)

        con2 <- file("./00_Data/final/en_US/en_US.news.txt", open = "rb")
        news <- readLines(con2, skipNul = TRUE, encoding="UTF-8")
        close(con2)

        con3 <- file("./00_Data/final/en_US/en_US.blogs.txt", open = "rb")
        blogs <- readLines(con3, skipNul = TRUE, encoding="UTF-8")
        close(con3)

        #sample data
        set.seed(26122017)
        portion <- 0.5
        sampleTwitter <- sampletext(twitter, portion)
        sampleBlog <- sampletext(blogs, portion)
        sampleNews <- sampletext(news, portion)

        # combine sampled texts into one variable
        sampleNormal <- c(sampleNews, sampleBlog, sampleTwitter)

        # write sampled texts into text files for further analysis
        writeLines(sampleNormal, "./05_App/WordPrediction/sampleall/SampleNormal.txt")
        writeLines(
                sampleTwitter,
                "./05_App/WordPrediction/sampletwitter10p/SampleTwitter10p.txt"
        )
        
        sampleNormal <-
                VCorpus(
                        DirSource(
                                "./05_App/WordPrediction/sampleall/",
                                encoding = "UTF-8"
                        )
                )
        
        #clean up text and store in object for processing
        sampleNormal <- cleansing(sampleNormal)
        saveRDS(
                sampleNormal,
                "./05_App/WordPrediction/intermediate_data/cleansamplenormal.RData"
        )
}

#Function to generate n-grams for used in shiny app
ngram_generation <- function(sampleData, ngramSize, type = "Normal") {
        library(RWeka)
        library(R.utils)
        library(dplyr)
        library(parallel)
        library(tm)
        
        # Define function to make N grams
        tdm_Ngram <- function (textcp, n) {
                NgramTokenizer <-
                        function(x) {
                                RWeka::NGramTokenizer(x, RWeka::Weka_control(min = n, max = n))
                        }
                tdm_ngram <-
                        TermDocumentMatrix(textcp, control = list(tokenizer = NgramTokenizer))
                tdm_ngram
        }
        
        # Define function to extract the N grams and sort
        ngram_sorted_df <- function (tdm_ngram) {
                tdm_ngram_m <- as.matrix(tdm_ngram)
                tdm_ngram_df <- as.data.frame(tdm_ngram_m)
                colnames(tdm_ngram_df) <- "Count"
                tdm_ngram_df <-
                        tdm_ngram_df[order(-tdm_ngram_df$Count), , drop = FALSE]
                tdm_ngram_df
        }
        
        # Save data frames into r-compressed files
        tdm_ngram <- tdm_Ngram(sampleData, ngramSize)
        tdm_ngram_df <- ngram_sorted_df(tdm_ngram)
        ngram <-
                data.frame(rows = rownames(tdm_ngram_df),
                           count = tdm_ngram_df$Count)
        ngram$rows <- as.character(ngram$rows)
        ngram_split <-
                strsplit(as.character(ngram$rows), split = " ")
        if (ngramSize == 4) {
                ngram <-
                        transform(
                                ngram,
                                first = sapply(ngram_split, "[[", 1),
                                second = sapply(ngram_split, "[[", 2),
                                third = sapply(ngram_split, "[[", 3),
                                fourth = sapply(ngram_split, "[[", 4)
                        )
                ngram <-
                        data.frame(
                                unigram = ngram$first,
                                bigram = ngram$second,
                                trigram = ngram$third,
                                quadgram = ngram$fourth,
                                freq = ngram$count,
                                stringsAsFactors = FALSE
                        )
        } else if (ngramSize == 3) {
                ngram <-
                        transform(
                                ngram,
                                first = sapply(ngram_split, "[[", 1),
                                second = sapply(ngram_split, "[[", 2),
                                third = sapply(ngram_split, "[[", 3)
                        )
                ngram <-
                        data.frame(
                                unigram = ngram$first,
                                bigram = ngram$second,
                                trigram = ngram$third,
                                freq = ngram$count,
                                stringsAsFactors = FALSE
                        )
        } else if (ngramSize == 2) {
                ngram <-
                        transform(
                                ngram,
                                first = sapply(ngram_split, "[[", 1),
                                second = sapply(ngram_split, "[[", 2)
                        )
                ngram <-
                        data.frame(
                                unigram = ngram$first,
                                bigram = ngram$second,
                                freq = ngram$count,
                                stringsAsFactors = FALSE
                        )
        } else{
                return(NULL)
        }
        nameFile = paste0(
                "./05_App/WordPrediction/intermediate_data/",
                type,
                "_ngram_",
                ngramSize,
                "gram.csv"
        )
        nameFile1 = paste0(
                "./05_App/WordPrediction/",
                type,
                "_ngram_",
                ngramSize,
                "gram.RData"
        )
        write.csv(ngram[ngram$freq > 1,], nameFile, row.names = F)
        ngram <- read.csv(nameFile, stringsAsFactors = F)
        saveRDS(
                ngram,
                nameFile1
        )
        cleanMemory()
        ngram
}

# Calculate N-Grams for twitter
sampleTwitter <-
        readRDS("./05_App/WordPrediction/intermediate_data/cleansampletwitter10p.RData")

cleanMemory()
quadgram <- ngram_generation(sampleTwitter, 4, "Twitter")
trigram <- ngram_generation(sampleTwitter, 3, "Twitter")
bigram <- ngram_generation(sampleTwitter, 2, "Twitter")

rm(quadgram)
rm(trigram)
rm(bigram)
cleanMemory()

# Calculate N-Grams for normal
# sampleNormal <-
#         readRDS("./05_App/WordPrediction/intermediate_data/cleansamplenormal.RData")
# cleanMemory()
# quadgram <- ngram_generation(sampleNormal, 4)
# trigram <- ngram_generation(sampleNormal, 3)
# bigram <- ngram_generation(sampleNormal, 2)