#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

suppressWarnings(library(tm))
suppressWarnings(library(stringr))
suppressWarnings(library(shiny))

#Load normal quadgrams, trigrams, bigrams.
qgramn <- readRDS("Normal_ngram_4gram.RData");
tgramn <- readRDS("Normal_ngram_3gram.RData");
bgramn <- readRDS("Normal_ngram_2gram.RData");

# Load twitter quadgrms, trigrms, bigrams
qgramt <- readRDS("Twitter_ngram_4gram.RData");
tgramt <- readRDS("Twitter_ngram_3gram.RData");
bgramt <- readRDS("Twitter_ngram_2gram.RData");

#Info for using which kind of back off algorithm is used
backAlgoMsg <<- ""

#Normal word prediction engine
predictWordNormal <- function(word) {
        
        wordClean <- removeNumbers(removePunctuation(tolower(word)))
        wordSplit <- strsplit(wordClean, " ")[[1]]
        
        # Back Off Algorithm
        # Predict the next term of the user input sentence
        # 1. For prediction of the next word, Quadgram is first used (first three words of Quadgram are the last three words of the user provided sentence).
        # 2. If no Quadgram is found, back off to Trigram (first two words of Trigram are the last two words of the sentence).
        # 3. If no Trigram is found, back off to Bigram (first word of Bigram is the last word of the sentence)
        # 4. If no Bigram is found, back off to the most common word with highest frequency 'the' is returned.
        
        
        if (length(wordSplit) >= 3) {
                wordSplit <- tail(wordSplit, 3)
                if (identical(character(0), head(qgramn[qgramn$unigram == wordSplit[1] &
                                                          qgramn$bigram == wordSplit[2] & qgramn$trigram == wordSplit[3], 4], 1))) {
                        predictWordNormal(paste(wordSplit[2], wordSplit[3], sep = " "))
                }
                else {
                        backAlgoMsg <<-
                                "Next word is predicted using 4-gram."
                        head(qgramn[qgramn$unigram == wordSplit[1] &
                                              qgramn$bigram == wordSplit[2] & qgramn$trigram == wordSplit[3], 4], 1)
                }
        }
        else if (length(wordSplit) == 2) {
                wordSplit <- tail(wordSplit, 2)
                if (identical(character(0), head(tgramn[tgramn$unigram == wordSplit[1] &
                                                         tgramn$bigram == wordSplit[2], 3], 1))) {
                        predictWordNormal(wordSplit[2])
                }
                else {
                        backAlgoMsg <<-
                                "Next word is predicted using 3-gram."
                        head(tgramn[tgramn$unigram == wordSplit[1] &
                                             tgramn$bigram == wordSplit[2], 3], 1)
                }
        }
        else if (length(wordSplit) == 1) {
                wordSplit <- tail(wordSplit, 1)
                if (identical(character(0), head(bgramn[bgramn$unigram == wordSplit[1], 2], 1))) {
                        backAlgoMsg <<-
                                "No match found. Most common word 'the' is returned."
                        head("the", 1)
                }
                else {
                        backAlgoMsg <<-
                                "Next word is predicted using 2-gram."
                        head(bgramn[bgramn$unigram == wordSplit[1], 2], 1)
                }
        }
}

#Twitter word prediction engine
predictWordTwitter <- function(word) {
        
        wordClean <- removeNumbers(removePunctuation(tolower(word)))
        wordSplit <- strsplit(wordClean, " ")[[1]]
        
        # Back Off Algorithm
        # Predict the next term of the user input sentence
        # 1. For prediction of the next word, Quadgram is first used (first three words of Quadgram are the last three words of the user provided sentence).
        # 2. If no Quadgram is found, back off to Trigram (first two words of Trigram are the last two words of the sentence).
        # 3. If no Trigram is found, back off to Bigram (first word of Bigram is the last word of the sentence)
        # 4. If no Bigram is found, back off to the most common word with highest frequency 'the' is returned.
        
        
        if (length(wordSplit) >= 3) {
                wordSplit <- tail(wordSplit, 3)
                if (identical(character(0), head(qgramt[qgramt$unigram == wordSplit[1] &
                                                        qgramt$bigram == wordSplit[2] & qgramt$trigram == wordSplit[3], 4], 1))) {
                        predictWordTwitter(paste(wordSplit[2], wordSplit[3], sep = " "))
                }
                else {
                        backAlgoMsg <<-
                                "Next word is predicted using 4-gram Twitter Data."
                        head(qgramt[qgramt$unigram == wordSplit[1] &
                                            qgramt$bigram == wordSplit[2] & qgramt$trigram == wordSplit[3], 4], 1)
                }
        }
        else if (length(wordSplit) == 2) {
                wordSplit <- tail(wordSplit, 2)
                if (identical(character(0), head(tgramt[tgramt$unigram == wordSplit[1] &
                                                        tgramt$bigram == wordSplit[2], 3], 1))) {
                        predictWordTwitter(wordSplit[2])
                }
                else {
                        backAlgoMsg <<-
                                "Next word is predicted using 3-gram Twitter Data."
                        head(tgramt[tgramt$unigram == wordSplit[1] &
                                            tgramt$bigram == wordSplit[2], 3], 1)
                }
        }
        else if (length(wordSplit) == 1) {
                wordSplit <- tail(wordSplit, 1)
                if (identical(character(0), head(bgramt[bgramt$unigram == wordSplit[1], 2], 1))) {
                        backAlgoMsg <<-
                                "No match found. Most common word 'the' is returned."
                        head("the", 1)
                }
                else {
                        backAlgoMsg <<-
                                "Next word is predicted using 2-gram Twitter Data."
                        head(bgramt[bgramt$unigram == wordSplit[1], 2], 1)
                }
        }
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        output$prediction <- renderPrint({
                if(input$twtMode){
                        result <- predictWordTwitter(input$ipString)  
                        output$text2 <- renderText({backAlgoMsg})
                }else{
                        result <- predictWordNormal(input$ipString)  
                        output$text2 <- renderText({backAlgoMsg})   
                }
                result
        });
        output$text1 <- renderText({
                input$ipString});
}
)
