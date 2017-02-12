#Author : Manojkumar PArmar
#Date: 22/01/2017
#DataScience Capstone Week1 Quiz.

#load libraries
library(datasets)

#cleanup environmant
rm(list = ls())
cat("\014")

#read All files
#if file is not available then read else load file with all data
if(! file.exists("./00_Data/All_Read.RData")){
        tweetFile <- file("./00_Data/final/en_US/en_US.twitter.txt", "r")
        blogFile <- file("./00_Data/final/en_US/en_US.blogs.txt","r")
        newsFile <- file("./00_Data/final/en_US/en_US.news.txt","r")
        contentTweet <- readLines(tweetFile)
        contentBlog <- readLines(blogFile)
        contentNews <- readLines(newsFile)
        close(tweetFile)
        close(blogFile)
        close(newsFile)
        save.image("./00_Data/All_Read.RData")
}else{
        load("./00_Data/All_Read.RData")
        
}

#Q1 :  The 𝚎𝚗_𝚄𝚂.𝚋𝚕𝚘𝚐𝚜.𝚝𝚡𝚝  file is how many megabytes?
file.size("./00_Data/final/en_US/en_US.blogs.txt")/2^20

#Q2 : The 𝚎𝚗_𝚄𝚂.𝚝𝚠𝚒𝚝𝚝𝚎𝚛.𝚝𝚡𝚝 has how many lines of text?
lenTweetFile <- length(contentTweet)

#Q3 : What is the length of the longest line seen in any of the 
#     three en_US data sets?
tweetMax <- max(unname(sapply(contentTweet, nchar)))
newsMax <- max(unname(sapply(contentNews, nchar)))
blogMax <- max(unname(sapply(contentBlog, nchar)))

#Q4 : In the en_US twitter data set, if you divide the number of lines where 
#     the word "love" (all lowercase) occurs by the number of lines the word 
#     "hate" (all lowercase) occurs, about what do you get?
countlove <- length(grep("love", contentTweet))
counthate <- length(grep("hate", contentTweet))
countlove/counthate

#Q5 : The one tweet in the en_US twitter data set that matches the word
#     "biostats" says what?
contentTweet[grep("biostats", contentTweet)]

#Q6 : How many tweets have the exact characters "A computer once beat me 
#     at chess, but it was no match for me at kickboxing". (I.e. the line 
#     matches those characters exactly.)
length(grep("A computer once beat me at chess, but it was no match for 
            me at kickboxing", contentTweet))
