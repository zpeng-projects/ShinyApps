library(shiny)
library(twitteR)
library(tm)
library(SnowballC) 
library(quantmod)
library(TTR)
library(MASS)
library(class)
api_key <- "HJB9l39OhH7XKqr6deYHROft6"
api_secret <- "l4ZyUExemzqPnEeD5qtw5aHuuN8oSMCOe7pNSNaE7lTVHzuGYI"
access_token <- "2534082073-qlC6nlEgvmHb1zzJCqDhZhtIqIAYgZZBDJcOVra"
access_token_secret <- "GSdNj3Clasqm4Ub5Nl647y8cuafphvPi5NpcHkdasFk0p"

func_removeNonAscii<-function(x) {
  dat2 <- unlist(strsplit(x, split=" "))
  dat3 <- grepl("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
  dat4 <- dat2[!dat3]
  dat5 <- paste(dat4, collapse = " ")
}

hu.liu.pos<-scan("positive-words.txt", what="character", comment.char=";")
hu.liu.neg<-scan("negative-words.txt", what="character", comment.char=";")

shinyServer(function(input, output) {
  
  data <- reactive({  
    setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
    tweets<-searchTwitter({input$tw_query}, n={input$tw_number}*1.2,lang="en")
    tw_df<-do.call("rbind", lapply(tweets, as.data.frame))     
  })
  
  
  
})
