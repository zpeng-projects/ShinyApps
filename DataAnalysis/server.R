library(shiny)
library(twitteR)
library(tm)
library(SnowballC) 
api_key <- "HJB9l39OhH7XKqr6deYHROft6"
api_secret <- "l4ZyUExemzqPnEeD5qtw5aHuuN8oSMCOe7pNSNaE7lTVHzuGYI"
access_token <- "2534082073-qlC6nlEgvmHb1zzJCqDhZhtIqIAYgZZBDJcOVra"
access_token_secret <- "GSdNj3Clasqm4Ub5Nl647y8cuafphvPi5NpcHkdasFk0p"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
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
    tweets<-searchTwitter({input$tw_query}, n={input$tw_number},lang="en")
    tw_df<-do.call("rbind", lapply(tweets, as.data.frame)) 
  })
  
  # Generate a plot of the data. Also uses the inputs to build the 
  # plot label. Note that the dependencies on both the inputs and
  # the data reactive expression are both tracked, and all expressions 
  # are called in the sequence implied by the dependency graph
  prePdata <- reactive({
    tw_df<-data()
    tw_df$text<-sapply(tw_df$text,func_removeNonAscii)
    myCorpus <- Corpus(VectorSource(tw_df$text))    
    myCorpus <- tm_map(myCorpus, removePunctuation)
    myCorpus <- tm_map(myCorpus, removeNumbers)
    myCorpus <- tm_map(myCorpus, tolower)
    myStopwords <- c(stopwords('english'))
    myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
    dictCorpus <- myCorpus    
    myCorpus <- tm_map(myCorpus, stemDocument)
    myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=dictCorpus)  
    myCorpus<-tm_map(myCorpus, stripWhitespace)   
    outD<-list( myCorpus,tw_df$created)
    myCorpus<-data()[[1]]
    temp<-data()[[2]]
    len<-length(temp)
    timeP<-temp-temp[len]
    pos<-sapply(myCorpus,function(x) strsplit(x," "))
    pos1<-sapply(pos,function(x) sum(x%in%hu.liu.pos))
    neg1<-sapply(pos,function(x) sum(x%in%hu.liu.neg))
    sen<-pos1-neg1
    m<-{input$tw_sm}*floor(len/2)
    ax<-rep(0,len)
    for(i in seq_len(len))
    {
      lo<-ifelse(i<=m,1,i-m)
      hi<-ifelse(i>=(len-m),len,i+m)
      ax[i]<-sum(sen[lo:hi])
    } 
    a<-data.frame(x=timeP,y=ax,z=sen,t=myCorpus)
  })
  
  
  output$plot <- renderPlot({   
    df<-processed_data()
        
    plot(df$x,df$y,type="l",col="red") 
      
  })
  
#   # Generate a summary of the data
#   output$summary <- renderPrint({
#     summary(data())
#   })
#   
  # Generate an HTML table view of the data
    output$table <- renderTable({
    inspect(processed_data()[,4])
  })
})
