library(shiny)
library(twitteR)
library(tm)
library(SnowballC) 
library(quantmod)
library(TTR)
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
    
  stock_dat <- reactive({
    
    stock = getSymbols({input$stock_name}, from={input$start_date},
             to={input$end_date},src="yahoo", auto.assign=F)  

  }) 
  
  model_dat <- reactive({
    dat<-stock_dat()
    pred<-c({input$vol},{input$ret},{input$ret1},{input$ret2},
          {input$ret3},{input$ret4},{input$retSP},{input$retNA}) 
    n<-nrow(dat)
    df <- data.frame(matrix(ncol = length(pred), nrow = n))
    names(df)<-pred   
    m<-1
    
    if(!is.null({input$vol}))
    { 
      df$vol<-dat[,5]-Lag(dat[,5],c(1))      
    }
    
    if(!is.null({input$ret}))
    { 
      df$ret<-ClCl(dat)*100
    }  
    if(!is.null({input$ret4}))
        { m=5
       df$ret4<-Lag(ClCl(dat),c(4))*100
     }  
    if(!is.null({input$ret3}))
    {
      m=4
      df$ret3<-Lag(ClCl(dat),c(3))*100
    } 
    if(!is.null({input$ret2}))
    { m=3
      df$ret2<-Lag(ClCl(dat),c(2))*100
    } 
    if(!is.null({input$ret1}))
    { m=2
      df$ret1<-Lag(ClCl(dat),c(1))*100
    } 
    if(!is.null({input$retSP}))
    { 
      sp<-getSymbols("^GSPC", from={input$start_date},
                     to={input$end_date},src="yahoo", auto.assign=F)
      df$retSP<-ClCl(sp)*100
    }  
    if(!is.null({input$retNA}))
    { 
      NASD<-getSymbols("^IXIC", from={input$start_date},
                     to={input$end_date},src="yahoo", auto.assign=F)
      df$retNA<-ClCl(NASD)*100
    } 
    tmp<-rep(NA,n)
    tmp[1:n-1]<-df$ret[2:n]
    df$updown<-tmp
    m<-m+1
    j<-n-1
    df$updown<-ifelse(df$updown>0,1,0)
    df<-df[m:j,]    
  })
  
  
  data <- reactive({      
    tweets<-searchTwitter({input$tw_query}, n={input$tw_number}*1.2,lang="en")
    tw_df<-do.call("rbind", lapply(tweets, as.data.frame))     
  })
      
  
  prep_data <- reactive({ 
    tw_df<-data()
    raw_tweet<-tw_df$text
    tw_df$text<-sapply(tw_df$text,func_removeNonAscii)
    tw_df<-tw_df[!grepl("sex|porn|nude",tolower(tw_df[,1])),]
   if (nrow(tw_df)>{input$tw_number}) tw_df<-tw_df[1:{input$tw_number},]
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
    pos<-sapply(myCorpus,function(x) strsplit(x," "))
    pos1<-sapply(pos,function(x) sum(x%in%hu.liu.pos))
    neg1<-sapply(pos,function(x) sum(x%in%hu.liu.neg))
    sen<-pos1-neg1
    outP<-list(sen,myCorpus,tw_df)
  })
    
  process_data <- reactive({
    a<-prep_data() 
    sen<-a[[1]]   
    len<-length(sen)    
    ind_order_asc<-order(sen)
    
    if ({input$dat_order}=="desc")
    {ord<-ind_order_asc[len:{len+1-{input$tw_Read}}]}
    else if ({input$dat_order}=="asc") 
    {ord<-ind_order_asc[1:{input$tw_Read}]}
    else if ({input$dat_order}=="time")
    {ord<-1:{input$tw_Read}}  
    
    m<-floor(len/20)
    ax<-rep(0,len)
     for(i in seq_len(len))
     {
       lo<-ifelse(i<=m,1,i-m)
       hi<-ifelse(i>=(len-m),len,i+m)
       ax[i]<-mean(sen[lo:hi])
     } 
     b<-list(ax,sen,ord,a[[2]],a[[3]])
  })
  
  output$plot1 <- renderPlot({
    
    a<-stock_dat()
    if ({input$cht}=="lchart") lineChart(a)
    if ({input$cht}=="bchart") barChart(a)
    if ({input$cht}=="cchart") candleChart(a,multi.col=TRUE)
    addBBands()
     
#     model_order<-{input$model_order}
#     data_a<-stock_dat()
#     x<-data_a$b
#     y<-data_a$a
#     n<-length(x)
#     xla<-paste("number of days since",as.character(x[n]))
#     xx<-as.numeric(difftime(as.Date(as.character(x)),as.Date(as.character(x[n])),units="days"))
#     plot(xx,y,xlab=xla,ylab="stock price ($)",col="red")  
#     title(paste("Stock price of",toupper({input$stock_name})))
#     xfit<-lm(y~ poly(xx,model_order))
#     points(xx, predict(xfit), type="l", col="blue", lwd=2)
  })    
  
  
  
  output$plot <- renderPlot({   
    
    dat<-process_data()
    tmp<-as.numeric(dat[[5]]$created)
    len<-nrow(dat[[5]])
    timeP<-tmp-tmp[len]
    unit<-"second"
    if (timeP[1]>500) {
      timeP<-timeP/60
      unit<-"min"
    } 
    if (timeP[1]>500) {
      timeP<-timeP/60
      unit<-"hour"
    } 
    if (timeP[1]>200) {
      timeP<-timeP/24
      unit<-"day"
    } 
    plot(timeP,dat[[1]],type="l",col="red",ylab="Sentiment",xlab=paste("time in ",unit)) 
    title(paste("time series of sentiment for \"", 
                {input$tw_query},"\"", "\n total", len, "tweets retrieved in",timeP[1],unit))
      
  })
  
#   output$Pre1 <- renderPrint({
#     
#     a<-model_dat()
#     a
#     
#   })
 # Generate a summary of the data
#    output$summary <- renderPrint({
#      "searchin twitter ..."
#  })
output$table1 <- renderTable({
  dat<-model_dat()  
  table(dat$updown)
})

   output$table <- renderTable({
     dat<-process_data()  
     ord<-dat[[3]]
     tw<-dat[[5]][ord,1]
     c<-data.frame(tw)
 })
})
