
a_test <- 1
hu.liu.pos<-scan("positive-words.txt", what="character", comment.char=";")
hu.liu.neg<-scan("negative-words.txt", what="character", comment.char=";")
library(twitteR) 
library(tm)
api_key <- "HJB9l39OhH7XKqr6deYHROft6"      
api_secret <- "l4ZyUExemzqPnEeD5qtw5aHuuN8oSMCOe7pNSNaE7lTVHzuGYI"      
access_token <- "2534082073-qlC6nlEgvmHb1zzJCqDhZhtIqIAYgZZBDJcOVra"      
access_token_secret <- "GSdNj3Clasqm4Ub5Nl647y8cuafphvPi5NpcHkdasFk0p"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

shinyServer(
  
  
  function(input, output, session) {   
    senti_data <- reactive({    
      tweets<-searchTwitter({input$tw_queries}, n=1000000,lang="en")
      tw_df<-do.call("rbind", lapply(tweets, as.data.frame))
      myCorpus <- Corpus(VectorSource(tw_df$text))
    })
    stock_data <- reactive({   
      
      #cdata <- session$clientData
      #output$clientdataText <- renderText({
      #cnames <- names(cdata)        
      #allvalues <- lapply(cnames, function(name) {
      #    paste(name, cdata[[name]], sep=" = ")
      #  })
      #  paste(allvalues, collapse = "\n")
      #})
      
      # A histogram
      #output$myplot <- renderPlot({
      #  hist(rnorm(input$obs), main="Generated in renderPlot()")
      #})
      
      model_order<-{input$model_order}
      start_date<-{input$start_date}
      end_date<-{input$end_date}
      s_d<-format(start_date, "%d")
      s_m<-format(start_date, "%m")
      s_y<-format(start_date, "%Y")
      e_d<-format(end_date, "%d")
      e_m<-format(end_date, "%m")
      e_y<-format(end_date, "%Y")      
      url<-paste("http://ichart.finance.yahoo.com/table.csv?s=",toupper({input$stock_name}),"&a=",
                 s_m,"&b=",s_d,"&c=",s_y,"&d=",e_m,"&e=",e_d,"&f=",e_y,"&g=d&ignore=.csv",sep="")
      stock_df<-read.csv(url,sep=",",stringsAsFactors=FALSE) 
      aa<-as.numeric({input$price_type})
      stock_df[,1]<-as.Date(stock_df[,1])
      y<-stock_df[!is.na(stock_df[,aa]),aa]
      x<-stock_df[!is.na(stock_df[,aa]),1]      
      all.data<-list(stockDate=x, stockPrice=y,data)
    }) 
    
#     output$urlText <- renderText({
#       paste(sep = "",
#             "protocol: ", session$clientData$url_protocol, "\n",
#             "hostname: ", session$clientData$url_hostname, "\n",
#             "pathname: ", session$clientData$url_pathname, "\n",
#             "port: ",     session$clientData$url_port,     "\n",
#             "search: ",   session$clientData$url_search,   "\n"
#       )
#     })
    
#     output$queryText <- renderText({
#       query <- parseQueryString(session$clientData$url_search)
#       
#       # Return a string with key-value pairs
#       paste(names(query), query, sep = "=", collapse=", ")
#     })
    
    output$plot <- renderPlot({
      model_order<-{input$model_order}
      data_all<-stock_data()
      
      x<-data_all[[1]]
      y<-data_all[[2]]
      n<-length(x)
      xla<-paste("number of days since",as.character(x[n]))
      xx<-as.numeric(difftime(as.Date(as.character(x)),as.Date(as.character(x[n])),units="days"))
      plot(xx,y,xlab=xla,ylab="stock price ($)",col="red")  
      title(paste("Stock price of",toupper({input$stock_name})))
      xfit<-lm(y~ poly(xx,model_order))
      points(xx, predict(xfit), type="l", col="blue", lwd=2)
      title(a_test)
    })   
output$view <- renderTable({
  a<-senti_data()[,c(1,5)]
  #a[,1]
})
  
  
})
