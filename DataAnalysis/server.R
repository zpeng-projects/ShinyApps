shinyServer(
  function(input, output) {   
    dataValues <- reactive({
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
      
      all.data<-data.frame(a=y,b=x)      
    }) 
    
    
    output$plot <- renderPlot({
      model_order<-{input$model_order}
      data_a<-dataValues()
      x<-data_a$b
      y<-data_a$a
      n<-length(x)
      xla<-paste("number of days since",as.character(x[n]))
      xx<-as.numeric(difftime(as.Date(as.character(x)),as.Date(as.character(x[n])),units="days"))
      plot(xx,y,xlab=xla,ylab="stock price ($)",col="red")  
      title(paste("Stock price of",toupper({input$stock_name})))
      xfit<-lm(y~ poly(xx,model_order))
      points(xx, predict(xfit), type="l", col="blue", lwd=2)
    })    
})
