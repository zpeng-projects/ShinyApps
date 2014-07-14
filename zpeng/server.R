library(shiny)
library(Rcpp)
library(quantmod)
library(TTR)
library(MASS)
library(class)
library(ROCR)
library(pROC)
library(Deducer)
library(ggplot2)
library(tree)
library(randomForest)
library(caret)
library(e1071)
library(kernlab)
library(tm)
library(SnowballC) 

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
  
  # twitter functions
  data <- reactive({    
    library(twitteR)
    setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
    tweets<-searchTwitter({input$tw_query}, n={input$tw_number}*1.2,lang="en")
    tw_df<-do.call("rbind", lapply(tweets, as.data.frame))     
  })
  
  prep_data <- reactive({ 
    tw_df<-data()
    raw_tweet<-tw_df$text
    tw_df$text<-sapply(tw_df$text,func_removeNonAscii)    
    tw_df<-tw_df[!grepl("sex|porn|nude",tolower(tw_df[,1])),]
    tw_df[,1]<-gsub("[hH][Tt][Tt][Pp]","",tw_df[,1])
    if(nrow(tw_df)>{input$tw_number}) {tw_df<-tw_df[1:{input$tw_number},]}
    myCorpus <- Corpus(VectorSource(tw_df$text))    
    myCorpus <- tm_map(myCorpus, removePunctuation)
    myCorpus <- tm_map(myCorpus, removeNumbers)
    myCorpus <- tm_map(myCorpus, tolower)
    myStopwords <- c(stopwords('english'))
    myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
    dictCorpus <- myCorpus    
    myCorpus <- tm_map(myCorpus, stemDocument)
 #  myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=dictCorpus)  
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
    
    m<-floor(len/10)
    ax<-rep(0,len)
    for(i in seq_len(len))
    {
      lo<-ifelse(i<=m,1,i-m)
      hi<-ifelse(i>=(len-m),len,i+m)
      ax[i]<-mean(sen[lo:hi])
    } 
    b<-list(ax,sen,ord,a[[2]],a[[3]])
  })
 
  rec_data <- reactive({ 
   a<-dat<-process_data()
   
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
    title(paste("sentiment change for \"", 
{input$tw_query},"\"", "\n total", len, "in the last",round(timeP[1],2),unit))

  })
  
  output$table <- renderTable({
  dat<-process_data()  
  ord<-dat[[3]]
  if({input$tw_type}=="rawtw"){avv<-data.frame(tweets=dat[[5]][ord,1])}
  else {tweetst<-unlist(dat[[4]])
  avv<-data.frame(tweets=tweetst[ord])}
  avv
})
   
  # stock 
  stock_dat <- reactive({    
  stock = getSymbols({input$stock_name}, from={input$start_date},
                     to={input$end_date},src="yahoo", auto.assign=F) }) 

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
tmp[1:n-1]<-ClCl(dat)[2:n]
df$updown<-tmp
m<-m+1
j<-n-1
df$updown<-ifelse(df$updown>0,"up","down")
df<-df[m:j,]    
})

  output$plot1 <- renderPlot({
  a<-stock_dat()
  if ({input$cht}=="lchart") lineChart(a)
  if ({input$cht}=="bchart") barChart(a)
  if ({input$cht}=="cchart") candleChart(a,multi.col=TRUE)
  addBBands()
})  

  model_do<-reactive({
  
  dat<-model_dat()
  n<-nrow(dat)
  m<-ncol(dat)
  ratio<-{input$sampleR}
  set.seed(120)
  trainInd<-sample(1:n,floor(n*ratio))
  dat$updown<-as.factor(dat$updown)
  training<-dat[trainInd,]
  testing<-dat[-trainInd,]
  ntrain<-nrow(training)
  ntest<-nrow(testing)
  
  if({input$model}=="random"){
    set.seed(1234)
    fit<-NULL
    rand.pred<-as.factor(ifelse(runif(ntrain)>0.5,"up","down"))
    train_pred<-table(prediction=rand.pred, truth=training$updown)
    
    rand.pred<-as.factor(ifelse(runif(ntest)>0.5,"up","down"))        
    valid_pred<-table(prediction=rand.pred, truth=testing$updown)
  }
  else if({input$model}=="logireg"){
    fit =glm(updown ~ . ,data =training, family = binomial)
    
    glm.probs = predict(fit, training,type ="response")
    glm.pred =rep ("down",ntrain)
    glm.pred[glm.probs >0.5]="up"        
    train_pred<-table(glm.pred, training$updown)
    
    val.probs<-predict(fit,testing,type ="response")
    glm.pred =rep ("down",ntest)
    glm.pred[val.probs >0.5]="up"  
    valid_pred<-table(glm.pred, testing$updown)
    fit<-list(glm.probs,training$updown)
    
  }
  else if({input$model}=="lda"){
    fit =lda(updown ~ . ,data =training)    
    probs = predict(fit,training)    
    pred.lda <- predict(fit,training)$post[,2]
    pred =probs$class        
    train_pred<-table(pred, training$updown)
    
    probs<-predict(fit,testing)
    pred =probs$class          
    valid_pred<-table(pred, testing$updown)
    fit<-list(pred.lda,training$updown)
    
  }
  else if({input$model}=="qda"){
    fit =qda(updown ~ . ,data =training)
    
    probs = predict(fit,training)
    pred.qda <- predict(fit,training)$post[,2]
    pred =probs$class        
    train_pred<-table(pred, training$updown)
    
    probs<-predict(fit,testing)
    pred =probs$class          
    valid_pred<-table(pred, testing$updown)
    fit<-list(pred.qda,training$updown)
  }
  else if({input$model}=="knn"){    
    train.x<-training[,-m]
    test.x<-testing[,-m]
    train.y<-as.factor(training[,m])
    test.y<-as.factor(testing[,m])        
    fit <- knn(train.x,test=test.x,cl=train.y, k={input$N_knn})  
    pred<-fit
    train_pred<-NULL             
    valid_pred<-table(fit, test.y)
    fit<-list(pred[,2],test.y)
  }
  else if({input$model}=="tree"){
    
    fit =tree(updown ~ . ,data =training)
    
    probs = predict(fit,training, type ="class")
    pred =probs        
    train_pred<-table(pred, training$updown)
    
    probs<-predict(fit,testing,type ="class")
    pred =probs         
    valid_pred<-table(pred, testing$updown)
    #fit<-list(probs,training$updown)
  }
  else if({input$model}=="rf"){
    library (e1071)
    set.seed(1234)
    cvCtrl <- trainControl(method = "repeatedcv", repeats = 3)
    fit<-train(updown ~ .,data = training, method = "rf",trControl = cvCtrl)
    #fit =randomForest(updown ~ . ,data =training, mtry ={input$n_sub},ntree ={input$ntree})
    
    probs = predict(fit)
    pred =probs        
    train_pred<-table(pred, training$updown)
    
    probs<-predict(fit,newdata=testing)
    pred =probs         
   
  }
  
  else if({input$model}=="svm"){  
  }
  outP<-list(train_pred,valid_pred,fit)
 
  
  #   })
  
  #   output$Pre1 <- renderPrint({
  #     
  #     a<-model_dat()
  #     a
  #     
  #   })
  # Generate a summary of the data
  #    output$summary <- renderPrint({
  #      a<-model_do
  #      a
})

  output$testControls <- renderUI({
  if({input$model}=="knn"){
    numericInput("N_knn", "K nearest neighbors:", value=1, min=1, step=1)}
  else if({input$model}=="rf"){       
    c(numericInput("n_sub", "number of subset of variables:", 2),
      numericInput("ntree", "number of trees:", 500))}  
#   else if({input$model}=="svm"){       
#     list(selectInput("kernel", "Choose a kernel:", choices = c("linear","polynomial", "radial")), 
#       selectInput("scale", "scale data?",choices = c(FALSE,TRUE)),
#       numericInput("cost", "Cost:0.01-10e5", 1))
#     ifelse({input$kernel}=="polynomial",numericInput("porder", "polynomial order:", 2),NULL),
#     ifelse({input$kernel}=="radial",numericInput("gamma", "gamma", value=1),NULL))
 #   }
  
  
})

    output$rocresult <- renderPlot({
      dat<-model_do()      
      if({input$model}=="tree") {
        plot(dat[[3]])
        text(dat[[3]], pretty =0)}
      else if({input$model}=="svm") {
        a<-dat[[3]]
       rocplot(a[[1]],a[[2]])}       
      else {
        pred.roc<- prediction(dat[[3]][[1]],dat[[3]][[2]])
        perf.roc<- performance(pred.roc, "tpr", "fpr")
        plot(perf.roc)}
      })

    output$ConfusionTrain <- renderTable({
      
      if({input$model}!="knn")
      {
        dat<-model_do()[[1]]         
        dat } 
      })
  
    output$resultTrain <- renderPrint({
      if({input$model}!="knn")
      {dat<-model_do()[[1]]         
      paste("Accuracy:",round((dat[1,1]+dat[2,2])/(dat[1,1]+dat[2,2]+dat[1,2]+dat[2,1])*100,2),"%")}      
      })

    output$ConfusionTest <- renderTable({
      dat<-model_do()[[2]]
      dat
      })
    output$resultTest <- renderPrint({
      dat<-model_do()[[2]]         
      paste("Accuracy:",round((dat[1,1]+dat[2,2])/(dat[1,1]+dat[2,2]+dat[1,2]+dat[2,1])*100,2),"%")
})
  
  
})
