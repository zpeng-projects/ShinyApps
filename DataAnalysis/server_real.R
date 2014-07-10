library(shiny)
library(twitteR)
library(tm)
library(SnowballC) 
library(quantmod)
library(TTR)
library(MASS)
library(class)

shinyServer(function(input, output) {
    
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
        train_pred<-table(rand.pred, training$updown)
        
        rand.pred<-as.factor(ifelse(runif(ntest)>0.5,"up","down"))        
        valid_pred<-table(rand.pred, testing$updown)
      }
      if({input$model}=="logireg"){
        fit =glm(updown ~ . ,data =training, family = binomial)
        
        glm.probs = predict(fit ,type ="response")
        glm.pred =rep ("down",ntrain)
        glm.pred[glm.probs >0.5]="up"        
        train_pred<-table(glm.pred, training$updown)
        
        val.probs<-predict(fit,testing,type ="response")
        glm.pred =rep ("down",ntest)
        glm.pred[val.probs >0.5]="up"  
        valid_pred<-table(glm.pred, testing$updown)
      }
      if({input$model}=="lda"){
        fit =lda(updown ~ . ,data =training)
        
        probs = predict(fit)
        pred =probs$class        
        train_pred<-table(pred, training$updown)
        
        probs<-predict(fit,testing)
        pred =probs$class          
        valid_pred<-table(pred, testing$updown)
      }
      if({input$model}=="qda"){
        fit =qda(updown ~ . ,data =training)
        
        probs = predict(fit)
        pred =probs$class        
        train_pred<-table(pred, training$updown)
        
        probs<-predict(fit,testing)
        pred =probs$class          
        valid_pred<-table(pred, testing$updown)
      }
      if({input$model}=="knn"){
        
        train.x<-training[,-m]
        test.x<-testing[,-m]
        train.y<-training[,m]
        test.y<-testing[,m]        
        fit =knn(train.x,test.x, train.y, {input$N_knn})        
        train_pred<-NULL                
        valid_pred<-table(fit, testing$updown)
      }
      if({input$model}=="tree"){
        library(tree)
        fit =tree(updown ~ . ,data =training)
        
        probs = predict(fit,training, type ="class")
        pred =probs        
        train_pred<-table(pred, training$updown)
        
        probs<-predict(fit,testing,type ="class")
        pred =probs         
        valid_pred<-table(pred, testing$updown)
      }
      if({input$model}=="rf"){
        library(randomForest)
        set.seed(1234)
        fit =randomForest(updown ~ . ,data =training, mtry ={input$n_sub}, 
                          ntree ={input$ntree})
        
        probs = predict(fit)
        pred =probs        
        train_pred<-table(pred, training$updown)
        
        probs<-predict(fit,newdata=testing)
        pred =probs         
        valid_pred<-table(pred, testing$updown)
      }
      
      if({input$model}=="svm"){
        library(e1071)
        training$updown<-as.factor(training$updown)
        testing$updown<-as.factor(test$updown)
        fit =svm(updown ~ . ,data =training, kernel={input$kernel}, 
                 cost ={input$cost}, scale = {input$scale})
        
        probs = predict(fit)
        pred =probs        
        train_pred<-table(pred, training$updown)
        
        probs<-predict(fit,newdata=testing)
        pred =probs         
        valid_pred<-table(pred, testing$updown)
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
            numericInput("N_knn", "K nearest neighbors:", 10)}
      if({input$model}=="rf"){       
            c(numericInput("n_sub", "number of subset of variables:", 2),
              numericInput("ntree", "number of trees:", 500))}  
      if({input$model}=="svm"){       
            c(selectInput("kernel", "Choose a kernel:", choices = c("linear", "radial")), 
              selectInput("scale", "scale data?",choices = c(TRUE, FALSE)),
              numericInput("cost", "Cost:0.01-10e5",10))}
   })

  output$table1 <- renderTable({
        dat<-model_do()[[1]]
        dat
    })

  output$table2 <- renderTable({
        dat<-model_do()[[2]]
        dat
  })

  
})
