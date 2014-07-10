library(shiny)
shinyServer(function(input, output) {

data_work <- reactive({  
  dist <- switch(input$dist_t,
                 norm = rnorm,
                 unif = runif,
                 lnorm = rlnorm,
                 exp = rexp,
                 rnorm)
  
  dist(input$n_t)
})
output$plot_t <- renderPlot({
  dist <- input$dist_t
  n <- input$n
  
  hist(data_work(), 
       main=paste('r', dist, '(', n, ')', sep=''))
})

# Generate a summary of the data
output$summary_t <- renderPrint({
  summary(data_work())
})

# Generate an HTML table view of the data
output$table_t <- renderTable({
  data.frame(x=data_work())
})





})
