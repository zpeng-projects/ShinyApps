
library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Stock Analyzer"
 ),
  sidebarPanel(
    
    textInput("stock_name", "Stock:", value = "YHOO"),
    helpText("Enter stock symbles above"),
    tags$hr(),
    selectInput("price_type", "Price type:",
                list("Open price" = 2, 
                     "High price" = 3, 
                     "Low price" = 4,
                     "Close price" = 5)),
    helpText("Select price type above"),
    tags$hr(), 
    helpText("Select the time period:"),
    dateInput("start_date", "Start Date:", value = "2013-05-15"),    
    dateInput("end_date", "End Date:", value = "2014-06-15"),
    helpText("Note: please keep Start date at least 30 days earlier than End date"),
    tags$hr(), 
    sliderInput("model_order", "Polynomial order:", 
                min = 1,max = 20,step=1,value = 5),
    helpText("Note: be aware of over-fitting")
  ),
  mainPanel(
    h4("This application loads historical stock price data, and applies a polynomial fitting to it"), 

    p("Users are allowed to select a stock name, price type, time period and order of polynomial fitting
             on the left sidebar"),
    
    h4("Steps: "),
    p("Step 1: select a stock name"),
    p("Step 2: select price type. options: open, high, low, and close price"),
    p("Step 3: select start date. The format should be yyyy-mm-dd"),
    p("Step 4: select end date. The format should be yyyy-mm-dd.
           Please keep Start date at least 30 days earlier than End date"),
    p("Step 5: change the order of polynomial fitting. The range is 1 to 20"),
    
    h5("Enjoy!"),    
    
    br(),
    tags$hr(),
    plotOutput("plot")    
    
    
   
  )))
