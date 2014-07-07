
library(shiny)

shinyUI(fluidPage(
  titlePanel("Stock Analyzer"),  
  fluidRow(
    column(4,"4"),
    column(4, offset = 4,"4 offset 4")      
  ),
#   fluidRow(
#     column(3, offset = 3,"3 offset 3"),
#     column(3, offset = 3,"3 offset 3")),
#   navlistPanel(
#     "Header A",
#     tabPanel("Component 1"),
#     tabPanel("Component 2"),
#     "Header B",
#     tabPanel("Component 3"),
#     tabPanel("Component 4"),
#     "-----",
#     tabPanel("Component 5")
#   ),
  sidebarLayout(position="left",
      sidebarPanel( 
        textInput("queries", "tw_queries:", value = "YHOO"),        
        tags$hr(),
        textInput("stock_name", "Stock:", value = "YHOO"),        
        tags$hr(),
        selectInput("price_type", "Price type:",
                list("Open price" = 2, 
                     "High price" = 3, 
                     "Low price" = 4,
                     "Close price" = 5)),
        tags$hr(), 
        dateInput("start_date", "Start Date:", value = "2013-05-15"),
        dateInput("end_date", "End Date:", value = "2014-06-15"),
        tags$hr(), 
        sliderInput("model_order", "Polynomial order:", 
                min = 1,max = 20,step=1,value = 5)     
  ),
  mainPanel(
    h6("Episode IV", align = "center"),
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
    plotOutput("plot"),
    tableOutput("view")
    
#     h3("URL components"),
#     verbatimTextOutput("urlText"),
#       
#     h3("Parsed query string"),
#     verbatimTextOutput("queryText"),
#         
#     h3("clientData values"),
#     verbatimTextOutput("clientdataText"),
#     plotOutput("myplot"),
#     
#     p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph", style = "font-family: 'times'; font-si16pt"),
#     strong("strong() makes bold text."),
#     em("em() creates italicized (i.e, emphasized) text."),
#     br(),
#     code("code displays your text similar to computer code"),
#     div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
#     br(),
#     p("span does the same thing as div, but it works with",
#       span("groups of words", style = "color:blue"),
#       "that appear inside a paragraph."),
#     img(src="bigorb.png", height = 400, width = 400)
    
  
 ))
)
 )
