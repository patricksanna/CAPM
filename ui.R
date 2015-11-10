library(shiny)

shinyUI(fluidPage(
  titlePanel("Expected Return for a stock"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Input a ticker and see the stock's chart."),
      
      textInput("symb", label = h3("Input a Valid Stock Ticker"), value = "GE") 
    ),

    mainPanel(plotOutput("plot"))
  )
))
