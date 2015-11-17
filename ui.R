library(shiny)

shinyUI(fluidPage(
  titlePanel("Expected Return for a Stock"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Input a ticker and see the stock's regression to market and expected return."),
      
      textInput("symb", label = h3("Input a Valid Stock Ticker"), value = "GE")
      
      ),  
    sidebarPanel(
      textOutput("eRmath"),
      helpText("[eR = rF + Beta(mR - rF)]")
    
      )),

    mainPanel(
      plotOutput("plot")
  )
))
