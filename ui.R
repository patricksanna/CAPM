library(shiny)

shinyUI(fluidPage(
  titlePanel("CAPM for Desired Stock"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Input a ticker and see the stock's regression to market and expected return."),
      
      textInput("symb", label = h3("Input a Valid Stock Ticker"), value = "GE")
      
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Market Regression", plotOutput("plot")),
        tabPanel("Expected Return",  plotOutput("SML"), textOutput("eRmath"), helpText("[eR = rF + Beta(mR - rF)]"))
        
      )
  ))
))
