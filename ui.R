library(shiny)

shinyUI(fluidPage(
  titlePanel("CAPM for Desired Stock"),
  
  sidebarLayout(
    sidebarPanel(color = 'cadetblue4',
      helpText("Input a ticker and see the stock's regression to market and expected return."),
      
      textInput("symb", label = h4("Input a Valid Stock Ticker"), value = "GE"),
      dateRangeInput("date", label = h4("Input Date Range for Returns"), start = '2007-01-01', end = as.character(Sys.Date())),
      helpText("Date Format: [YYYY/MM/DD]")
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Market Regression", plotOutput("plot"),h4(textOutput('beta'))),
        tabPanel("Expected Return",  plotOutput("SML"), textOutput("eRmath"), helpText("[eR = rF + Beta(mR - rF)]"))
        
      )
  ))
))
