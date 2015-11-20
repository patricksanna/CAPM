library(shiny)

shinyUI(fluidPage(
  titlePanel("CAPM for Desired Stock"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Input a ticker and see the stock's regression to market and expected return."),
      
      textInput("symb", label = h4("Input a Valid Stock Ticker"), value = "GE"),
      dateInput("sdate", label = h5("Input a Start Date for Returns"), value = '2007-01-01'),
      dateInput("edate", label = h5("Input an End Date for Returns (Leave Blank for Today's)"), value = 'today'),
      helpText("Date Format: [YYYY/MM/DD]")
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Market Regression", plotOutput("plot")),
        tabPanel("Expected Return",  plotOutput("SML"), textOutput("eRmath"), helpText("[eR = rF + Beta(mR - rF)]"))
        
      )
  ))
))
