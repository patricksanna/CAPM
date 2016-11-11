## Patrick's version of the CAPM app
library(quantmod)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  titlePanel("Factor Model of Desired Stock"),
  
  sidebarLayout(
    sidebarPanel(color = 'cadetblue4',
                 helpText("Input a ticker and see the stock's regression to market and expected return."),
                 
                 textInput("symb", label = h4("Input a Valid Stock Ticker"), value = "GE"),
                 dateRangeInput("date", label = h4("Input Date Range for Returns"), start = '2007-01-01', end = as.character(Sys.Date())),
                 helpText("Date Format: [YYYY/MM/DD]")
    ),
    
    mainPanel(
	    ## This sets up two panels -- I think we only want one, the 3D scatterplot.  Should we remove the second panel?  
      tabsetPanel(
        tabPanel("Market Regression", plotlyOutput("plot"),h4(textOutput('beta'))),
        tabPanel("Expected Return",  plotOutput("SML"), textOutput("eRmath"), helpText("[eR =Beta(mR)]"))
        
      )
    ))
)




server <- function(input, output) {
  dataInput <- reactive({
    validate(
      need(input$symb != "", "Please type a ticker"),
      need(input$date[1] < input$date[2], 'Start date is either missing or is later than end date.')
    )
    
    prices <- getSymbols(input$symb, from = input$date[1], to = input$date[2], auto.assign = FALSE)
    market <- getSymbols("^GSPC", from = input$date[1], to = input$date[2], auto.assign = FALSE)
    
    #options(download.file.method="curl")
    #t <- getSymbols.FRED("TB1YR", auto.assign = FALSE)
    ## Lets also pull crude oil prices from FRED
    oil <- getSymbols("USO", auto.assign = FALSE)
    dataPrices <- merge.xts(Ad(prices), Ad(market), Ad(oil), join = "inner")
    
    stock <- dataPrices[,1]
    mm <- dataPrices [,2]
    oil <- dataPrices[,3]
    sReturns <- Delt(stock)[-1]
    mmReturns <- Delt(mm)[-1]
    oilReturns <- Delt(oil)[-1]
    data.merged <- merge.xts(mmReturns, oilReturns, sReturns, join="inner")
    names(data.merged) <- c("market", "oil", "stock")


    ## wont work yet -- vectors of different lengths -- merge before regression
    reg <- lm((data.merged$stock) ~ (data.merged$market) + data.merged$oil)
    rMM <- mean(mmReturns)*365
    #rF <- as.vector(t[length(t)])/100
    rI <-   reg$coefficients[2] * (rMM)
    theData <- list(x = data.frame(cbind(as.vector(data.merged$stock), as.vector(data.merged$market), as.vector(data.merged$oil))), y = reg, z = rI[length(rI)], rMM = rMM)

  })
  

  output$plot <- renderPlotly({
    
      theData <- dataInput()
      ## plotly 3D scatterplot
      forPlotly <- theData$x
      names(forPlotly) <- c("stock", "market", "oil")
      p <- plot_ly(forPlotly, z = ~stock, y = ~market, x = ~oil, type = 'scatter3d', mode = 'markers',
        opacity = 1)#, line = list(width = 6, color = ~color, reverscale = FALSE))
    ## qplot(theData$x[,2], theData$x[,1], ylab = "Asset Returns", xlab = "Market Returns") +    
    ##   geom_abline(intercept =theData$y$coef[1], slope = theData$y$coef[2], color = "grey23") +
    ##   labs(title = "Market Regression")+
    ##   geom_point(color = "cadetblue4") 
      
    
  })
  
  output$eRmath <- renderText({
    theData <- dataInput()
    return(paste("Expected Return:", round(theData$z*100, 2), "%", " = ", round(100, 2), "%", "+", round(theData$y$coefficients[2], 2), "(", round(100*theData$rMM,2), "%", "-", round(100, 2), "%",")"))
  })
  
  output$SML <- renderPlot({
    theData <- dataInput()
    qplot(theData$y$coef[2], theData$z, ylim = c(0, .15), xlim = c(0, 2), ylab = "Expected Return", xlab = "Beta") +
      geom_abline(intercept =0, slope = ((theData$z)/theData$y$coef[2])) +
      labs(title = "Security Market Line") +
      geom_point(color = "cadetblue4", size = 3.5)

  })
  
  output$beta <- renderText({
    theData <- dataInput()
    paste("    Beta:  ", round(theData$y$coef[2], 2))
  
  })
}


shinyApp(ui = ui, server = server)
