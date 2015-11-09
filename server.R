library(quantmod)

shinyServer(function(input, output) {
  
  dataInput <- reactive({
    
    prices <- getSymbols(input$symb, auto.assign = FALSE)
    market <- getSymbols("^GSPC", auto.assign = FALSE)
    dataPrices <- merge.xts(Ad(prices), Ad(market), join = "inner")
    stock <- dataPrices[,1]
    mm <- dataPrices [,2]
    sReturns <- Delt(stock)[-1]
    mmReturns <- Delt(mm)[-1]
    coVar <- lm(as.vector(sReturns) ~ as.vector(mmReturns))
    beta <- coVar/ sd(mmReturns)^2
    rM <- mean(mmReturns)
    rF <- .047
      ##Risk Free rate as of 11/06/2015
    rI <- rF + beta * (rM - rF)
      ##CAPM plot needed to show risk and expected return of the asset.
    theData <- list(x = data.frame(cbind(as.vector(sReturns), as.vector(mmReturns))), y = reg)
  })
  

  output$plot <- renderPlot({
    
    theData <- dataInput(beta, rI)
   ## need to label the axes below and add the equation to the table, also may want to use ggplot ----
    plot(theData$x[,2], theData$x[,1])
    abline(theData$y$coef[1], theData$y$coef[2])
  })
})
