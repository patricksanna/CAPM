library(quantmod)
library(ggplot2)

shinyServer(function(input, output) {
  
  dataInput <- reactive({
    
    prices <- getSymbols(input$symb, auto.assign = FALSE)
    market <- getSymbols("^GSPC", auto.assign = FALSE)
    dataPrices <- merge.xts(Ad(prices), Ad(market), join = "inner")
    stock <- dataPrices[,1]
    mm <- dataPrices [,2]
    sReturns <- Delt(stock)[-1]
    mmReturns <- Delt(mm)[-1]
    reg <- lm((sReturns) ~ (mmReturns))
    rMM <- mean(mmReturns)*365
    rF <- .0047
      ##Risk Free rate as of 11/06/2015 for 1 year bond (maybe a way to make dynamic?)
    rI <- rF +  reg$coefficients[2] * (rMM - rF)
    theData <- list(x = data.frame(cbind(as.vector(sReturns), as.vector(mmReturns))), y = reg, z = rI, rF = rF, rMM = rMM)

  })
  

  output$plot <- renderPlot({
    
    theData <- dataInput()
    qplot(theData$x[,2], theData$x[,1], ylab = "Asset Returns", xlab = "Market Returns") +    
    geom_abline(intercept =theData$y$coef[1], slope = theData$y$coef[2]) +
    annotate("text", x = -.075, y = .19, label = "Regression:")
  })
  
  output$eRmath <- renderText({
    theData <- dataInput()
    return(paste("Expected Return:", round(theData$z*100, 2), "%", " = ", round(theData$rF*100, 2), "%", "+", round(theData$y$coefficients[2], 2), "(", round(100*theData$rMM,2), "%", "-", round(100*theData$rF, 2), "%",")"))
  })
  
  output$SML <- renderPlot({
    theData <- dataInput()
      qplot(theData$y$coef[2], theData$z, ylim = c(0, .20), xlim = c(0, 2), ylab = "Expected Return", xlab = "Beta") +
      geom_abline(intercept =theData$rF, slope = ((theData$z - theData$rF)/theData$y$coef[2])) +
       annotate("text", x = .25, y = .19, label = "Expected Return:")


  })
})
