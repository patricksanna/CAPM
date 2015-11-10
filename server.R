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
    rMM <- sapply(mmReturns, mean)*100
    rF <- .0047
      ##Risk Free rate as of 11/06/2015 for 1 year bond (maybe a way to not hardcode?)
    rI <- rF +  reg$coefficients * (rMM - rF)
    theData <- list(x = data.frame(cbind(as.vector(sReturns), as.vector(mmReturns))), y = reg, z = rI)
  })
  

  output$plot <- renderPlot({
    
    theData <- dataInput()
   ## need to label the axes below and add the equation to the table, also may want to use ggplot ----
    plot(theData$x[,2], theData$x[,1])
    abline(theData$y$coef[1], theData$y$coef[2])
    qplot(theData$y$coef[2], theData$z[2], ylim = c(0, .20), xlim = c(0, 2), ylab = "Expected Return", xlab = "Beta")
      ##Possibly add risk free intercept and the SML. Also need smaller intervals for E[R]
      ##Would a second plot for reg be redundant? 
  })
})
