library(quantmod)
library(dygraphs)

shinyServer(function(input, output) {
  
  dataInput <- reactive({
    
    prices <- getSymbols(input$symb, auto.assign = FALSE)
    market <- getSymbols("^GSPC", auto.assign = FALSE)
    dataPrices <- merge.xts(Ad(prices), Ad(market), join = "inner")
    stock <- dataPrices[,1]
    mm <- dataPrices [,2]
    sReturns <- Delt(stock)[-1]
    mmReturns <- Delt(mm)[-1]
    lm(as.vector(sReturns) ~ as.vector(mmReturns))
      ##CAPM plot needed to show risk and expected return of the asset.
  })
  

  output$plot <- renderDygraph({
    
    prices <- dataInput()
    
    dygraph(Ad(prices)) %>%
      dyRangeSelector()
  })
})
