library(quantmod)
library(dygraphs)

shinyServer(function(input, output) {
  
  dataInput <- reactive({
    
    prices <- getSymbols(input$symb, auto.assign = FALSE)
    market <- getSymbols("^GSPC", auto.assign = FALSE)
    dataPrices <- merge.xts(Ad(prices), Ad(market), join = "inner")

    
  })
  

  output$plot <- renderDygraph({
    
    prices <- dataInput()
    
    dygraph(Ad(prices)) %>%
      dyRangeSelector()
  })
})
