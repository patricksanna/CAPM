library(quantmod)
library(dygraphs)

shinyServer(function(input, output) {
  
  dataInput <- reactive({
    
    prices <- getSymbols(input$symb, auto.assign = FALSE)
    
  })
  

  output$plot <- renderDygraph({
    
    prices <- dataInput()
    
    dygraph(Ad(prices)) %>%
      dyRangeSelector()
  })
})