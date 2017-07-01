library(shiny)

source("Text Mining.R")

predictEnvironment <- init_Predictive_Model_Env()



shinyServer(function(input, output) {
  
  #temp test line
  #output$value <- renderPrint(paste(input$text,"some stuff",disk[[1]][1000]))
  output$value <- renderPrint(predictNextWords(input$text, predictEnvironment))
  
})
