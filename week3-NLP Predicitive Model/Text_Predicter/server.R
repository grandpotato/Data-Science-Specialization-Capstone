library(shiny)
library(stringi)
source("Text Mining.R")

predictEnvironment <- init_Predictive_Model_Env()



shinyServer(function(input, output) {
  
  #temp test line
  #output$value <- renderPrint(paste(input$text,"some stuff",disk[[1]][1000]))
  # output$value <- renderPrint(predictNextWords(gsub("[.]*([[:alnum:]]*\\s[[:alnum:]]*\\s[[:alnum:]]*)","\\1", stri_trim(input$text)), predictEnvironment)[[1]])
  # output$predict.status <- renderPrint(predictNextWords(gsub("[.]*([[:alnum:]]*\\s[[:alnum:]]*\\s[[:alnum:]]*)","\\1", stri_trim(input$text)), predictEnvironment)[[2]])
  
  output$value <- renderPrint(predictNextWords(paste(word(stri_trim(input$text), -3:-1)[!is.na(word(input$text, -3:-1))], collapse = ' '), predictEnvironment)[[1]])
  output$predict.status <- renderPrint(predictNextWords(paste(word(stri_trim(input$text), -3:-1)[!is.na(word(input$text, -3:-1))], collapse = ' '), predictEnvironment)[[2]])
})
