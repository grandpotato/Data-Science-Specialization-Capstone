library(shiny)
library(stringi)
source("Text Mining.R")
print(paste0("start time:",Sys.time()))
predictEnvironment <- init_predictive_model_env()
print(paste0("environment ready time:",Sys.time()))


shinyServer(function(input, output) {
  
  #temp test line
  #output$value <- renderPrint(paste(input$text,"some stuff",disk[[1]][1000]))
  # output$value <- renderPrint(predictNextWords(gsub("[.]*([[:alnum:]]*\\s[[:alnum:]]*\\s[[:alnum:]]*)","\\1", stri_trim(input$text)), predictEnvironment)[[1]])
  # output$predict.status <- renderPrint(predictNextWords(gsub("[.]*([[:alnum:]]*\\s[[:alnum:]]*\\s[[:alnum:]]*)","\\1", stri_trim(input$text)), predictEnvironment)[[2]])
  
  output$value <- renderPrint(predict_next_words(paste(word(stri_trim(input$text), -4:-1)[!is.na(word(input$text, -4:-1))], collapse = ' '), predictEnvironment)[[1]])
  output$predict.status <- renderPrint(predict_next_words(paste(word(stri_trim(input$text), -4:-1)[!is.na(word(input$text, -4:-1))], collapse = ' '), predictEnvironment)[[2]])
})
