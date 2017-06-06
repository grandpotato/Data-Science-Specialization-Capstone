library(shiny)
library(jsonlite)

bigram <- fromJSON("2-gram.json")
trigram <- fromJSON("3-gram.json")
quadgram <- fromJSON("4gram.json")
#predictEnvironment <- init_Predictive_Model_Env(docs,BigramTokenizer)


shinyServer(function(input, output) {
  
  
  #temp test line
  output$value <- renderPrint(paste(input$text,"some stuff",disk[[1]][1000]))
  #output$value <- renderPrint(predictNextWords(input$text, predictEnvironment))
  
})
