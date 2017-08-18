library(shiny)
library(stringi)
library(stringr)
source("Text Mining.R")

#how to I make a loading screen? So nothing starts until the following statement is complete?
predictEnvironment <- init_predictive_model_env()


shinyServer(function(input, output) {
  
  
  #Fix up the block below

  MAX_WORD_INPUT <- 5
  
  num_words_to_get <- reactive({
    num_words <- CountWords(input$text)
    result <- MAX_WORD_INPUT
    
    if(num_words < MAX_WORD_INPUT) {
      result <- num_words
    }
    
    result
  })
  
  last_n_words <- reactive({
    word(stri_trim(input$text), start = -1* num_words_to_get(), end = -1)
  })
  
  a_prediction <- reactive({
    temp <- predict_next_words(last_n_words, predictEnvironment)
    paste(temp[[1]]$Predict, collapse = " ")
  })
  
  output$predict.status <- renderPrint(predict_next_words(last_n_words(), predictEnvironment)[[2]])
  output$value <- renderPrint(predict_next_words(last_n_words(), predictEnvironment)[[1]]$Predict)
})
