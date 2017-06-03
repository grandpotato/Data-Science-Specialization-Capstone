library(shiny)
source("Text Mining.R", local = TRUE)


#Well this stuff should go within the MakePredictMatrix function
kDataLocation <- "../../data/sampled/"
docs <- MakeTMMap(kDataLocation)
#also this should be done so it loads into a CSV

#This should be a class of a list of Matricies...I wouldn't really know how to do that....especially dynamic assignment of memory sizing or depth. questions for another time.

predictEnvironment <- init_Predictive_Model_Env(docs,BigramTokenizer)


shinyServer(function(input, output) {
  
  
  output$value <- renderPrint(predictNextWords(input$text, predictEnvironment))
  
})
