
library(shiny)

shinyUI(fluidPage(
  
  textInput("text", label = h3("Start Typing!"), value = "Enter text..."),
  
  hr(),
  fluidRow(column(3, verbatimTextOutput("value")))
  
))
