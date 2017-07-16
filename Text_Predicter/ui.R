
library(shiny)

source("createNgrams.R")
source("Text Mining.R")

shinyUI(fluidPage(
  
  textInput("text", label = h3("Start Typing!"), value = "Enter text..."),
  
  hr(),
  fluidRow(column(3, verbatimTextOutput("value"))),
  fluidRow(column(3, verbatimTextOutput("predict.status")))
  
))
