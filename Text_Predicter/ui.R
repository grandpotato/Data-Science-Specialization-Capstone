
library(shiny)
library(shinydashboard)

source("createNgrams.R")
source("Text Mining.R")


shinyUI(dashboardPage(
  dashboardHeader(title = "Text Predictor"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("PREDICTOR", tabName = "predictor", icon = icon("gear")),
      menuItem("ABOUT", tabName = "about", icon = icon("info"))
    )
    
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "predictor",
              textInput("text", label = h3("Start Typing!")),
              h3("Text matched on"),
              fluidRow(column(12, verbatimTextOutput("predict.status"))),
              h3("Predicted Text"),
              fluidRow(column(12, verbatimTextOutput("value")))
      ),
      
      tabItem(tabName = "about",
              strong("Author:"), p("Kevin Ho"),
              strong("Date:"), p("27/07/17"),
              strong("Details"), p("This is an implementation of a text prediction application for Coursera Data Science Specialization Capstone Project.")
        
      )
    )
    

  )
  
))
