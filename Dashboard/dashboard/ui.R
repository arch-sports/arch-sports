#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)
library(plotly)
library(DT)

# Define UI for application that draws a histogram
fluidPage(

  # Application title
  titlePanel("Architech Sports - Ballantyne"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file",
                label   = "Select data file",
                accept  = c(".csv",
                            ".xlsx")),
      
      selectInput(inputId  = "athlete",
                  label    = "Choose athlete",
                  selected = "All",
                  choices  = "All"),
      
      h2("Enter new test"),
      
      uiOutput(outputId = "inputs"),
      
      actionButton(inputId = "addData",
                   label   = "Add data")
    ),

    mainPanel(
      plotlyOutput("speedPlot"),
      
      dataTableOutput("speedTable")
    )
  )
)
