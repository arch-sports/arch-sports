#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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

path  <- "C:/Users/Caitie Mayo/OneDrive - Architech Sports and Physical Therapy/Athlete Workout Cards/Athlete Workout Cards - Ballantyne/Data"
  
mean_agg <- function(x) {
  mean(x, na.rm = TRUE)
}
  
min_agg <- function(x) {
  if (class(x) != "factor") {
    min(x, na.rm = TRUE)
  } else {
    return(NULL)
  }
}



# Define server logic required to draw a histogram
function(input, output, session) {
  
  create_row <- function(x) {
    tib <- tibble(col = 1)

    for (name in names(x)) {
      tib <- cbind(tib, tibble("{name}" := input[[name]]))
      print(tib)
    }
  
    tib$col <- NULL
    return(tib)
  }
  
  # Import file into speed variable
  speed <- reactive({
    file <- input$file
    if (is.null(file)) {
      return(NULL)
    } else {
      read_excel(path = as.character(file$datapath)) %>%
        mutate(Date = as.Date(Date),
               Run  = as.factor(Run)) %>%
        as.data.frame()
    }
  })
  
  # Create inputs for new data based on input file
  output$inputs <- renderUI({
    tmp <- speed()
    
    lapply(names(tmp), function(name) {
      switch(class(tmp[[name]]),
             "Date"      = dateInput(inputId = name,
                                     label   = str_to_title(name)),
             "numeric"   = numericInput(inputId = name,
                                      label   = str_to_title(name),
                                      value   = 0),
             "factor"    = textInput(inputId = name,
                                     label   = str_to_title(name)),
             "character" = textInput(inputId = name,
                                     label   = str_to_title(name)))
    })
  })
  
  # newData <- reactive({
  #   tmp <- speed()[is.na(speed()), ]
  #   
  #   tib <- tibble(x = 1)
  #   
  #   for (name in names(tmp)) {
  #     tib <- cbind(tib, tibble("{name}" := input[[name]]))
  #   }
  #   print(tib)
  #   tib$x <- NULL
  #   tib
  # })
  
  newData <- reactive({
    speed()[is.na(speed()), ]
  })
  
  observeEvent(input$addData, {
    new_row <- newData() %>%
      create_row()
    
    
    next_row <- newData() %>%
      add_row(new_row)
    
  })
  
  
  
  observe({
    updateSelectInput(inputId  = "athlete",
                      selected = "All",
                      choices  = rbind("All", sort(unique(speed()$Athlete))))
  })
  
  data <- reactive({
    if (input$athlete != "All") {
      rbind(speed()[speed()$Athlete == input$athlete, ], newData())
    } else {
      rbind(speed(), newData())
    }
  })

  output$speedPlot <- renderPlotly({
    if (!is.null(speed())) {
      if (input$athlete != "All") {
        plot <- speed() %>%
          aggregate(by = list(speed()$Athlete, speed()$Date), FUN = min_agg) %>%
          select(-c(Athlete, Run, Group.2)) %>%
          filter(Group.1 == input$athlete) %>%
          ggplot(mapping = aes(x = Date)) +
          geom_line(mapping = aes(y = Split),
                    color = 'red') +
          geom_point(mapping = aes(y = Split),
                     color = 'red') +
          geom_line(mapping = aes(y = Sprint),
                    color = 'blue') +
          geom_point(mapping = aes(y = Sprint),
                     color = 'blue') +
          ylim(0, NA) +
          labs(
            title = "Average 20-Yd Sprint/10-Yd Split Times",
            x = "Date",
            y = "Time"
          ) 
        
        ggplotly(plot)
      } else {
        plot <- speed() %>%
          aggregate(by = list(speed()$Athlete, speed()$Date), FUN = min_agg) %>%
          select(-c(Athlete, Run, Group.2)) %>%
          ggplot(mapping = aes(x = Date, color = Group.1)) +
          geom_line(mapping = aes(y = Split)) +
          geom_point(mapping = aes(y = Split)) +
          geom_line(mapping = aes(y = Sprint)) +
          geom_point(mapping = aes(y = Sprint)) +
          ylim(0, NA) +
          labs(
            title = "Average 20-Yd Sprint/10-Yd Split Times",
            x = "Date",
            y = "Time"
          )
        
        ggplotly(plot)
      }
    }
  })
  
  # Create data table
  output$speedTable <- renderDataTable({
    datatable(data(), 
              rownames = FALSE,
              filter   = "top",
              extensions = c('Buttons', 'RowGroup', 'Scroller'),
              options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                rowGroup = list(dataSrc = 0),
                deferRender = TRUE,
                scroller = TRUE,
                scrollY = 200
              ))
  })

}
