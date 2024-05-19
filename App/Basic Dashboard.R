using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

using("shiny", "shinydashboard", "googlesheets4", "tibbletime", "dplyr", "tidyverse", "corrplot", "lubridate", "bslib", "plotly")


library(shiny)
library(shinydashboard)
library(googlesheets4)
library(tibbletime)
library(dplyr)
library(data.table)
library(tidyverse)
library(corrplot)
library(lubridate)
library(bslib)
library(plotly)

test.data <- read_sheet("https://docs.google.com/spreadsheets/d/1VzdlsDCA-X8HVKvUktG1A5XZNiq3IuOwX2Zbq8k6Iu0/", "MASTER DATA TABLE")
train.data <- read_sheet("https://docs.google.com/spreadsheets/d/1MY7WtWH4ba4npaUHOYmZ4OotMZwp8k_jVTCBhqOSeyU/", "TRAINING DATA")
test.data <- cbind(Location = "Ballantyne", test.data)
test_columns = names(test.data)[c(-1, -2, -3, -4)]
train_athletes = sort(unique(train.data$NAME))

#Create header
# dropdown_message = dropdownMenu(type = "messages",
#                                 messageItem(
#                                   from = "Sales Dept",
#                                   message = "Sales are steady this month."
#                                 ),
#                                 messageItem(
#                                   from = "New User",
#                                   message = "How do I register?",
#                                   icon = icon("question"),
#                                   time = "13:45"
#                                 ),
#                                 messageItem(
#                                   from = "Support",
#                                   message = "The new server is ready.",
#                                   icon = icon("life-ring"),
#                                   time = "2014-12-01"
#                                 )
#                               )
# 
# notifications = dropdownMenu(type = "notifications",
#                              notificationItem(
#                                text = "5 new users today",
#                                icon("users")
#                              ),
#                              notificationItem(
#                                text = "12 items delivered",
#                                icon("truck"),
#                                status = "success"
#                              ),
#                              notificationItem(
#                                text = "Server load at 86%",
#                                icon = icon("exclamation-triangle"),
#                                status = "warning"
#                              )
#                             )
# 
# tasks = dropdownMenu(type = "tasks", badgeStatus = "success",
#                      taskItem(value = 90, color = "green",
#                               "Documentation"
#                      ),
#                      taskItem(value = 17, color = "aqua",
#                               "Project X"
#                      ),
#                      taskItem(value = 75, color = "yellow",
#                               "Server deployment"
#                      ),
#                      taskItem(value = 80, color = "red",
#                               "Overall project"
#                      )
#                   )

header = dashboardHeader(title = 'Architech Sports')

#Create sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Location Stats", icon = icon("th"), tabName = "locationstats"),
    menuItem("Training Data", icon = icon("dashboard"), tabName = "training")
  )
)

#Create body
body = dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidPage(
              fluidRow(
                box(plotOutput("plot1"), width = 9, height = '50vh'),
                
                box(
                  width = 3,
                  height = '50vh',
                  selectInput(inputId = "test_choice", label = "Select test to view", choices = sort(test_columns)),
                  dateRangeInput("test_date", 
                                 label = "Select date range", 
                                 max = Sys.Date(), 
                                 start = trunc.Date(Sys.Date(), units = "months"), 
                                 end = Sys.Date())
                )
              ),
              fluidRow(
                box(plotOutput("plot2"), height = '50vh'),
                
                box(
                  title = "Inputs", status = "warning",
                  selectInput(inputId = "test_choice2", label = "Select test to view", choices = test_columns),
                  selectInput(inputId = "athlete_choice", label = "Select athlete to view", choices = sort(unique(test.data$Name))),
                  dateRangeInput("test_date2", 
                                 label = "Select date range", 
                                 max = Sys.Date(), 
                                 start = trunc.Date(Sys.Date(), units = "months"), 
                                 end = Sys.Date())
                )
              )
            )
    ),
    
    # Second tab content
    tabItem(tabName = "locationstats",
            fluidRow(
              box(
                width = 2,
                title = 'Location by Test',
                selectInput(inputId = 'location_boxplot_test', label = 'Select test to view', choices = test_columns)
              ),
              box(
                width = 10,
                plotOutput('location_boxplot')
              )
            ),
            fluidRow(
              column(
                height = 'auto',
                width = 12,
                box(
                  width = NULL,
                  height = 'auto',
                  title = "Correlation Matrix",
                  plotOutput("plot3", height = '1000')
                )
              )
              
            )
    ),
    
    #Third tab content
    tabItem(tabName = "training",
            fluidRow(
              column(
                width = 2,
                title = "Inputs",
                selectInput("train_athlete_choice", label = "Select athlete to view", choices = train_athletes),
                selectInput("train_choice", label = "Select lift to view", choices = ""),
                dateRangeInput("train_date", 
                               label = "Select date range", 
                               max = Sys.Date(), 
                               start = trunc.Date(Sys.Date(), units = "months"), 
                               end = Sys.Date())
              ),
              column(
                width = 10,
                div(style = 'height:100%'),
                box(
                  height = 'auto',
                  width = NULL,
                  status = 'primary',
                  solidHeader = TRUE,
                  title = "Estimated 1RM",
                  plotOutput("plot4", '100%')
                ),
                box(
                  height = 'auto',
                  width = NULL,
                  status = 'primary',
                  title = "Volume",
                  plotOutput("plot5", '100%')
                ),
                box(
                  height = 'auto',
                  width = NULL,
                  title = "Tonnage",
                  plotOutput("plot6", '100%')
                )
              )
            )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  
  observeEvent(input$train_athlete_choice, {
    choices <- arrange(unique(filter(train.data, NAME == input$train_athlete_choice)['EXERCISE']), .by_group = FALSE)
    updateSelectInput(session, "train_choice", choices = choices)
  })
  
  output$plot1 <- renderPlot({
    data_time <- as_tbl_time(na.omit(test.data), index = Date)
    filtered_data <- filter_time(data_time, time_formula = input$test_date[1] ~ input$test_date[2])
    data <- density(filtered_data[[input$test_choice]])
    plot(data, main="Distribution of Test Scores")
  })
  
  output$plot2 <- renderPlot({
    data_test <- filter(test.data, Name == input$athlete_choice)
    data_with_dates <- data.table("Date" = data_test[["Date"]], "Test" = data_test[[input$test_choice2]])
    tryCatch(
      {
        ggplot(data_with_dates, aes(x = Date, y = Test)) +
          geom_line() +
          geom_point()
      },
      error = function(e) {""}
    )
    
  })
  
  #Location Stats Plots
  
  output$plot3 <- renderPlot({
    res <- cor(as.data.frame(test.data[c(-1,-2,-3, -4)]), use = "pairwise.complete.obs", method = "pearson")
    corrplot(res, method = "color", type = "upper")
  })
  
  output$location_boxplot <- renderPlot({
    column <- test.data[[input$location_boxplot_test]]
    ggplot(data = test.data, mapping = aes(x = Location, y = .data[[input$location_boxplot_test]])) +
      geom_boxplot(fill = "red",color = "black", alpha = 0.3) +
      geom_jitter(aes(color = 'blue'), alpha=0.2) +
      labs(x = "Location") +
      ggtitle("Summary of Tests by Location") +
      guides(color = "none") +
      theme_minimal() +
      coord_cartesian(ylim = quantile(column, c(0, 0.99), na.rm = TRUE))
  })
  
  output$plot4 <- renderPlot({
    data_time <- as_tbl_time(na.omit(train.data), index = DATE)
    filtered_data <- filter_time(data_time, time_formula = input$train_date[1] ~ input$train_date[2])
    filtered_data_lift <- filter(filtered_data, EXERCISE == input$train_choice, NAME == input$train_athlete_choice)
    data <- select(filtered_data_lift, c("DATE", "VOLUME", "TONNAGE", "EST. 1RM"))
    tryCatch(
      {
        ggplot(data, mapping = aes(x = DATE, y = .data[['EST. 1RM']])) +
          geom_line() + 
          geom_point() +
          geom_label(
            aes(label = .data[['EST. 1RM']]),
            nudge_x = 0.25,
            nudge_y = 0.25
          )
      },
      error = function(e) {""}
    )
    
  })
  
  output$plot5 <- renderPlot({
    data_time <- as_tbl_time(na.omit(train.data), index = DATE)
    filtered_data <- filter_time(data_time, time_formula = input$train_date[1] ~ input$train_date[2])
    filtered_data_lift <- filter(filtered_data, EXERCISE == input$train_choice, NAME == input$train_athlete_choice)
    data <- select(filtered_data_lift, c("DATE", "VOLUME", "TONNAGE", "EST. 1RM"))
    tryCatch(
      {
        ggplot(data, mapping = aes(x = DATE, y = VOLUME)) +
          geom_line() + 
          geom_point() +
          geom_label(
            aes(label = VOLUME),
            nudge_x = 0.25,
            nudge_y = 0.25
          )
      },
      error = function(e){""}
    )
    
  })
  
  output$plot6 <- renderPlot({
    data_time <- as_tbl_time(na.omit(train.data), index = DATE)
    filtered_data <- filter_time(data_time, time_formula = input$train_date[1] ~ input$train_date[2])
    filtered_data_lift <- filter(filtered_data, EXERCISE == input$train_choice, NAME == input$train_athlete_choice)
    data <- select(filtered_data_lift, c("DATE", "VOLUME", "TONNAGE", "EST. 1RM"))
    tryCatch(
      {
        ggplot(data, mapping = aes(x = DATE, y = TONNAGE)) +
          geom_line() + 
          geom_point() +
          geom_label(
            aes(label = TONNAGE),
            nudge_x = 0.25,
            nudge_y = 0.25
          )
      },
      error = function(e){""}
    )
    
  })
  
}

shinyApp(ui, server)