library(googlesheets4)
library(tidyverse)
library(vtable)
library(data.table)

# Set Working Directory
setwd(getSrcDirectory(function(){}))

# Import testing data
test_url <- 'https://docs.google.com/spreadsheets/d/1VzdlsDCA-X8HVKvUktG1A5XZNiq3IuOwX2Zbq8k6Iu0/edit?gid=2006047091#gid=2006047091'
test_sheet <- 'MASTER DATA TABLE'
data <- read_sheet(test_url, sheet = test_sheet)

# Add dummy column for whether athlete <= 13 years old
data <- data %>%
  mutate(Young = ifelse(data$`Age At Testing` <= 13, 1, 0),
         Old   = ifelse(data$`Age At Testing` > 13 & data$`Age At Testing` <= 18, 1, 0),
         Adult = ifelse(data$`Age At Testing` > 18, 1, 0),
         Month = month(Date)) 

# Get last month's and this month's testing data
data_last    <- data[month(data$Date) == month(Sys.Date()) - 1,]
data_current <- data[month(data$Date) == month(Sys.Date()),]

func_list <- list(
  Best = ~max(.x, na.rm = TRUE),
  Current = ~last(.x, na_rm = TRUE),
  Last = ~nth(.x, -2, na_rm = TRUE),
  Best_change = ~(max(.x, na.rm = TRUE) - last(.x, na_rm = TRUE)),
  Best_perc = ~((max(.x, na.rm = TRUE) - last(.x, na_rm = TRUE))/max(.x, na.rm = TRUE)),
  Last_change = ~(nth(.x, -2, na_rm = TRUE) - last(.x, na_rm = TRUE)),
  Last_perc = ~((nth(.x, -2, na_rm = TRUE) - last(.x, na_rm = TRUE))/nth(.x, -2, na_rm = TRUE))
)

var_list <- c(
  'Name',
  'Date',
  'Month',
  'Age At Testing',
  'CMJ - Flight',
  'IMTP Peak',
  'Young',
  'Old',
  'Adult',
  'DSI',
  'Nordbord % Diff'
)

summary_data <- data %>%
  select(-Young, - Old, -Adult) %>%
  group_by(Name) %>%
  summarise_at(
    .vars = names(data)[!names(data) %in% var_list],
    .funs = func_list
  )

write.csv(summary_data, 
          file = paste(year(Sys.Date()), '_', month(Sys.Date()), '_Monthly_Report.csv', sep = ''), 
          row.names = FALSE)
