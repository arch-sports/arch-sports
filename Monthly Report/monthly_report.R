library(googlesheets4)
library(tidyverse)
library(vtable)
library(data.table)

# Set Working Directory
setwd(getSrcDirectory(function(){}))

# Import testing data
test_url <- 'https://docs.google.com/spreadsheets/d/1VzdlsDCA-X8HVKvUktG1A5XZNiq3IuOwX2Zbq8k6Iu0/edit?gid=2006047091#gid=2006047091'
test_sheet <- 'MASTER DATA TABLE'
file_name <- paste('./', year(Sys.Date()),'_', month(Sys.Date()), '_testing_data.csv', sep = '')

if (!file.exists(file_name)) {
  data <- read_sheet(test_url, sheet = test_sheet)
  data %>%
    mutate(YOUNG      = ifelse(data$AGE <= 13, 1, 0),
           OLD        = ifelse(data$AGE> 13 & data$AGE <= 18, 1, 0),
           ADULT      = ifelse(data$AGE > 18, 1, 0),
           DATE_MONTH = month(DATE),
           NORDBORD   = rowMeans(cbind(data$NORDBORD_L, data$NORDBORD_R), na.rm = TRUE)) %>%
    write.csv(file = file_name,
              row.names = FALSE)
} else {
  data <- read.csv(file = file_name)
}

find_min <- c('PRO_AGILITY', 'SPLIT_10', 'SPRINT_20', 'SHUTTLE_300')

# Add dummy column for whether athlete <= 13 years old


# Get last month's and this month's testing data
data_last    <- data[month(data$DATE) == month(Sys.Date()) - 1,]
data_current <- data[month(data$DATE) == month(Sys.Date()),]

# Get athletes who tested last month
athletes <- unique(data_current$NAME)

# Create function list for summary table
func_list <- list(
  Current = ~last(.x, na_rm = TRUE),
  Best = ~max(.x, na.rm = TRUE),
  Best_change = ~(last(.x, na_rm = TRUE) - max(.x, na.rm = TRUE)),
  Best_perc = ~((last(.x, na_rm = TRUE) - max(.x, na.rm = TRUE))/max(.x, na.rm = TRUE)),
  Last = ~nth(.x, -2, na_rm = TRUE),
  Last_change = ~(last(.x, na_rm = TRUE) - nth(.x, -2, na_rm = TRUE)),
  Last_perc = ~((last(.x, na_rm = TRUE) - nth(.x, -2, na_rm = TRUE))/nth(.x, -2, na_rm = TRUE))
)

# Create variable list for summary table
var_list <- c(
  'NAME',
  'DATE',
  'DATE_MONTH',
  'AGE',
  'CMJ_FLIGHT',
  'IMTP_PEAK',
  'YOUNG',
  'OLD',
  'ADULT',
  'DSI',
  'NORDBORD_DIFF',
  'NORDBORD_L',
  'NORDBORD_R'
)

summary_data <- data %>%
  #select(-YOUNG, - OLD, -ADULT) %>%
  group_by(NAME) %>%
  summarise_at(
    .vars = names(data)[!names(data) %in% var_list],
    .funs = func_list
  ) %>% 
  filter(NAME %in% athletes)

summary_data[summary_data == -Inf] <- NA
summary_data[summary_data == Inf] <- NA

write.csv(summary_data, 
          file = paste(year(Sys.Date()), '_', month(Sys.Date()), '_summary_data.csv', sep = ''), 
          row.names = FALSE)
