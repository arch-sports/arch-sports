library(googlesheets4)
library(tidyverse)
library(vtable)
library(data.table)
library(quarto)

# Set Working Directory
setwd(getSrcDirectory(function(){}))
month_test <- readline(prompt = 'Enter the test month: ')
month_test <- as.numeric(month_test)

# Import testing data
test_url <- 'https://docs.google.com/spreadsheets/d/1VzdlsDCA-X8HVKvUktG1A5XZNiq3IuOwX2Zbq8k6Iu0/edit?gid=2006047091#gid=2006047091'
test_sheet <- 'MASTER DATA TABLE'
file_name <- paste('./', year(Sys.Date()),'_', month_test, '_testing_data.csv', sep = '')

find_min <- c('PRO_AGILITY', 'SPLIT_10', 'SPRINT_20', 'SHUTTLE_300')
if (!file.exists(file_name)) {
  data <- read_sheet(test_url, sheet = test_sheet)
  data %>%
    mutate(YOUNG       = ifelse(data$AGE <= 13, 1, 0),
           OLD         = ifelse(data$AGE> 13 & data$AGE <= 18, 1, 0),
           ADULT       = ifelse(data$AGE > 18, 1, 0),
           DATE_MONTH  = month(DATE),
           NORDBORD    = rowMeans(cbind(data$NORDBORD_L, data$NORDBORD_R), na.rm = TRUE),
           PRO_AGILITY = PRO_AGILITY * -1,
           SPLIT_10    = SPLIT_10 * -1,
           SPRINT_20   = SPRINT_20 * -1,
           SHUTTLE_300 = SHUTTLE_300 * -1)
} else {
  data <- read.csv(file = file_name)
}

# Get last month's and this month's testing data
data_last    <- data[month(data$DATE) == month_test - 1,]
data_current <- data[month(data$DATE) == month_test,]

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

# Create variable list to exclude for summary table
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

for (name in names(summary_data)) {
  for (test in find_min) {
    if(length(grep(test, name)) != 0) {
      summary_data[[name]] <- -1 * summary_data[[name]]
    }
  }
}

for (name in names(data)) {
  if(name %in% find_min) {
    data[[name]] <- -1 * data[[name]]
  }
}

summary_data[summary_data == -Inf] <- NA
summary_data[summary_data == Inf] <- NA

write.csv(data,
          file = paste(year(Sys.Date()), '_', month_test, '_testing_data.csv', sep = ''),
          row.names = FALSE)

write.csv(summary_data, 
          file = paste(year(Sys.Date()), '_', month_test, '_summary_data.csv', sep = ''), 
          row.names = FALSE)

quarto_render('./Monthly_Report_Quarto.qmd',
              output_format = 'html',
              output_file = paste(year(Sys.Date()), '_', month_test, '_Monthly Report.html', sep = ""),
              execute_params = list(month = month_test))
