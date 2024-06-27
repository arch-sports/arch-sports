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
  write.csv(data,
            file = file_name,
            row.names = FALSE)
} else {
  data <- read.csv(file = file_name)
}
names(data) <- str_replace_all(names(data), '//.*', "_")

# Add dummy column for whether athlete <= 13 years old
data <- data %>%
  mutate(Young = ifelse(data$Age.At.Testing <= 13, 1, 0),
         Old   = ifelse(data$Age.At.Testing > 13 & data$Age.At.Testing <= 18, 1, 0),
         Adult = ifelse(data$Age.At.Testing > 18, 1, 0),
         Month = month(Date),
         Nordbord = rowMeans(cbind(data$Nordbord.Max.L, data$Nordbord.Max.R), na.rm = TRUE))

# Get last month's and this month's testing data
data_last    <- data[month(data$Date) == month(Sys.Date()) - 1,]
data_current <- data[month(data$Date) == month(Sys.Date()),]

# Get athletes who tested last month
athletes <- unique(data_current$Name)

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
  'Name',
  'Date',
  'Month',
  'Age.At.Testing',
  'CMJ...Flight',
  'IMTP.Peak',
  'Young',
  'Old',
  'Adult',
  'DSI',
  'Nordbord...Diff',
  'Nordbord.Max.L',
  'Nordbord.Max.R'
)

summary_data <- data %>%
  select(-Young, - Old, -Adult) %>%
  group_by(Name) %>%
  summarise_at(
    .vars = names(data)[!names(data) %in% var_list],
    .funs = func_list
  ) %>% 
  filter(Name %in% athletes)

summary_data[summary_data == -Inf] <- NA
summary_data[summary_data == Inf] <- NA

write.csv(summary_data, 
          file = paste(year(Sys.Date()), '_', month(Sys.Date()), '_summary_data.csv', sep = ''), 
          row.names = FALSE)
