library(googlesheets4)
library(tidyverse)
library(dplyr)

setwd(getSrcDirectory(function(){}))

locations <- list("Ballantyne", "Pineville")

for (location in locations) {
  dir.create(paste('Data/', location, sep = ""), showWarnings = FALSE)
}

#Training Data URLs
train.URL <- list(
  "Ballantyne" = "https://docs.google.com/spreadsheets/d/1MY7WtWH4ba4npaUHOYmZ4OotMZwp8k_jVTCBhqOSeyU/"
)

#Testing Data URLs
test.URL <- list(
  "Ballantyne" = "https://docs.google.com/spreadsheets/d/1VzdlsDCA-X8HVKvUktG1A5XZNiq3IuOwX2Zbq8k6Iu0/",
  "Pineville" = "https://docs.google.com/spreadsheets/d/1Ze4abQWHOchEDLOo0p-JP4ZgkBDxDyb_P4_HSzA4ipg"
)

#Testing Data Sheet Names
test.Sheets <- list(
  "Ballantyne" = 'MASTER DATA TABLE',
  "Pineville" = 'MASTER DATA TABLE'
)

#Training Data Sheet Names
train.Sheets <- list(
  "Ballantyne" = 'TRAINING DATA'
)


#Read data into csv
for (location in locations) {
  #Testing data
  path <- paste('Data/', location, sep = "")
  file_name <- paste(location, '_Test.csv', sep = "")
  
  if(!file.exists(file.path(path, file_name))){
    read_sheet(as.character(test.URL[location]), as.character(test.Sheets[location])) %>%
      write.csv(file = file.path(path, file_name), row.names = FALSE)
  }
  
  #Training data
  if(location %in% names(train.URL)){
    path <- paste('Data/', location, sep = "")
    file_name <- paste(location, '_Training.csv', sep = "")
    
    if(!file.exists(file.path(path, file_name))){
      read_sheet(as.character(train.URL[location]), as.character(train.Sheets[location])) %>%
        write.csv(file = file.path(path, file_name), row.names = FALSE)
    }
    
  }
}
