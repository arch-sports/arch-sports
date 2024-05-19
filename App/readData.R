library(googlesheets4)
library(tidyverse)
library(dplyr)

setwd(getSrcDirectory(function(){}))

locations <- list("Ballantyne", "Pineville")

#Testing Data URLs
test.URL <- list(
  "Ballantyne" = "https://docs.google.com/spreadsheets/d/1VzdlsDCA-X8HVKvUktG1A5XZNiq3IuOwX2Zbq8k6Iu0/",
  "Pineville" = "https://docs.google.com/spreadsheets/d/1Ze4abQWHOchEDLOo0p-JP4ZgkBDxDyb_P4_HSzA4ipg"
)

#Training Data URLs
train.URL <- list(
  "Ballantyne" = "https://docs.google.com/spreadsheets/d/1MY7WtWH4ba4npaUHOYmZ4OotMZwp8k_jVTCBhqOSeyU/"
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

#Create list of dataframes
testData = vector(mode = 'list', length = length(names(test.URL)))
trainData = vector(mode = 'list', length = length(names(train.URL)))

names(testData) <- names(test.URL)
names(trainData) <- names(train.URL)

#Read data into csv
for (location in locations) {
  #Create location directories if they do not exist already
  dir.create(paste('Data/', location, sep = ""), showWarnings = FALSE)
  
  #Testing data
  path <- paste('Data/', location, sep = "")
  file_name <- paste(location, '_Test.csv', sep = "")
  
  if(!file.exists(file.path(path, file_name))){
    read_sheet(as.character(test.URL[location]), as.character(test.Sheets[location])) %>%
      write.csv(file = file.path(path, file_name), row.names = FALSE)
  }
  testData[[location]] <- read.csv(file = file.path(path, file_name))
  
  path <- paste('Data/', location, sep = "")
  file_name <- paste(location, '_Training.csv', sep = "")
  #Training data
  if(location %in% names(train.URL) && !file.exists(file.path(path, file_name))){
    read_sheet(as.character(train.URL[location]), as.character(train.Sheets[location])) %>%
      write.csv(file = file.path(path, file_name), row.names = FALSE)
  }
  if(file.exists(file.path(path, file_name))){
    trainData[[location]] <- read.csv(file = file.path(path, file_name))
  }
}
