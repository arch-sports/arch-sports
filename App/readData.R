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

# testData$Ballantyne <-
#   upData(testData$Ballantyne,
#          labels = c(
#            Age.At.Testing = 'Age',
#            CMJ...Flight = 'Countermovement Jump - Flight Time',
#            CMJ...Imp.Mom = 'Countermovement Jump - Impulse-Momentum',
#            Broad.Jump = 'Broad Jump',
#            IMTP.Peak = 'Isometric Mid-Thigh Pull - Peak Force',
#            IMTP.F.BW = 'Isometric Mid-Thigh Pull - F/BW',
#            Flexed.Hang = 'Flexed Arm Hang',
#            Pull.Ups = 'Pull Ups',
#            X20.Yard.Sprint = '20-Yard Sprint',
#            X10.Yard.Split = '10-Yard Split',
#            X5.10.5 = '5-10-5 Pro Agility',
#            Yo.Yo = 'Yo-Yo Level 2',
#            X300.Yd = '300-Yard Shuttle',
#            Nordbord.Max.L = 'Nordbord Max Left',
#            Nordbord.Max.R = 'Nordbord Max Right',
#            Nordbord...Diff = 'Nordbord % Difference',
#            DSI = 'Dynamic Strength Index'
#          ),
#          units = .q(
#            Age.At.Testing = years,
#            CMJ...Flight = inches,
#            CMJ...Imp.Mom = inches,
#            Broad.Jump = inches,
#            IMTP.Peak = N,
#            IMTP.F.BW = 'N/kg',
#            Flexed.Hang = s,
#            X20.Yard.Sprint = s,
#            X10.Yard.Split = s,
#            X5.10.5 = s,
#            X300.Yd = s,
#            Nordbord.Max.L = N,
#            Nordbord.Max.R = N
#          )
#   )
# 
# testData$Pineville <-
#   upData(testData$Pineville,
#          labels = c(
#            Age.At.Testing = 'Age',
#            Vertical.Jump = 'Vertical Jump',
#            Broad.Jump = 'Broad Jump',
#            Sprint.40yd = '40-Yard Sprint',
#            R.5.10.5 = '5-10-5 Pro Agility Right',
#            L.5.10.5 = '5-10-5 Pro Agility Left',
#            Yo.Yo.Test.Level.2 = 'Yo-Yo Level 2',
#            Yo.Yo.Test.Level.1 = 'Yo-Yo Level 1',
#            Box.Drill = 'Box Drill'
#          ),
#          units = .q(
#            Vertical.Jump = inches,
#            Broad.Jump = inches,
#            Sprint.40yd = s,
#            R.5.10.5 = s,
#            L.5.10.5 = s,
#            Box.Drill = s
#          )
#   )






































