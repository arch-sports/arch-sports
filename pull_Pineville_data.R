library(googlesheets4)
library(tidyverse)

male_url <- "https://docs.google.com/spreadsheets/d/1VjX6dNpz1PX9mLZGPpX2utJ4Ti28ZyBoVD9nXDWj-r4"
female_url <- "https://docs.google.com/spreadsheets/d/13QQ1Oozw8Xiard1pw5bHKfflRrMU4jNIWPxXbJLN1OE"

male_sheet_names <- sheet_names(male_url)
male_sheet_names <- male_sheet_names[c(-1, -2, -3, -4, -5, -6, -7)]
female_sheet_names <- sheet_names(female_url)
female_sheet_names <- female_sheet_names[c(-1, -2, -3, -4, -5, -6, -7)]
test_header <- data.frame(
  'Vertical Jump' = numeric(), 
  'Broad Jump' = numeric(), 
  'Sprint - 40yd' = numeric(), 
  '(R) 5-10-5' = numeric(), 
  '(L) 5-10-5' = numeric(), 
  'Yo Yo Test Level 2' = numeric(), 
  'Yo Yo Test Level 1' = numeric(), 
  'Box Drill' = numeric())

male_data <- NULL
female_data <- NULL
male_missed <- list()
female_missed <- list()

for (name in male_sheet_names) {
  if(!(name %in% male_data$Name)){
    tryCatch({
      range_write(male_url, data = test_header, sheet = name, range = "C2:J2")
      data <- read_sheet(male_url, sheet = name, range = "B2:J27")
      data <- data[rowSums(is.na(data)) < ncol(data), ]
      data <- mutate(data, Name = name)
      if(colnames(data)[9] != 'Box Drill'){
        names(data)[9] <- 'Box Drill'
      }
      if(is.null(male_data)) {
        male_data <- data
      } else{
        male_data <- rbind(male_data, data)
      }
    }, error = function(e) {
      append(male_missed, name)
    }
    )
    
  }
}

for (name in female_sheet_names) {
  if(!(name %in% female_data$Name)){
    tryCatch({
      range_write(female_url, data = test_header, sheet = name, range = "C2:J2")
      data <- read_sheet(female_url, sheet = name, range = "B2:J27")
      data <- data[rowSums(is.na(data)) < ncol(data), ]
      data <- mutate(data, Name = name)
      if(colnames(data)[9] != 'Box Drill'){
        names(data)[9] <- 'Box Drill'
      }
      if(is.null(female_data)) {
        female_data <- data
      } else{
        female_data <- rbind(female_data, data)
      }
    }, error = function(e) {
      append(female_missed, name)
    }
    )
    
  }
}

male_data <- mutate("M/F" = "Male")
female_data <- mutate("M/F" = "Female")
full_data <- rbind(male_data, female_data)
