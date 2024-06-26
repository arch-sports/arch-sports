start_time <- Sys.time()

library(googlesheets4)
library(tidyverse)
library(dplyr)

male_url <- "https://docs.google.com/spreadsheets/d/1VjX6dNpz1PX9mLZGPpX2utJ4Ti28ZyBoVD9nXDWj-r4"
female_url <- "https://docs.google.com/spreadsheets/d/13QQ1Oozw8Xiard1pw5bHKfflRrMU4jNIWPxXbJLN1OE"

male_sheet_names <- sheet_names(male_url)
male_sheet_names <- male_sheet_names[c(-1, -2, -3, -4, -5, -6, -7)]
female_sheet_names <- sheet_names(female_url)
female_sheet_names <- female_sheet_names[c(-1, -2, -3, -4, -5, -6, -7)]
test_header <- data.frame(
  'Vertical_Jump' = numeric(), 
  'Broad_Jump' = numeric(), 
  'Sprint_40yd' = numeric(), 
  'R_5_10_5' = numeric(), 
  'L_5_10_5' = numeric(), 
  'Yo_Yo_Test_Level_2' = numeric(), 
  'Yo_Yo_Test_Level_1' = numeric(), 
  'Box_Drill' = numeric())

male_data <- NULL
female_data <- NULL
male_missed <- list()
female_missed <- list()

counter = 1
for (name in male_sheet_names) {
  print(paste(counter, 'of', length(male_sheet_names)))
  if(!(name %in% male_data$Name)){
    tryCatch({
      range_write(male_url, data = test_header, sheet = name, range = "C2:J2")
      data <- read_sheet(male_url, sheet = name, range = "B2:J27")
      data <- data[rowSums(is.na(data)) < ncol(data), ]
      data <- mutate(data, Name = name)
      if(colnames(data)[9] != 'Box_Drill'){
        names(data)[9] <- 'Box_Drill'
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
  counter = counter + 1
}
write.csv(male_data, 'Pineville male.csv')
male_time <- Sys.time()
time_taken <- male_time - start_time
print(paste('Time to complete male data:',time_taken))

counter = 1
for (name in female_sheet_names) {
  print(paste(counter, 'of', length(female_sheet_names)))
  if(!(name %in% female_data$Name)){
    tryCatch({
      range_write(female_url, data = test_header, sheet = name, range = "C2:J2")
      data <- read_sheet(female_url, sheet = name, range = "B2:J27")
      data <- data[rowSums(is.na(data)) < ncol(data), ]
      data <- mutate(data, Name = name)
      if(colnames(data)[9] != 'Box_Drill'){
        names(data)[9] <- 'Box_Drill'
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
  counter = counter + 1
}

male_data <- mutate(male_data, "M/F" = "Male")
female_data <- mutate(female_data, "M/F" = "Female")
full_data <- rbind(male_data, female_data)
full_data <- cbind(Location = "Pineville", full_data) %>%
  relocate(c('Name', 'M/F'), .before = Date)
write.csv(full_data, file = "Pineville Test Data.csv", row.names = FALSE)

end_time <- Sys.time()
time_taken <- end_time - start_time
print(paste('Time taken:', time_taken))
