library(googlesheets4)
library(tidyverse)

# Import testing data
test_url <- 'https://docs.google.com/spreadsheets/d/1VzdlsDCA-X8HVKvUktG1A5XZNiq3IuOwX2Zbq8k6Iu0/edit?gid=2006047091#gid=2006047091'
test_sheet <- 'MASTER DATA TABLE'
data <- read_sheet(test_url, sheet = test_sheet)

# Add dummy column for whether athlete <= 13 years old
data <- data %>%
  mutate(Young = ifelse(data$`Age At Testing` <= 13, 1, 0),
         Old   = ifelse(data$`Age At Testing` > 13 & data$`Age At Testing` <= 18, 1, 0),
         Adult = ifelse(data$`Age At Testing` > 18, 1, 0))

# Get counts of athletes. Athletes with only one test will be excluded from improvements
athlete_counts <- data %>% count(Name)

# Get last month's testing data
data_last <- data[month(data$Date) == month(Sys.Date()) - 1,]
data_current <- data[month(data$Date) == month(Sys.Date()),]

# Get list of athlete names who tested last month
athletes <- unique(data_current$Name)

best_df <- data.frame()
find_min <- c('10 Yard Split', '20 Yard Sprint', '5-10-5', 'Nordbord % Diff')
# Get each athlete's best scores
for (athlete in athletes) {
  athlete_data <- data[data$Name == athlete,] %>%
    select(-'Age At Testing', -Date, -Young, -Old, -Adult)
  
  best <- if(nrow(athlete_data) == 1){
    athlete_data
  } else {
    sapply(athlete_data, 
           function(x) ifelse(x %in% find_min, 
                              min(x, na.rm = 1), 
                              max(x, na.rm = 1)))
  }
  
  best_df <- rbind(best_df, best[1,])
  names(best_df) <- names(athlete_data)
}

best_df <- cbind(best_df$Name, as.data.frame(sapply(best_df[,2:ncol(best_df)], as.numeric)))

improve_last_df <- data.frame()
improve_best_df <- data.frame()
# Get each athlete's improvement from last month
for (athlete in athletes) {
  columns <- data %>%
    select(-Date, -'Age At Testing') %>%
    names()

  athlete_current <- data_current[data_current$Name == athlete, 4:(ncol(data_current) - 3)] %>%
    mutate(athleteCount = ifelse(count(data, Name)$n[count(data, Name)$Name == athlete] > 1,
                                 1, 0)) %>%
    filter(athleteCount == 1) %>%
    select(-athleteCount)
  
  athlete_last    <- data_last[data_last$Name == athlete, 4:(ncol(data_last) - 3)] %>%
    mutate(athleteCount = ifelse(count(data, Name)$n[count(data, Name)$Name == athlete] > 1,
                                 1, 0)) %>%
    filter(athleteCount == 1) %>%
    select(-athleteCount)
  
  athlete_best    <- best_df[best_df$Name == athlete, 2:(ncol(best_df))] %>%
    mutate(athleteCount = ifelse(count(data, Name)$n[count(data, Name)$Name == athlete] >= 1,
                                 1, 0)) %>%
    filter(athleteCount == 1) %>%
    select(-athleteCount)
  
  improvement_last <- if(nrow(athlete_current) == nrow(athlete_last)) {
    athlete_current - athlete_last
  } else if(nrow(athlete_last) == 0) {
    athlete_current
  } else {
    athlete_last
  }
  improvement_last <- mutate(improvement_last,
                        Name = athlete)
  
  improve_last_df <- rbind(improve_last_df, improvement_last)
  improve_last_df[is.na(improve_last_df)] <- 0
  
  improvement_best <- if(nrow(athlete_current) == 0 || nrow(athlete_best) == 0){
    athlete_best
  } else {
    athlete_current - athlete_best
  }
  improve_best_df <- rbind(improve_best_df, improvement_best)
  improve_best_df[is.na(improve_best_df)] <- 0
  
}
count_improvements_last <- improve_last_df %>%
  select(-'Nordbord % Diff', -DSI) %>%
  sapply(function(x) sum(
         ifelse(x %in% find_min,
                ifelse(x < 0, 1, 0),
                ifelse(x > 0, 1, 0))
       ))

count_improvements_best <- improve_best_df %>%
  select(-'Nordbord % Diff', -DSI) %>%
  sapply(function(x) sum(
    ifelse(x %in% find_min,
           ifelse(x < 0, 1, 0),
           ifelse(x > 0, 1, 0))
  ))

