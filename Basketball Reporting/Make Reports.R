library(quarto)
library(knitr)
library(fs)
library(dplyr)

setwd(getSrcDirectory(function(){})[1])

data <- read.csv(file = './Data/basketball_random.csv') %>% mutate(Team = gsub('/', '_', Team))

teams <- unique(data$Team)


time <- Sys.time()

for (team in teams) {
  print(paste('Making report for', team, sep = " "))
  dir.create(paste('./Reports/', team, sep = ''), showWarnings = FALSE)
  dir.create(paste('./Reports/', team, '/Individual Reports', sep = ''), showWarnings = FALSE)
  
  athletes <- data$Athlete[data$Team == team]
  
  quarto_render('basketball_report.qmd',
              output_format = 'pdf',
              output_file = paste(team, '.pdf', sep = ''),
              execute_params = list(Team = team))
  
  file_move(paste(team, '.pdf', sep = ''), paste('./Reports/', team, '/', team, '.pdf', sep = ''))
  
  for (athlete in athletes) {
    print(paste('Making report for', athlete, sep = " "))
    
    quarto_render('Individual_Report.qmd',
                output_format = 'pdf',
                output_file = paste(team, '_', athlete, '.pdf', sep = ""),
                execute_params = list(Athlete = athlete, Team = team))
  
    file_move(paste(team, '_', athlete, '.pdf', sep = ""), paste('./Reports/', team, '/Individual Reports/', team, '_', athlete, '.pdf', sep = ""))
  }
}




print(paste('Time taken:', Sys.time() - time))