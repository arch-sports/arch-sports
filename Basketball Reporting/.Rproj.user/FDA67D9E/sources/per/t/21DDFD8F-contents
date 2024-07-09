library(quarto)
library(knitr)
library(fs)
library(dplyr)

time <- Sys.time()

setwd(getSrcDirectory(function(){})[1])

data <- read.csv(file = '../Data/pro_skills_testing.csv') %>%
  mutate(Athlete = paste(First, Last, sep = " "))

dir.create('../HTML Reports/', showWarnings = FALSE)
dir.create('../HTML Reports/Individual Reports - Athletes', showWarnings = FALSE)
dir.create('../HTML Reports/Individual Reports - Technical', showWarnings = FALSE)

quarto_render('../HTML Scripts/basketball_report.qmd',
            output_format = 'html',
            output_file = 'Pro Skills Basketball.html'
            )

athletes <- data$Athlete[!is.na(data$jump_height)]

file_move('Pro Skills Basketball.html', '../HTML Reports/Pro Skills Basketball.html')

for (athlete in athletes) {
  print(paste('Making report for', athlete, sep = " "))
  
  quarto_render('../HTML Scripts/Individual_Report.qmd',
              output_format = 'html',
              output_file = paste(athlete, '.html', sep = ""),
              execute_params = list(Athlete = athlete))
  

  file_move(paste(athlete, '.html', sep = ""), paste('../HTML Reports/Individual Reports - Athletes/', athlete, '.html', sep = ""))
  
  quarto_render('../HTML Scripts/Individual_Report_Technical.qmd',
                output_format = 'html',
                output_file = paste(athlete, ' - Technical.html', sep = ""),
                execute_params = list(Athlete = athlete))
  
  file_move(paste(athlete, ' - Technical.html', sep = ""), paste('../HTML Reports/Individual Reports - Technical/', athlete, ' - Technical.html', sep = ""))

   
}

print(paste('Time taken:', Sys.time() - time))