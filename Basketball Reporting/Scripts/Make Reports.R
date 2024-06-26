library(quarto)
library(knitr)
library(fs)
library(dplyr)

time <- Sys.time()

setwd(getSrcDirectory(function(){})[1])

data <- read.csv(file = '../Data/pro_skills_testing.csv') %>%
  mutate(Athlete = paste(First, Last, sep = " "))

dir.create('../Reports/', showWarnings = FALSE)
dir.create('../Reports/Individual Reports - Athletes', showWarnings = FALSE)
dir.create('../Reports/Individual Reports - Technical', showWarnings = FALSE)

quarto_render('../Scripts/basketball_report.qmd',
            output_format = 'pdf',
            output_file = 'Pro Skills Basketball.pdf'
            )

athletes <- data$Athlete[!is.na(data$jump_height)]

file_move('Pro Skills Basketball.pdf', '../Reports/Pro Skills Basketball.pdf')

for (athlete in athletes) {
  print(paste('Making report for', athlete, sep = " "))
  
  quarto_render('../Scripts/Individual_Report.qmd',
              output_format = 'pdf',
              output_file = paste(athlete, '.pdf', sep = ""),
              execute_params = list(Athlete = athlete))
  

  file_move(paste(athlete, '.pdf', sep = ""), paste('../Reports/Individual Reports - Athletes/', athlete, '.pdf', sep = ""))
  
  quarto_render('../Scripts/Individual_Report_Technical.qmd',
                output_format = 'pdf',
                output_file = paste(athlete, ' - Technical.pdf', sep = ""),
                execute_params = list(Athlete = athlete))
  
  file_move(paste(athlete, ' - Technical.pdf', sep = ""), paste('../Reports/Individual Reports - Technical/', athlete, ' - Technical.pdf', sep = ""))

   
}

print(paste('Time taken:', Sys.time() - time))