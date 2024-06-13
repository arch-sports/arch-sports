library(quarto)
library(knitr)
library(fs)
library(dplyr)

setwd(getSrcDirectory(function(){})[1])

data <- read.csv(file = './Data/pro_skills_testing.csv') %>%
  mutate(Athlete = paste(First, Last, sep = " "))

quarto_render('basketball_report.qmd',
            output_format = 'pdf',
            output_file = 'Pro Skills Basketball.pdf'
            )

time <- Sys.time()


dir.create('./Reports/', showWarnings = FALSE)
dir.create('./Reports/Individual Reports - Athletes', showWarnings = FALSE)
dir.create('./Reports/Individual Reports - Technical', showWarnings = FALSE)

athletes <- data$Athlete[!is.na(data$jump_height)]

file_move('Pro Skills Basketball.pdf', './Reports/Pro Skills Basketball.pdf')

for (athlete in athletes) {
  print(paste('Making report for', athlete, sep = " "))
  
  quarto_render('Individual_Report.qmd',
              output_format = 'pdf',
              output_file = paste(athlete, '.pdf', sep = ""),
              execute_params = list(Athlete = athlete))
  

  file_move(paste(athlete, '.pdf', sep = ""), paste('./Reports/Individual Reports - Athletes/', athlete, '.pdf', sep = ""))
  
  quarto_render('Individual_Report_Technical.qmd',
                output_format = 'pdf',
                output_file = paste(athlete, '- Technical.pdf', sep = ""),
                execute_params = list(Athlete = athlete))
  
  file_move(paste(athlete, '.pdf', sep = ""), paste('./Reports/Individual Reports - Technical/', athlete, '.pdf', sep = ""))
  
}




print(paste('Time taken:', Sys.time() - time))