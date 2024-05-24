library(quarto)
library(knitr)
library(fs)

setwd(getSrcDirectory(function(){})[1])

time <- Sys.time()
quarto_render('basketball_report.qmd',
              output_format = 'pdf',
              output_file = 'Pro Skills Basketball Test Results.pdf')

file_move('Pro Skills Basketball Test Results.pdf', './Reports/Pro Skills Basketball Test Results.pdf')

data <- read.csv(file = './Data/basketball_random.csv')
athletes <- data$Athlete

for (athlete in athletes) {
  quarto_render('Individual_Report.qmd',
              output_format = 'pdf',
              output_file = paste(athlete, '.pdf', sep = ""),
              execute_params = list(Athlete = athlete))

  fs::file_move(paste(athlete, '.pdf', sep = ""), paste('./Reports/Individual Reports/', athlete, '.pdf', sep = ""))
}

print(paste('Time taken:', Sys.time() - time))