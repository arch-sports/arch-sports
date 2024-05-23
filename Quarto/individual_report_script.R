library(quarto)
library(knitr)

data <- read.csv(file = 'basketball_random.csv')

athletes <- data$Athlete
athlete = 'Jonathan Vialpando'
quarto_render('Individual_Report.qmd',
              output_format = 'pdf',
              output_file = athlete,
              execute_params = list(Athlete = athlete))


