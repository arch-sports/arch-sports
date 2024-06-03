library(tidyverse)
library(kableExtra)
library(gridExtra)

data <- read.csv(file = 'Basketball Reporting/Data/pro_skills_testing.csv')
data <- data[!is.na(data$jump_height),]

quad_count <- length(which(data$quad == 'N'))
hip_count <- length(which(data$gator == 'N'))

low_ecc_count <- length(data$ecc_mean_force_assym[between(data$ecc_mean_force_assym, 0.05, 0.1)])
high_ecc_count <- length(data$ecc_mean_force_assym[data$ecc_mean_force_assym >= 0.10])

low_con_count <- length(data$con_mean_force_assym[between(data$con_mean_force_assym, 0.05, 0.1)])
high_con_count <- length(data$con_mean_force_assym[data$con_mean_force_assym >= 0.10])

low_land_count <- length(data$peak_assym[between(data$peak_assym, 0.05, 0.10)])
high_land_count <- length(data$peak_assym[data$peak_assym >= 0.10])

output <- data.frame(
  quad = c(quad_count, round(quad_count/33*100, 2)),
  hip = c(hip_count, round(hip_count/33*100, 2)),
  low_ecc = c(low_ecc_count, round(low_ecc_count/33*100, 2)),
  high_ecc = c(high_ecc_count, round(high_ecc_count/33*100, 2)),
  low_con = c(low_con_count, round(low_con_count/33*100, 2)),
  high_con = c(high_con_count, round(high_con_count/33*100, 2)),
  low_land = c(low_land_count, round(low_land_count/33*100, 2)),
  high_land = c(high_con_count, round(high_con_count/33*100, 2))
)

t_output <- transpose(output)
output_table <- tableGrob(t_output, 
                          cols = c('Count', '% of Total'), 
                          rows = c('Quad Tightness', 
                                   'Hip Tightness', 
                                   'Ecc. Assym. btw 5%-10%', 
                                   'Ecc. Assym. >10%', 
                                   'Con. Assym. btw 5%-10%', 
                                   'Con. Assym. >10%',
                                   'Landing Assym. btw 5%-10%', 
                                   'Landing Assym. >10%'))
grid.arrange(output_table)
