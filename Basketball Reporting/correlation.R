library(tidyverse)
library(corrplot)

data <- read.csv(file = './Data/pro_skills_testing.csv')
clean_data <- data[!is.na(data$jump_height), ]

corr_data <- select(clean_data, -c(First, Last, Team, ecc_mean_assym_side, con_mean_force_assym_side, peak_assym_side, event)) %>%
  mutate(pain = ifelse(pain == 'Y', 1, 0), gator = ifelse(gator == 'Y', 1, 0), calf = ifelse(calf == 'Y', 1, 0), lat = ifelse(lat == 'Y', 1, 0), quad = ifelse(quad == 'Y', 1, 0))

corr_matrix <- cor(corr_data, use = 'pairwise.complete.obs', method = 'pearson')
print(corrplot(corr_matrix, method = 'ellipse', type = 'upper'))
