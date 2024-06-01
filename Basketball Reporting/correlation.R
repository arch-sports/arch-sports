library(tidyverse)
library(corrplot)

data <- read.csv(file = './Data/pro_skills_testing.csv')
clean_data <- data[!is.na(data$jump_height), ]

corr_data <- select(clean_data, -c(First, Last, Team, ecc_mean_assym_side, con_mean_force_assym_side, peak_assym_side, event)) %>%
  mutate(pain = ifelse(pain == 'Y', 1, 0), gator = ifelse(gator == 'Y', 1, 0), calf = ifelse(calf == 'Y', 1, 0), lat = ifelse(lat == 'Y', 1, 0), quad = ifelse(quad == 'Y', 1, 0))

i <- 1
j <- 1
corr_matrix <- matrix(nrow = length(names(corr_data)), ncol = length(names(corr_data)))
rownames(corr_matrix) = names(corr_data)
colnames(corr_matrix) = names(corr_data)

for (name1 in names(corr_data)) {

  for(name2 in names(corr_data)){
    fit <- lm(corr_data[[name1]] ~ corr_data[[name2]])
    anova <- anova(fit)
    p_value <- anova$`Pr(>F)`[1]
    corr_matrix[i, j] <- 1 - p_value
    j <- j + 1
  }

  j <- 1
  i <- i + 1
}

print(corrplot(corr_matrix, method = 'number', type = 'upper'))


corr_matrix <- cor(corr_data, use = 'pairwise.complete.obs', method = 'pearson')
print(corrplot(corr_matrix, method = 'number', type = 'upper'))
