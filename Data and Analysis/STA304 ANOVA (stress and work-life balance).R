my_data <- read.csv('STA304 Group Project Dataset.csv', header = TRUE, sep = ',')

data_cleaned <- my_data[, !(names(my_data) == "X")]

N = 200
B = .25
D = (B^2)/4
sigma = sd(data_cleaned$academic_workload)
n = ceiling((N * sigma^2) / ((N-1) * D + sigma^2))

set.seed(1)
random_indices <- sample(1:nrow(data_cleaned), n)
sample_data <- data_cleaned[random_indices, ]

sample_data$study_category <- cut(sample_data$hours_study, 
                                  breaks = c(0, 10, 20, Inf),
                                  labels = c("Low", "Medium", "High"))

anova_social <- aov(stress_numeric ~ factor(missed_social_events), data = sample_data)
print(summary(anova_social))


