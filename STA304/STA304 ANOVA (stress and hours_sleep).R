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
sample_data

sample_data$stress_numeric <- as.numeric(factor(sample_data$stress, 
                                                levels = c("Never", "Sometimes", "Always")))

sample_data$sleep_category <- cut(sample_data$hours_sleep, 
                                  breaks = c(0, 6, 8, Inf),
                                  labels = c("Less than 6", "6-8", "More than 8"))

anova_result <- aov(stress_numeric ~ sleep_category, data = sample_data)
summary(anova_result)

