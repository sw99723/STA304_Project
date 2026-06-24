my_data <- read.csv('STA304 Group Project Dataset.csv', header = TRUE, sep = ',')

data_cleaned <- my_data[, !(names(my_data) == "X")]

N = 200
B = 0.29
D = (B^2)/4
sigma = sd(data_cleaned$academic_workload)
n = ceiling((N * sigma^2) / ((N-1) * D + sigma^2))

set.seed(1)
random_indices <- sample(1:nrow(data_cleaned), n)
sample_data <- data_cleaned[random_indices, ]
sample_data

sample_data$stress_numeric <- as.numeric(factor(sample_data$stress, 
                                                levels = c("Never", "Sometimes", "Always")))

anova_result <- aov(stress_numeric ~ factor(academic_workload), data = sample_data)
summary(anova_result)

