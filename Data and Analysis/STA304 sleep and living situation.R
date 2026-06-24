my_data <- read.csv('STA304 Group Project Dataset.csv', header = TRUE, sep = ',')

data_cleaned <- my_data, !(names(my_data) == "X")]
data_cleaned <- data_cleaned[-c(3, 12),]

N = 200
В = 0.25
D = (B^2) / 4

sigma = sd(data_cleaned$academic_workload, na.rm = TRUE) # Ensure NA handling
n = ceiling((N * sigma^2) / ((N - 1) * D + sigma^2))

# Select a random sample of n rows
set.seed(1)
random_indices <- sample(1:nrow(data_cleaned), n) sample_data <- data_cleaned[random_indices, ]

# Perform One-Way ANOVA between stress levels and academic workload
anova_result ‹- aov(stress ~ academic_workload, data = sample_data) summaryanova_result)