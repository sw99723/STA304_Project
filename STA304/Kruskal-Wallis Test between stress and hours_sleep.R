my_data <- read.csv("STA304 Group Project Dataset.csv", header=TRUE, sep=',')
# Datapreprocessing
# Remove column 'X'
data_cleaned <- my_data[,!(names(my_data)=="X")]
# Remove fake responses on row 3 and 12
data_cleaned <- data_cleaned[-c(3, 12),]
data_cleaned

N = 200
B = .29
D = (B^2)/4
sigma = sd(data_cleaned$academic_workload)
n = ceiling((N * sigma^2) / ((N-1) * D + sigma^2))
n

# Select a random sample of n rows
set.seed(1)
random_indices <- sample(1:nrow(data_cleaned), n)
sample_data <- data_cleaned[random_indices, ]
sample_data

# Kruskal-Wallis Test between stress and hours_sleep
sample_data$stress_numeric <- as.numeric(factor(sample_data$stress, 
                                                levels = c("Never", "Sometimes", "Always")))

sample_data$sleep_category <- cut(sample_data$hours_sleep, 
                                  breaks = c(0, 6, 8, Inf),
                                  labels = c("less than 6 hours", "6-8 hours", "over 8 hours"),
                                  include.lowest = TRUE, right = FALSE)

shapiro_result_hours_sleep <- shapiro.test(sample_data$hours_sleep)
print(shapiro_result_hours_sleep)

kruskal_result_sleep <- kruskal.test(stress_numeric ~ sleep_category, data = sample_data)
print(kruskal_result_sleep)
tapply(sample_data$stress_numeric, sample_data$sleep_category, mean) #mean stress levels across different sleep catagories

