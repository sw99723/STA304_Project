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

#####Simple and Multiple Linear Regressions#####

# Research Question 1 - Simple Linear Regression
sample_data$stress_numeric <- as.numeric(factor(sample_data$stress, levels = c("Never", "Sometimes", "Always")))
rq1_stress.lm = lm(stress_numeric ~ academic_workload, data = sample_data)
summary(rq1_stress.lm)

# Assumptions for Research Question 1 - Simple Linear Regression
png("residuals_rq1.png", width = 800, height = 600)
# Plot code goes here, for example:
plot(residuals(rq1_stress.lm))
dev.off()


# Research Question 2 - Multiple Linear Regression
rq2.lm = lm(hours_sleep ~ academic_workload + missed_social_events, data = sample_data)
summary(rq2.lm)

# Assumptions for Research Question 2 - Multiple Linear Regression
png("residuals_rq2.png", width = 800, height = 600)
# Plot code goes here, for example:
plot(residuals(rq2_stress.lm))
dev.off()


# Assumptions for Research Question 2 - Multiple Linear Regression
png("qq_rq2.png", width = 800, height = 600)
# Plot code goes here, for example:
plot(rq2.lm, which=2)
dev.off()

# Assumptions for Research Question 2 - Multiple Linear Regression
library(car)
vif(rq2.lm)

# Research Question 3 - Multiple Linear Regression
rq3.lm <- lm(stress_numeric ~ missed_social_events + living_situation, data = sample_data)
summary(rq3.lm)

# Assumptions for Research Question 3 - Multiple Linear Regression
png("residuals_rq3.png", width = 800, height = 600)
# Plot code goes here, for example:
plot(residuals(rq3.lm))
dev.off()


# Assumptions for Research Question 3 - Multiple Linear Regression
library(car)
vif(rq3.lm)


#####ANOVA##### 
#Conduct Barlett's test to fulfill assumptions before conducting ANOVA test

sample_data$stress_numeric <- as.numeric(factor(sample_data$stress, 
                                                levels = c("Never", "Sometimes", "Always")))

anova_result <- aov(stress_numeric ~ factor(academic_workload), data = sample_data)
summary(anova_result)

# ANOVA between stress and hours_sleep


sample_data$stress_numeric <- as.numeric(factor(sample_data$stress, 
                                                levels = c("Never", "Sometimes", "Always")))

sample_data$sleep_category <- cut(sample_data$hours_sleep, 
                                  breaks = c(0, 6, 8, Inf),
                                  labels = c("Less than 6", "6-8", "More than 8"))

anova_result <- aov(stress_numeric ~ sleep_category, data = sample_data)
summary(anova_result)

# ANOVA between stress and missed social events
sample_data$study_category <- cut(sample_data$hours_study, 
                                  breaks = c(0, 10, 20, Inf),
                                  labels = c("Low", "Medium", "High"))

anova_social <- aov(stress_numeric ~ factor(missed_social_events), data = sample_data)
print(summary(anova_social))

#####Chi Square Test#####

sample_data$workload <-as.numeric(factor(sample_data$academic_workload,levels =c(1,2,3,4,5)))
                                 
sample_data$anxiety_numeric <- as.numeric(factor(sample_data$anxiety,levels = c("Never", "Sometimes", "Always")))

sample_data$concentration_numeric <- as.numeric(factor(sample_data$concentration,levels = c("Never", "Sometimes", "Always")))

sample_data$living <- as.numeric(factor(sample_data$living_situation,
                                        levels = c("Living alone", "Living with family", "Living with roommates", "Living on campus")))

sample_data$time <-as.numeric(factor(sample_data$time_management,levels =c(0,1,2,3,4)))

sample_data$finances <-as.numeric(factor(sample_data$financial_problems,levels =c(0,1,2,3,4)))

sample_data$socials <-as.numeric(factor(sample_data$missed_social_events,levels =c(0,1,2,3,4)))

#Academic Workload vs Stress
new_table <- table(sample_data$workload, sample_data$stress_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Academic Workload vs Anxiety
new_table <- table(sample_data$workload, sample_data$anxiety_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Academic Workload vs Concentration
new_table <- table(sample_data$workload, sample_data$concentration_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Living Situation vs Stress
new_table <- table(sample_data$living, sample_data$stress_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Living Situation vs Anxiety
new_table <- table(sample_data$living, sample_data$anxiety_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Living Situation vs Concentration
new_table <- table(sample_data$living, sample_data$concentration_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Time Management vs Stress
new_table <- table(sample_data$time, sample_data$stress_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Time Management vs Anxiety
new_table <- table(sample_data$time, sample_data$anxiety_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Time Management vs Concentration
new_table <- table(sample_data$time, sample_data$concentration_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Financials vs Stress
new_table <- table(sample_data$finances, sample_data$stress_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Financials vs Anxiety
new_table <- table(sample_data$finances, sample_data$anxiety_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Financials vs Concentration
new_table <- table(sample_data$finances, sample_data$concentration_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)
