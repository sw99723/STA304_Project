library("tidyverse")

# You will need to modify the working directory.
setwd("C:\\Users\\AnnaH\\OneDrive\\Desktop\\STA304 TA Material\\STA304 TA Material\\2024 New Instructions\\Data Collection")

data = read.csv("sample_data.csv", header = TRUE)

names(data)

#####################################################################
# Sample Size Computations (SRS)                                    #
#####################################################################

N = 300
p = 0.5
q = 1-p
B = 0.13

D = B^2 / 4
(N * p * q) / ((N-1) * D + p * q)


#####################################################################
# (RQ1) Do students prefer assignments over tests and examinations? #
#####################################################################

# Checking to see the proportion of those who like assignments
nrow(data[data$LikesAssignments == "Yes",])
nrow(data[data$LikesAssignments == "No",])

prop.test(x = nrow(data[data$LikesAssignments == "Yes",]), 
          n = 50, p = 0.5, correct = FALSE)

# In general; it doesn't seem like students prefer (or not prefer)
# assignments over tests.

#####################################################################
# (RQ2) Does whether students prefer assignments over examinations  #
# depend on demographic factors? (Examples of demographic factors   #
# include gender and whether they’re international or domestic).    #
#####################################################################

# Obtaining responses from those who identify as male/female
male_dat = data[data$Gender == "Male",] 
female_dat = data[data$Gender == "Female",]

# Seeing if the proportions of those who like assignments differ from male/female
f_assignments = data %>%
  filter(Gender == "Female", LikesAssignments == "Yes")
m_assignments = data %>%
  filter(Gender == "Male", LikesAssignments == "Yes")

# Checking the assumptions for the test of proportions (both values must be >= 5)
nrow(female_dat) * (nrow(f_assignments)/nrow(female_dat))
nrow(female_dat) * (1-(nrow(f_assignments)/nrow(female_dat)))
nrow(male_dat) * (nrow(m_assignments)/nrow(male_dat))
nrow(male_dat) * (1-(nrow(m_assignments)/nrow(male_dat)))

# Statistical test of proportions
prop.test(x = c(nrow(m_assignments), nrow(f_assignments)), 
          n = c(nrow(male_dat), nrow(female_dat)), 
          alternative = "two.sided",
          correct = TRUE)
# Seems like gender isn't linked to whether or not someone likes assignments

# Seeing if the proportions of those who like assignments differ from international/domestic

# Obtaining responses from those who identify as domestic/international
dom_dat = data[data$Status == "Domestic",] 
int_dat = data[data$Status == "International",]

d_assignments = data %>%
  filter(Status == "Domestic", LikesAssignments == "Yes")
i_assignments = data %>%
  filter(Status == "International", LikesAssignments == "Yes")

# checking the assumptions for the test of proportions
nrow(dom_dat) * (nrow(i_assignments)/nrow(dom_dat))
nrow(dom_dat) * (1-(nrow(i_assignments)/nrow(dom_dat)))
nrow(int_dat) * (nrow(d_assignments)/nrow(int_dat))
nrow(int_dat) * (1-(nrow(d_assignments)/nrow(int_dat)))


# statistical test
prop.test(x = c(nrow(d_assignments), nrow(i_assignments)), 
          n = c(nrow(dom_dat), nrow(int_dat)), 
          alternative = "two.sided",
          correct = TRUE)
# Similarly, seems like whether someone is international/domestic isn't linked with
# whether someone likes assignments

#####################################################################
# (RQ3) Do students perceive assignments to enhance industry        #
# relevant skills, such as collaboration, writing, and coding?      #                                                    #
#####################################################################

# Initial Analysis on Likert Data

summary(data$Collaboration)
sd(data$Collaboration)

summary(data$Writing)
sd(data$Writing)

summary(data$Interesting)
sd(data$Interesting)

summary(data$Understanding)
sd(data$Understanding)

summary(data$Coding)
sd(data$Coding)

#############################################
# One-sample test for means                 #
#############################################

# Testing normality assumption. There are multiple methods. 
qqnorm(data$Collaboration)
qqline(data$Collaboration)

shapiro.test(data$Collaboration) # if p-val < 0.05, it is not-normal

# Note: normality assumption fails but since sample size is >30 then
# we can use the t-test. I will not be checking the assumptions for others.

t.test(data$Collaboration, mu = 4, alternative = "two.sided")
t.test(data$Writing, mu = 4, alternative = "two.sided")
t.test(data$Interesting, mu = 4, alternative = "two.sided")
t.test(data$Understanding, mu = 4, alternative = "two.sided")
t.test(data$Coding, mu = 4, alternative = "two.sided")

#############################################
# Two-sample test for means (Gender)        #
#############################################

# Testing to see if the perceptions of assignments differ among genders.
# Here, the sample sizes will be <30 (for male) so we need to actually test
# assumptions!
# For ease I will just be using shapiro.wilk for normality, but you may use 
# other tests you are familiar with.

# Collaboration ##########################################

shapiro.test(female_dat$Collaboration) 
shapiro.test(male_dat$Collaboration) 

# Using barlett's test for equal variances
bartlett.test(Collaboration ~ Gender, data = data)
# if p-val < 0.05, the variances are unequal.

t.test(male_dat$Collaboration, 
       female_dat$Collaboration, 
       alternative = "two.sided", var.equal = TRUE)

# Writing ################################################

shapiro.test(female_dat$Writing) 
shapiro.test(male_dat$Writing) 

bartlett.test(Writing ~ Gender, data = data)

# Note: the normality assumptions fail, so we use the mann-whitney test
# (non-parametric version of the t-test)
wilcox.test(male_dat$Writing, 
            female_dat$Writing, 
            alternative = "two.sided", var.equal = TRUE)
# can ignore the warning message.

# Interesting ############################################

shapiro.test(female_dat$Interesting) 
shapiro.test(male_dat$Interesting) 

bartlett.test(Interesting ~ Gender, data = data)

t.test(male_dat$Interesting, 
       female_dat$Interesting, 
       alternative = "two.sided", var.equal = TRUE)

# Understanding ##########################################

shapiro.test(female_dat$Understanding) 
shapiro.test(male_dat$Understanding) 

bartlett.test(Understanding ~ Gender, data = data)

wilcox.test(male_dat$Understanding, 
            female_dat$Understanding, 
            alternative = "two.sided", var.equal = TRUE)

# Coding ##########################################

shapiro.test(female_dat$Coding) 
shapiro.test(male_dat$Coding) 

bartlett.test(Coding ~ Gender, data = data)

wilcox.test(male_dat$Coding, 
            female_dat$Coding, 
            alternative = "two.sided", var.equal = TRUE)

#############################################
# Two-sample test for means (Dom vs Int)    #
#############################################

# Testing to see if the perceptions of assignments differ among international
# and domestic students.

# Collaboration ##########################################

shapiro.test(dom_dat$Collaboration) 
shapiro.test(int_dat$Collaboration) 

# Using barlett's test for equal variances
bartlett.test(Collaboration ~ Status, data = data)
# if p-val < 0.05, the variances are unequal.

t.test(dom_dat$Collaboration, 
       int_dat$Collaboration, 
       alternative = "two.sided", var.equal = TRUE)

# Writing ################################################

shapiro.test(dom_dat$Writing) 
shapiro.test(int_dat$Writing) 

bartlett.test(Writing ~ Status, data = data)

wilcox.test(dom_dat$Writing, 
            int_dat$Writing, 
            alternative = "two.sided", var.equal = TRUE)

# Interesting ############################################

shapiro.test(dom_dat$Interesting) 
shapiro.test(int_dat$Interesting) 

bartlett.test(Interesting ~ Status, data = data)

t.test(dom_dat$Interesting, 
       int_dat$Interesting, 
       alternative = "two.sided", var.equal = TRUE)

# Understanding ##########################################

shapiro.test(dom_dat$Understanding) 
shapiro.test(int_dat$Understanding) 

bartlett.test(Understanding ~ Status, data = data)

t.test(dom_dat$Understanding, 
       int_dat$Understanding, 
       alternative = "two.sided", var.equal = TRUE)

# Coding ##########################################

shapiro.test(dom_dat$Coding) 
shapiro.test(int_dat$Coding) 

bartlett.test(Coding ~ Status, data = data)

wilcox.test(dom_dat$Coding, 
            int_dat$Coding, 
            alternative = "two.sided", var.equal = TRUE)

# in general: seems like there arent many differences amongst international
# students!
