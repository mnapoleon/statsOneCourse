### Assignment #6
install.packages("psych")

library(psych)

data <- read.table("stats1-datafiles-Stats1.13.HW.06.txt", header = T)
cor(data[1:3]) 

### Question 1
## In a model predicting salary, what is the unstandardized regression coefficient for 
### years, assuming years is the only predictor variable in the model?
model1 <- lm(data$salary ~ data$years)
summary(model1)
confint(model1)

# 5638

### Question 2
### In a model predicting salary, what is the 95% confidence interval for the unstandardized
### regression coefficient for years, assuming years is the only predictor variable in the model?

# 4930 6345

### Question 3
### In a model predicting salary, what is the unstandardized regression coefficient for years, 
### assuming years and courses are both included as predictor variables in the model?
model2 <- lm(data$salary ~ data$years + data$courses)
summary(model2)
confint(model2)

# 4807

### Question 4
### In a model predicting salary, what is the 95% confidence interval for the unstandardized 
### regression coefficient for years, assuming years and courses are both included as predictor variables 
### in the model?

# 4140 5473

### Question 5
### What is the predicted difference in salary between Doctors and Lawyers assuming an equal and average 
### number of years and courses?
prof.code <- C(data$profession, treatment)
model3 <- lm(data$salary ~ data$years + data$courses + (prof.code))
summary(model3)
confint(model3)

#  9204.13

### Question 6
### Is the predicted difference between Doctors and Lawyers statistically significant?

# Yes

### Question 7
### What is the predicted difference in salary between Doctors and Teachers assuming an 
### equal and average number of years and courses?

#  15902.77

## Question 8
### Is the predicted difference between Doctors and Teachers statistically significant?

# Yes

### Question 9
### What is the actual difference in mean salary between Doctors and Teachers?

tapply(data$salary, data$profession, mean)

# 24611

### Question 10
### What combination of predictors represents the best model in terms of predicting salary?

model4 <- lm(data$salary ~ data$years + (prof.code))
summary(model4)
confint(model4)

model5 <- lm(data$salary ~ data$course + (prof.code))
summary(model5)
confint(model5)

# Years, courses, and profession