### Assignment #10
setwd("/Users/michaelnapoleon/coursea/statsOneCourse/Lab10")

# If necessary, install packages
install.packages("psych")
install.packages("aod")
install.packages("QuantPsyc")

setwd("/Users/michaelnapoleon/coursea/statsOneCourse/Lab10")

library(psych)
library(aod)
library(QuantPsyc)


### Question 1
### What is the median population age for the countries which voted to take action against global warming? (round to 2 decimal places)
data <- read.table("stats1-datafiles-Stats1.13.HW.10.txt", header = T)
action <- subset(data, change == 1)

describe(action)
# 35.78

### Question 2
### Run a logistic regression including all predictor variables. Which predictors are significant in this model?
lrfit <- glm(data$change ~ data$age + data$educ + data$gdp + data$co2, family = binomial)
summary(lrfit)

# age and educ

### Question 3
### What does the negative value for the estimate of educ means?

# all

### Question 4
### What is the confidence interval for educ, using profiled log-likelihood? 
### (round to 2 decimal places, and give the lower bound first and the upper bound second, separated by a space)

confint(lrfit) # CIs using profiled log-likelihood (default for logistic models)


#-31.13 -3.03

### Question 5
### What is the confidence interval for age, using standard errors? 
### (round to 2 decimal places, and give the lower bound first and the upper bound second, separated by a space)

confint.default(lrfit) # CIs using standard errors

#0.09 0.65

### Question 6
### Compare the present model with a null model. What is the difference in deviance for the two models? (round to 2 decimal places)
with(lrfit, null.deviance - deviance) #difference in deviance for the two models
with(lrfit, df.null - df.residual) #df for the difference between the two models
with(lrfit, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = FALSE)) #p-value

# 16.30

### Question 7
### How many degrees of freedom are there for the difference between the two models?

#4

### Question 8
### Is the p-value for the difference between the two models significant?

# Yes

### Question 9
### Do chi-squared values differ significantly if you drop educ as a predictor in the model?
wald.test(b = coef(lrfit), Sigma = vcov(lrfit), Terms = 3) #educ

# Yes

### Question 10
### What is the percentage of cases that can be classified correctly based on our model?

ClassLog(lrfit, data$change)

# 81%