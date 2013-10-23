### Assignment #5

#install.packages("psych")
#install.packages("ggplot2")

library(psych)
library(ggplot2)

data <- read.table("Stats1.13.HW.04.txt", header = T)

### Question 1
### Run a regression model with salary as the outcome variable and years of experience as the predictor variable. 
### What is the 95% confidence interval for the regression coefficient? Type your answer exactly as it appears 
### in R but include only two decimal places (for example, if the 95% confidence interval is -1 to +1 then type -1.00 1.00)
model1 <- lm(data$salary ~ data$years)
summary(model1)
confint(model1)

#  4930.124  6345.48

### Question 2
### Run a regression model with salary as the outcome variable and courses as the predictor variable.
### What is the 95% confidence interval for the regression coefficient?
model2 <- lm(data$salary ~ data$courses)
summary(model2)
confint(model2)

# 560.0886   872.0908

### Question 3
### Run a multiple regression model with both predictors and compare it with both the model from Question 1 
### and the model from Question 2. Is the model with both predictors significantly better than:
model3 <- lm(data$salary ~ data$years + data$courses)
summary(model3)
confint(model3)

anova(model1)
anova(model2)
anova(model1, model3)
anova(model2, model3)

# both


### Question 4
### Run a standardized multiple regression model with both predictors. Do the confidence interval values 
### differ from the corresponding unstandardized model?
model3.z <- lm(scale(data$salary) ~ scale(data$years) + scale(data$courses))
summary(model3.z)
confint(model3)
confint(model3.z)

#Yes

### Question 5
### What function could you use to take a random subset of the data?

#sample

### Question 6
### Run the following command in R: set.seed(1). Now take a random subset of the original data so 
### that N=15. Is the correlation coefficient between salary and years of experience in this sample higher or 
### lower than in the whole data set?
set.seed(1)
data.15 <- data[sample(nrow(data), 15), ]
cor.test(data$salary, data$years)
cor.test(data.15$salary, data.15$years)

#Lower

### Question 7
### Take a subset of the original data from row 51 to 70. What is the percentage of variance explained by 
### a multiple regression model with both predictors (Provide your result with no decimal place)
data.subset <- data[51:70, ]
model4 <- lm(data.subset$salary ~ data.subset$years + data.subset$courses)
summary(model4)

#85

### Question 8
### Using model comparison, which model provides the best fit for the subsetted data from Question 7?
model1.subset <- lm(data.subset$salary ~ data.subset$years)
model2.subset <- lm(data.subset$salary ~ data.subset$courses)
model3.subset <- lm(data.subset$salary ~ data.subset$years + data.subset$courses)
summary(model1.subset)
summary(model2.subset)
summary(model3.subset)

# model3.subset

### Question 9
### What is the correlation between the salary values predicted by the multiple regression model 
### and the actual salary scores in the subsetted data? (Provide your result rounded to 2 decimal places)
data.predicted <- fitted(model3.subset)
cor.test(data.predicted, data.subset$salary)

#0.92


### Question 10
### Compute the correlation between the scores predicted by the multiple regression model 
### and the residuals from the same model. Is the correlation statistically significant?
data.subset$e <- resid(model3.subset)
cor.test(data.predicted, data.subset$e)

# No