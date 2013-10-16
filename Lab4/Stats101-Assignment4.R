### Assignment No. 4

# If necessary, install packages
#install.packages("psych")

# Load packages
library(psych)

# Read data into a dataframe called data
data <- read.table("Stats1.13.HW.04.txt", header = T)

### Question 1
### What is the correlation between salary and years of professional experience?
cor(data[2:4])

# 0.74


### Question 2
### What is the correlation between salary and courses completed?
cor(data[2:4])

# 0.54


### Question 3
### What is the percentage of variance explained in a regression model 
### with salary as the outcome variable and professional experience 
### as the predictor variable?
model1 <- lm(data$salary ~ data$years)
summary(model1)

# 55%

### Question 4
### Compared to the model from Question 3, would a regression model 
###  predicting salary from the number of courses be considered a 
### better fit to the data?
model2 <- lm(data$salary ~ data$courses)
summary(model2)

# No


### Question 5
### Now let's include both predictors (years of professional experience
### and courses completed) in a regression model with salary as the outcome.
### Now what is the percentage of variance explained?
model3 <- lm(data$salary ~ data$years + data$courses)
summary(model3)

# 65%


### Question 6
### What is the standardized regression coefficient for years of 
### professional experience, predicting salary?
model1.z <- lm(scale(data$salary) ~ scale(data$years))
summary(model1.z)

# 7.449e-01  or 0.74


### Question 7
### What is the standardized regression coefficient for courses completed, 
## predicting salary?
model2.z <- lm(scale(data$salary) ~ scale(data$courses))
summary(model2.z)

# 5.410e-01  or 0.54


### Question 8
### What is the mean of the salary distribution predicted by the model 
### including both years of professional experience and courses completed 
### as predictors? (with 0 decimal places)
data$predicted <- fitted(model3)
mean(data$predicted)

# 75426.44 or 75426

### Question 9
### What is the mean of the residual distribution for the model predicting 
### salary from both years of professional experience and courses completed? 
### (with 0 decimal places)
data$e <- resid(model3)
mean(data$e)

# -1.893208e-14  or 0

### Question 10
### Are the residuals from the regression model with both predictors normally 
### distributed?
data$e <- resid(model3)
hist(data$e)

# Yes