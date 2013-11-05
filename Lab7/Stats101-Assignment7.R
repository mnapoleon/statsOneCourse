### Assignment # 7

# install.packages("psych")
# install.packages("ggplot2")
# install.packages("multilevel")
library(psych)
library(ggplot2)
library(multilevel)

data <- read.table("stats1-datafiles-Stats1.13.HW.07.txt", header = T)
describeBy(data) 
### Question 1
### What is the correlation between extraversion and happiness?
cor(data)

# 0.19

### Question 2
### What is the correlation between extraversion and diversity of life experience?

# 0.21

### Question 3
### What is the correlation between diversity of life experience and happiness?

# 0.21

### Question 4
### What percentage of variance in happiness is explained by extraversion?
model1 <- lm(data$happy ~ data$extra)
summary(model1)

# 4%  

### Question 5
### What percentage of variance in happiness is explained by a model with both 
### extraversion and diversity of life experience as predictors?
model2 <- lm(data$happy ~ data$extra + data$diverse)
summary(model2)

# 7%

### Question 6
### What is the 95% confidence interval for the regression coefficient for extraversion 
### when it is the only predictor of happiness?
confint(model1)

# .07 .48

### Question 7
### What is the 95% confidence interval for the regression coefficient for extraversion 
### when it and diversity of life experience are both predictors of happiness?
confint(model2)

# .02 .43

### Question 8
### What is the unstandardized regression estimate of the indirect effect?
model.ALL <- sobel(data$extra, data$diverse, data$happy) 
model.ALL

# .05

### Question 9
### What is the z-value of the Sobel test?
model.ALL <- sobel(data$extra, data$diverse, data$happy) 
model.ALL

# 1.88

### Question 10
### Do these analyses suggest full mediation, partial mediation, or no mediation?

# partial