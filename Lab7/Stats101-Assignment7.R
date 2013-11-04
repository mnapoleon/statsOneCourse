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
model1 <- lm(data$extra ~ data$happy)
summary(model1)
confint(model1)

# 3.14680  *

### Question 2
### What is the correlation between extraversion and diversity of life experience?
model2 <- lm(data$extra ~ data$diverse)
summary(model2)
confint(model2)

# 3.17016  *

### Question 3
### What is the correlation between diversity of life experience and happiness?
model3 <- lm(data$diverse ~ data$happy)
summary(model3)
confint(model3)

# 2.00633  *

### Question 4
### What percentage of variance in happiness is explained by extraversion?
model4 <- lm(data$happy ~ data$extra)
summary(model4)
confint(model4)

# 4%  

### Question 5
### What percentage of variance in happiness is explained by a model with both 
### extraversion and diversity of life experience as predictors?
model5 <- lm(data$happy ~ data$extra + data$diverse)
summary(model5)
confint(model5)

# 7%

### Question 6
### What is the 95% confidence interval for the regression coefficient for extraversion 
### when it is the only predictor of happiness?

# 2.82 3.48  *

### Question 7
### What is the 95% confidence interval for the regression coefficient for extraversion 
### when it and diversity of life experience are both predictors of happiness?

# 0.02 0.43  *

### Question 8
### What is the unstandardized regression estimate of the indirect effect?
model.ALL <- sobel(data$happy, data$diverse, data$extra) 
model.ALL

# 0.03  *

### Question 9
### What is the z-value of the Sobel test?
model.ALL <- sobel(data$happy, data$diverse, data$extra) 
model.ALL

# 1.87  *

### Question 10
### Do these analyses suggest full mediation, partial mediation, or no mediation?

# no mediation  *