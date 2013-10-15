### Assignment No. 2

### load packages and data file
install.packages("psych")
install.packages("sm")

library(psych)
library(sm)

### working directory setup
#getwd()
#setwd("C:/Users/mnapoleo/stats101/Lab2")

# Read data into a dataframe called impact
data <- read.table("Stats1.13.HW.02.txt", header = T)data


### Question 1
### How many rows of data are in the data file?
nrow(data)


### Question 2
### What is the name of the dependent variable?
SR


### Question 3
### What is the mean of SR across all subjects?
mean(data$SR)

#12.65525


### Question 4
### What is the variance of SR across all subjects?
sd(data$SR)

#standard deviation squared = 6.55


### Question 5
### What is the mean of SR for all subjects at pretest?
pre=subset(data, data$time=="pre")
mean(pre$SR)

# 12.02083


### Question 6
### What is the standard deviation of SR for all subjects at posttest?
post=subset(data, data$time=="post")
sd(post$SR)

# 2.449128


### Question 7
### What is the median of SR for all subjects at posttest?
describe(post$SR)
median(post$SR)

# 13.5


### Question 8
### Which group has the highest mean at posttest?
describeBy(post, post$condition)

# DS


### Question 9
### Which one best approximates a normal distribution?
pre.wm = subset(pre, pre$condition=="WM")
post.wm = subset(post, post$condition=="WM")
pre.pe = subset(pre, pre$condition=="PE")
post.pe = subset(post, post$condition=="PE")
pre.ds = subset(pre, pre$condition=="DS")
post.ds = subset(post, post$condition=="DS")
par(mfrow = c(2,3))
hist(pre.wm$SR)
hist(post.wm$SR)
hist(pre.pe$SR)
hist(post.pe$SR)
hist(pre.ds$SR)
hist(post.ds$SR)

#Post test WM


### Question 10
### Which group showed the biggest gains in SR?
mean(post.wm$SR)-mean(pre.wm$SR)
mean(post.pe$SR)-mean(pre.pe$SR)
mean(post.ds$SR)-mean(pre.ds$SR)

# DS


 