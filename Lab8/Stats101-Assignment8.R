### Assignment #8
setwd("C:/Users/mnapoleo/statsOneCourse/Lab8")
install.packages("psych")
install.packages("car")
install.packages("lsr")
install.packages("reshape")
library(psych)
library(car)
library(lsr)
library(reshape)

data = read.table("stats1-datafiles-Stats1.13.HW.02.txt", header = T)
describeBy(data, data$time)

### Question 1
### Using a dependent t-test, is the difference between pre and post-test scores significant?
data.pre = subset(data, data$time == "pre")
data.post = subset(data, data$time == "post")
data.pre.out = describe(data.pre)
data.pre.out
data.post.out = describe(data.post)
data.post.out

t.test(data.pre$SR, data.post$SR, paired = T)

# Yes


### Question 2
### Create subsets for each training condition. Which group shows no difference between pre and post-test scores?
data.wm = subset(data, data$condition == 'WM')
data.wm.pre = subset(data.wm, data.wm$time == 'pre')
data.wm.post = subset(data.wm, data.wm$time == 'post')
data.pe = subset(data, data$condition == "PE")
data.pe.pre = subset(data.pe, data.pe$time == 'pre')
data.pe.post = subset(data.pe, data.pe$time == 'post')
data.ds = subset(data, data$condition == 'DS')
data.ds.pre = subset(data.ds, data.ds$time == 'pre')
data.ds.post = subset(data.ds, data.ds$time == 'post')

t.test(data.pe.pre$SR, data.pe.post$SR, paired = T)
t.test(data.wm.pre$SR, data.wm.post$SR, paired = T)
t.test(data.ds.pre$SR, data.ds.post$SR, paired = T)


# PE


### Question 3
### Which training group shows the largest effect size for the difference pre-test to post-test?

# DS


### Question 4
### Reshape the data into a wide format, and create a new variable for gain score. 
### Now subset the new dataframe based on the training conditions. Which comparison 
### between training conditions does not show a significant difference?
data.wide = cast(data, subject+condition~time)
data.wide$gain = data.wide$post - data.wide$pre
wm.wide = subset(data.wide, data.wide$condition == 'WM')
pe.wide = subset(data.wide, data.wide$condition == 'PE')
ds.wide = subset(data.wide, data.wide$condition == 'DS')

t.test(wm.wide$gain, pe.wide$gain, var.equal = T)
t.test(wm.wide$gain, ds.wide$gain, var.equal = T)
t.test(ds.wide$gain, pe.wide$gain, var.equal = T)


# None

### Question 5
### To compare the gain scores across all groups, we now turn to ANOVA. Is the homogeneity of 
### variance assumption violated?
leveneTest(data.wide$gain, data.wide$condition)

# No

### Question 6
### Run an ANOVA model on the gain scores as a function of training condition. Is the effect of condition significant?\
aov.model = aov(data.wide$gain ~ data.wide$condition)

summary(aov.model)

# Yes

### Question 7
### What is the corresponding eta-squared value? (round to 2 decimal places)
etaSquared(aov.model, anova=T)

# 0.34

### Question 8
### Are the eta-squared and partial eta-squared value different in this case?
etaSquared(aov.model, anova=T)

# NO

### Question 9
### Let's now run post-hoc comparisons (Tukey HSD). Which two groups do not significantly 
### differ from one another when considering gain scores?
TukeyHSD(aov.model)

# WM - DS


### Question 10
### Based on these data, which training condition should you choose to target some improvements in spatial reasoning?

# DS or WM