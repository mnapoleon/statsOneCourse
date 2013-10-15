### Assignment No. 3

# load packages
# install.packages("psych")
# install.packages("gclus")
# install.packages("rgl")

# Load packages
library(psych)
library(gclus)
library(rgl)

# working directory setup
getwd()
setwd("C:/Users/mnapoleo/statsOneCourse/Lab3")

# load data file
data <- read.table("Stats1.13.HW.03.txt", header = T)


### Question 1
### What is the correlation between S1 and S2 pre-training?
cor(data$S1.pre, data$S2.pre)

# 0.4920231


### Question 2
### What is the correlation between V1 and V2 pre-training?
cor(data$V1.pre, data$V2.pre)

# 0.9038863


### Question 3
### With respect to the measurement of two distinct constructs, 
### spatial reasoning and verbal reasoning, the pattern of correlations pre-training reveals:
data$S.pre = (data$S1.pre + data$S2.pre)/2
data$V.pre = (data$V1.pre + data$V2.pre)/2
cor(data$S.pre, data$V.pre)

# Both

### Question 4
### Correlations from the control group could be used to estimate test/retest reliability.
### If so, which test is most reliable?
data.aer = subset(data, data$cond=="aer")
cor(data.aer$S1.pre, data.aer$S1.post) 
cor(data.aer$S2.pre, data.aer$S2.post) 
cor(data.aer$V1.pre, data.aer$V1.post) 
cor(data.aer$V2.pre, data.aer$V2.post) 

# V2


### Question 5
### Does there appear to be a correlation between spatial reasoning before training 
### and the amount of improvement in spatial reasoning?
data$S.pre = (data$S1.pre + data$S2.pre)/2
data$V.pre = (data$V1.pre + data$V2.pre)/2
data$S.post = (data$S1.post + data$S2.post)/2
data$V.post = (data$V1.post + data$V2.post)/2
data$Sgain = data$S.post - data$S.pre
data$Vgain = data$V.post - data$V.pre
cor(data$Sgain, data$Vgain)

# No

### Question 6
### Does there appear to be a correlation between verbal reasoning before training 
### and the amount of improvement in verbal reasoning?
cor(data$V.pre, data$Vgain)

# No


### Question 7
### Which group exhibited more improvement in spatial reasoning?
describeBy(data$Sgain, data$cond)

# des

### Question 8
### Create a color scatterplot matrix for all 4 measures at pre-test. 
### Do the scatterplots suggest two reliable and valid constructs?
prebase = cbind(data[3], data[4], data[7], data[8])
pre.r = abs(cor(prebase))
cpairs(prebase, order.single(pre.r), panels.colors = dmat.color(pre.r), gap=.5)

# Yes


### Question 9
### Create a color scatterplot matrix for all 4 measures at post-test. Do the scatterplots suggest two reliable and valid constructs?
postbase = cbind(data[5], data[6], data[9], data[10])
post.r = abs(cor(postbase))
cpairs(postbase, order.single(post.r), panel.colors = dmat.color(post.r), gap=.5)

# Yes


### Question 10
### What is the major change from pre-test to post-test visible on the color matrix?


#Variance


