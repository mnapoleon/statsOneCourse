### Assignment #9


### Question 1
### What is the class of Haste and Prime in R?
class(data$Haste)
class(data$Prime)

#integer

### Question 2
### After converting Haste and Prime to factors, run an ANOVA 
### with both Haste and Prime as independent variables. Is the effect of Haste significant?
data$Haste <- factor(data$Haste)
data$Prime <- factor(data$Prime)

data.model <- aov(data$Helping ~ data$Haste * data$Prime)
summary(data.model)

#Yes

### Question 3
### Is the effect of Prime significant?

#Yes

### Question 4
### Is the interaction significant?

#Yes

### Question 5
### Save the ANOVA summary in a table and run 
### Tukey's pairwise comparison on all group means. 
### Do each level of Haste significantly differ from one another?

TukeyHSD(data.model)

#No

### Question 6
### What is the partial eta-squared value for the effect of Haste? (round to 2 decimal places).
etaSquared(data.model, anova=T)

#0.40

### Question 7
### What is the partial eta-squared value for the interaction? (round to 2 decimal places).

#0.18

### Question 8
### Let's now run simple effects of Prime at each level of Haste. At which level of 
### Haste is the effect of Prime significant?
data.haste1 <- subset(data, data$Haste == "1") 
data.haste2 <- subset(data, data$Haste == "2") 
data.haste3 <- subset(data, data$Haste == "3")

aov.haste1 <- aov(data.haste1$Helping ~ data.haste1$Prime)
summary(aov.haste1) 
aov.haste2 <- aov(data.haste2$Helping ~ data.haste2$Prime)
summary(aov.haste2) 
aov.haste3 <- aov(data.haste3$Helping ~ data.haste3$Prime)
summary(aov.haste3) 

#Early

### Question 9
### What is the partial eta-squared value for the effect of Prime when 
### people were early? (round to 2 decimal places).



etaSquared(aov.haste1, anova=T)

#0.59


### Question 10
### Which one of the following statements best illustrates the main 
### finding of the study?

# a - People are more likely to help others after being primed to do so if they are early
