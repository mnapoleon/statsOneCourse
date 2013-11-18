setwd("C:/Users/mnapoleo/statsOneCourse/Lab9")

install.packages("pysch")
install.packages("car")
install.packages("lsr")

library(psych)
library(car)
library(lsr)

AB <- read.table("datafiles-Stats1.13.Lab.09.txt", header=T)

leveneTest(AB$errors ~ AB$driving * AB$conversation)

AB.model <- aov(AB$errors ~ AB$driving * AB$conversation)
summary(AB.model)

AB1 <- subset(AB, AB$driving == "Easy")
AB2 <- subset(AB, AB$driving == "Difficult")

aov.AB1 <- aov(AB1$errors ~ AB1$conversation)
summary(aov.AB1)

aov.AB2 <- aov(AB2$errors ~ AB2$conversation)
summary(aov.AB2)

etaSquared(aov.AB1, anova=T)
etaSquared(aov.AB2, anova=T)

TukeyHSD(aov.AB1)
TukeyHSD(aov.AB2)