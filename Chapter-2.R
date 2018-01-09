library(ISLR)
library(ggplot2)
library(data.table)

setwd("F:/F-Documents/R/Code/ISLR")

ggplot(Wage, aes(age, wage)) + 
  geom_point(aes(alpha=0.2), color="gray") +
  geom_smooth(se=F) +
  labs(x = "Age", y="Wage")
  

ggplot(Wage, aes(year, wage)) +
  geom_point(color="gray", alpha=0.2) +
  geom_smooth(method = "lm") +
  labs(x="Year", y = "Wage")

ggplot(Wage, aes(as.numeric(education), wage)) +
  geom_boxplot(aes(fill=education)) +
  stat_boxplot(geom="errorbar", aes(group=education)) +
  theme(legend.position="None")

# Exercise 8
college = College
summary(college)
pairs(college[,1:10])

ggplot(college, aes(Private, Outstate)) +
  geom_boxplot() +
  geom_errorbar(stat = "boxplot")

# Colleges with more than 50% of the Top10Perc
# i.e Colleges with more than 50% of new students
# from top 10% of high school class
college$Elite = rep("No", nrow(college))
college$Elite[college$Top10perc > 50] <- "Yes"
college$Elite <- as.factor(college$Elite)
summary(college$Elite)

ggplot(college, aes(Elite, Outstate)) +
    geom_boxplot() +
    geom_errorbar(stat="boxplot")

ggplot(Auto, aes(horsepower, mpg)) +
  geom_point()

ggplot(Auto, aes(year, mpg)) +
  geom_point()

ggplot(Auto, aes(weight, mpg)) +
  geom_point()

ggplot(Auto, aes(mpg, displacement)) +
  geom_point()

ggplot(Auto, aes(mpg, cylinders)) +
  geom_point()











