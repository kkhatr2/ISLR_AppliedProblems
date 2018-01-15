library(ISLR)
library(ggplot2)
library(mvtnorm)
library(MASS)

# Lab Chapter 4
glm.fits <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                data=Smarket, family=binomial(link = logit))
summary(glm.fits)

glm.pred <- ifelse(predict(glm.fits, type="response") > 0.5, "Up", "Down")
table(glm.pred, Smarket$Direction)

train <- Smarket$Year < 2005
trainData <- Smarket[!train,]
dir.2005 <- Smarket$Direction[!train]

glm.fits <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                data=Smarket, family=binomial(link = logit), subset = train)

glm.probs <- predict(glm.fits, newdata = trainData, type="response")

glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")

table(glm.pred, dir.2005)
mean(glm.pred == dir.2005)

# Using Linear Discriminant Analysis
lds.fit <- lda(Direction ~ Lag1+Lag2, data=Smarket, subset=train)
lda.pred <- predict(lds.fit, newdata = trainData)

table(lda.pred$class, dir.2005)





