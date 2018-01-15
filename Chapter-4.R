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
mean(lda.pred$class == dir.2005)

# Fitting Quadratic Discriminant Analysis
qda.fit <- qda(Direction ~ Lag1+Lag2, data=Smarket, subset=train)
print(qda.fit)

qda.pred <- predict(qda.fit, newdata = trainData, type="response")
# Accuracy
mean(qda.pred$class == dir.2005)

# Using KNN
library(class)
set.seed(0)
knn.fit <- knn(train = Smarket[train, c(2,3)], 
               test = Smarket[!train, c(2,3)],
               cl= Smarket[train, 9],
               k = 3)

table(knn.fit, dir.2005)
mean(knn.fit == dir.2005)


# Using the Caravan dataset
dim(Caravan)  # Dataset has 5822 observations and 86 predictors

# Because the Caravan dataset has variables that are measured on different scales
# i.e. AGE in years and SALARY in dollars and a difference of 1 year vs difference
# of $1 is enormous, we should scale all the variables so that they have 
# mean of zero and variance of one
scaled.x <- scale(Caravan[,-86])

test <- 1:1000
train.x <- scaled.x[-test,]
train.y <- Caravan$Purchase[-test]
test.x <- scaled.x[test,]
test.y <- Caravan$Purchase[test]

for(i in 1:5){
  set.seed(1)
  caravan.knn <- knn(train = train.x,
                     test = test.x,
                     cl = train.y,
                     k = i)
  ta <- table(caravan.knn, test.y)
  print(ta)
  # Misclassification Rate = 11.6%
  print(paste("Misclass: ", mean(test.y != caravan.knn)))
  print(paste("Correct Classification: ", ta[2,2]/sum(ta[2,])))
}


