library(ISLR)
library(boot)
library(ggplot2)
library(MASS)

# Leave One Out Cross Validation (LOOCV)
# LOOCV results in a stable model as the MSE that is generated is only from one
# observation that was left out. Thus, LOOCV may not lead to an approx. unbiased
# test error. Approximately unbiased because almost the entire dataset is used to
# fit the model, thus the model will find a good estimate of the pattern of
# response. And, with the one obs. that was left out provide an unbiased estimate
# of test MSE. The flip side is that the test MSE is based on a single observation
# and any overfitting will not be caught.

# For demonstration, in the Auto dataset mpg is not a linear function of
# horsepower. So, we fit a polynomial model with various degrees and compute
# the test MSE using LOOCV.
res <- data.frame(degree = 1:10, mse = rep(NA, 10))
for(i in 1:10){
  poly.fit <- lm(mpg ~ poly(horsepower, i), data = Auto)
  si <- sum((residuals(poly.fit)/(1-hatvalues(poly.fit)))^2)
  res[i,2] <- si / nrow(Auto)
  rm(si, poly.fit)
}
rm(i)

ggplot(res, aes(degree, mse)) + 
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = seq(1, 10, by=1)) +
  labs(x="Polynomial Degree", y="MSE", title="LOOCV")

# K-fold cross validation has an intermediate effect to add a little bias and 
# reduce the variance, the bigger the k-fold validation the lesser the bias and
# thus increased variance. LOOCv has the least bias and the highest variance
# amond the resulting validation MSE.

################################################################################
##### Lab 
################################################################################
set.seed(1)
train = sample(nrow(Auto), 196)                            # Split the data 50:50
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)# Train the model
lm.test.pred <- predict(lm.fit, newdata = Auto)[-train]    # Selecting only the 
                                                           # test predictions
mse.test <- mean((Auto$mpg[-train] - lm.test.pred)^2)
# Test MSE = 26.14
# To predict and calculate MSE in one line without creating a new variable
mean((Auto$mpg - predict(lm.fit, newdata = Auto))[-train]^2)

# Computing test MSE for a polynomial model
poly.fit <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
# Test MSE for the polynomial Model is 19.82, about 24% smaller than linear model
mse.test <- mean((Auto$mpg - predict(poly.fit, newdata = Auto))[-train]^2)

set.seed(1)
train = sample(nrow(Auto), 313)                            # Split the data 80:20
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)# Train the model
mean((Auto$mpg - predict(lm.fit, newdata = Auto))[-train]^2)
# Test MSE with 20% of the data is 27.72 for linear model

# Computing test MSE for a polynomial model
poly.fit <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
# Test MSE for the polynomial Model is 20.96, about 24% smaller than linear model
mse.test <- mean((Auto$mpg - predict(poly.fit, newdata = Auto))[-train]^2)


# Splitting the data into 80:20 or 50:50 has similar effect on the test MSE
# i.e. test MSE reduces by about 24% in favor for a quadratic model.
# ------------------------
# Crossvalidation and fitting using maximum likelihood and the glm() function
# cv.glm() is part of the boot library
glm.fit <- glm(mpg ~ horsepower, data = Auto)
set.seed(0)
cv.err <- cv.glm(Auto, glm.fit) # Not specifying the argument "K" gives LOOCV
cv.err$delta

# Using LOOCV for multiple polynomial degree models
cv.err = rep(0, 5)
for(i in 1:5){
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto, family=gaussian)
  cv.err[i] <- cv.glm(Auto, glm.fit)$delta[1]
  rm(glm.fit)
}
plot(cv.err, type="b", xlab="Degree", ylab="MSE", main="LOOCV")
# From above, the LOOCV and mean MSE for the model with polynomials ranging
# from 1 - 5 shows a sharp reduction for the quadratic model.
# Also, LOOCV is the more unbiased estimate of MSE than k-fold cross-validation.
# cv.glm() does not use the formula for LOOCV.

# Using the bootstrap
alpha.fn <- function(data, idx){
  X = data$X[idx]
  Y = data$Y[idx]
  return ((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

boot(Portfolio, alpha.fn, R = 1000)

# Comparing regression coefficients std. error, as obtained from the regression
# method to the bootstrap.
boot.fn = function(data, index){
  return (coef(lm(mpg ~ poly(horsepower, 2), data=data, subset=index)))
}
boot(data=Auto, statistic = boot.fn, R=1000)
# Based on the bootstrap:
# standard error for intercept = 0.86, slope = 0.007
# As opposed to the linear regression standard errors for
# intercept = 0.72, slope = 0.006
summary (lm(mpg ~ horsepower ,data=Auto))$coef

# Since the quadratic model is better than linear
# We expect the bootstrap std.errors to be similar to the regression std. errs
boot(Auto, boot.fn, R= 1000)
summary (lm(mpg ~ poly(horsepower, 2) ,data=Auto))$coef


###############################################################################
###### Applied Problem 5
###############################################################################
# Splitting the Default dataset into 80:20 and measuring the test error
set.seed(1)
idx = sample(1:nrow(Default), nrow(Default) * 0.8, replace = F)
test <- Default[-idx,]
glm.fit <- glm(default ~ income+balance, data=Default, 
               family=binomial(link=logit), subset = idx)

test.pred <- ifelse(predict(glm.fit, newdata = test, type="response") > 0.5,
                "Yes", "No")

# Confusion matrix of prediction and true values
tr <- table(test.pred, test$default)
print(paste("Misclassification error on test data: ", 
            1 - sum(diag(tr))/sum(tr)))
# Test Prediction misclassification error is 0.026

# Manually doing split and validation to get the mean(mean(testError))
mErr <- rep(0, 3)
for(i in 1:3){
  set.seed(i)
  idx <- sample(1:nrow(Default), nrow(Default) * 0.8, replace=F)
  test <- Default[-idx,]
  glmFit <- glm(default ~ income+balance, data=Default, family=binomial(link=logit), 
                 subset=idx)
  test.pred <- ifelse(predict(glmFit, newdata = test, type="response") > 0.5,
                      "Yes", "No")
  mErr[i] <- mean(ifelse(test.pred == test$default, 0, 1))
  rm(idx, test, glmFit, test.pred)
}
mean(mErr)
# Splitting the data into test and train, then computing the mean and then
# the mean of those testdata means, value is 0.024. 
# Given the ratio of defaults in the test data were about 0.03, the 
# error is marginally better than a guess and, the model predicts considerably
# negatives more false.

###############################################################################
###### Applied Problem 6
###############################################################################
# SE(beta-hat) = sqrt(vcov(fit)[i,i]); for glm fits
glm.fit <- glm(default ~ income+balance, data=Default, family=binomial)
summary(glm.fit)

boot.fn = function(data, idx){
  b <- coef(glm(default ~ income+balance, data=data, family = binomial(link=logit),
                subset=idx))
  return(b)
}
set.seed(1)
boot(data = Default, statistic = boot.fn, R = 500)
# Using bootstrap sampling 500 times to compute standard errors
# The computed standard errors are very similar to the standard glm() results.
# I believe because the model fits well and the binomial is a parametric method
# The standard errors computed using either bootstrap or glm() are starkedly alike.

###############################################################################
###### Applied Problem 7
###############################################################################
sumError <- 0
for(i in 1:nrow(Weekly)){
  glm.fit <- glm(Direction ~ Lag1+Lag2, data=Weekly[-i,], family=binomial)
  loocv.pred <- predict(glm.fit, newdata = Weekly[i,], type="response")
  loocv.class <- ifelse(loocv.pred > 0.5, "Up", "Down")
  sumError <- sumError + ifelse(loocv.class != Weekly$Direction[i], 1, 0)
  rm(glm.fit, loocv.pred, loocv.class)
}
loocvMSE <- sumError / nrow(Weekly)
# Using the LOOCV the error rate is 0.45, which suggests that the model is
# correct only for a little bit more than half the time for an unknown value of
# Lag1 and Lag2.
# To use cv.glm we have to consturct our own cost function, 
# for a binomial, cost function in the help pages is good, otherwise
# cv.glm uses the default cost function of Mean Squared Loss.

###############################################################################
###### Applied Problem 8
###############################################################################
# Generating a simulated dataset
# Using the model Y = X - 2X^2 + e for response.
set.seed(1)
x = rnorm(100)
y = x - 2 * x^2 + rnorm(100)
data = data.frame(x = x, y = y)
rm(x, y)

# As expected data is a reverse parabola with maximum points clustered
# around x = 0 because, x is from a normal dist. with mean = 0 and sd = 1
ggplot(data, aes(x, y)) +
  geom_point()

cvErr = c(0,0,0,0)
for(i in 1:4){
  fit1 = glm(y ~ poly(x, i), data=data)
  set.seed(0)
  cvErr[i] = cv.glm(data, fit1)$delta[1]
}
plot(cvErr, type="b")

# Setting a random seed for LOOCV does not matter because it is inconsequential
# how we select which observation is left out. The point being only one 
# observation has to be left out and only once and, it doesn't matter which
# observation we leave out first. The cross-validated error will be the same.

# Because data was generated from a quadratic equation, we expect the quadratic
# term to be significant and none others. Least square fit reports just that
# from a summary() of the polynomial with 4th order term, both the 3rd and 4th 
# order terms are not significant as expected and only the linear and quadratic are.


###############################################################################
###### Applied Problem 9
###############################################################################
# Based on the Boston dataset from MASS library
#A Population estimate for "medv" is the mean of the medv variable
muhat_medv <-mean(Boston$medv)
# B
se_muhat_medv <- sd(Boston$medv) / sqrt(nrow(Boston))
# Mean estimate for medv = 22.53 with a standard error of 0.41

# C Using Bootstrap to construct the standard error
boot.fn = function(data, idx){
  return(mean(data$medv[idx]))
}

b = boot(Boston, boot.fn, R=500)
# Computing Standard Error using the formula or bootstrap yeilds the same result.

# D
# Based on the bootstrap standard error and the z-value of 1.96
# The 95% CI for medv is:
c(b[[1]] - 1.96 * 0.22218, b[[1]] + 1.96 * 0.22218)
# T-test confidence interval
t.test(Boston$medv)$conf.int

# T-test 95% CI is more conservative, as in it is much wider than the bootstrap
# CI. This is because bootstrap is non-parametric and uses the data to generate
# standard errors whereas T-test is parametric and is associated with area under
# a curve for respective degrees of freedom, is parametric and its CI is 
# computed based on the quantile value which, based on the sample size could be
# much different than 1.96.

# E
# Estimate for the median = 21.2, 50% quantile
median(Boston$medv)

# There is no popular formula for computing std. err for the median
# so, using bootstrap
boot.fn = function(data, idx){
  return(median(data$medv[idx]))
}

boot(Boston, boot.fn, R=500)
# Bootstrap SE for the median is 0.38
# The median interval does not includ the mean which suggests tht the data
# is skewed. Since the median is smaller than the mean, the data is left skewed.