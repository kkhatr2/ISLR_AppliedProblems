library(ISLR)
library(boot)
library(ggplot2)

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
boot(data = Default, statistic = boot.fn, R = 500)
# Using bootstrap sampling 500 times to compute standard errors
# The computed standard errors are very similar to the standard glm() results.
# I believe because the model fits well and the binomial is a parametric method
# The standard errors computed using either bootstrap or glm() are starkedly alike.

###############################################################################
###### Applied Problem 7
###############################################################################



