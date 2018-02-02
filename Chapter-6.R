library(ISLR)
library(glmnet)
library(leaps)
library(pls)
library(MASS)
library(ggplot2)
library(tidyr)

###################################################################################
######### Exerciese 8
###################################################################################
set.seed(0)
x = rnorm(100)
eps = rnorm(100)
y = -1 + 0.5*x - 0.5*x^2 + x^3 + eps
data = data.frame(y = y, x1 = x, x2 = x^2, x3 = x^3, x4 = x^4,
                  x5 = x^5, x6 = x^6, x7 = x^7, x8 = x^8, x9 = x^9, x10 = x^10)
plot(x, y)

bestSubset = regsubsets(y ~ ., data = data, nvmax = 10)

summary(bestSubset)
plot(bestSubset, scale="bic")
plot(bestSubset, scale="Cp")
plot(bestSubset, scale="adjr2")
coef(bestSubset, id=3)

# The lowest BIC is -220 and it occurs for multiple models
# BIC best model with least variables includes: x2, x2, x3
# Lowest cp = 2.2 for model that      includes: x1, x2, x3
# Adj R^2 gives 0.90 for model that   includes: x2, x2, x3, there are more than 1
# Thus variables in the original model are the most important predictors.
# The coefficients are also quite similar to the original predictors (unknown model)


bestForward = regsubsets(y ~ ., data = data, method="forward")

plot(bestForward$rss, main="Model SSE Vs Model Index", ylab = "SSE", type = "l")

summary(bestForward)
plot(bestForward, scale ="bic")
plot(bestForward, scale ="Cp")
plot(bestForward, scale ="adjr2")
coef(bestForward, id=3)

# Best forward also gives the model with linear and cubed terms as one of the 
# best models. Also, BIC, Cp are the same and the coefficients are remarkably
# similar as obtained using best subset method.


# Lasso Regression, alpha = 1 for the lasso.
mm = model.matrix(y ~ ., data=data)[,-1]
set.seed(1)
cv.out = cv.glmnet(x = mm, y = data$y, alpha = 1, nfold=5)
summary(cv.out)
plot(cv.out)
predict(cv.out, type="coefficients", s = cv.out$lambda.1se)
mean((data$y - predict(cv.out, s="lambda.min", newx = mm, type="response"))^2)

# Lasso correctly shrinks all variables except the linear, quadratic and cubic
# to zero for this simulated dataset.
# Shrinkage is less at lambda = 0.0694 and the lasso coefficients do not
# entirely agree with the generating model parameters.

# 7.f
# using the model y = b0 + b1 * x^7 + err
# For this model, and a limited range of X, response y looks like a horizontal line
data$y = 1 + 2 * x^7 + eps
plot(x, data$y)

regfit = regsubsets(y ~ ., data=data, nvmax = 10)
summary(regfit)

plot(regfit, scale="bic")
plot(regfit, scale="Cp")
coef(regfit, id=1)
yhat = cbind(matrix(rep(1), 100, ncol=1), data$x7) %*% 
                matrix(coef(regfit, id=1), ncol=1)
mse = mean((data$y - yhat)^2)
mst = mean((data$y - mean(data$y))^2)

cbind(matrix(rep(1, 100), ncol=1), data$x7)

# Best subset selection using only x^7 suggests:
# The best 1 variable model is the one with x7, and x7 occurs in all subsequent
# models with increasing number of variables. Thus, best-subset has identified
# x7 as the most important variable.
# BIC and CP selection criteria also suggests the model with one variable i.e. x7
# is the best, for response generated using only x^7.
# The model accuracy is also apparent from the Mean Squared Error (0.91) and
# Mean Squared Total (22226.71).

# Checking if Lasso also shrinks all variables except x7 to 0
# alpha = 1 for Lasso
mm = model.matrix(y ~ ., data=data)[,-1]
lasso.fit = cv.glmnet(x = mm, y = data$y, alpha = 1)
plot(lasso.fit)
predict(lasso.fit, type="coefficients", s = "lambda.min")
mse = mean((data$y - predict(lasso.fit, type="response", s = "lambda.min",
            newx = mm))^2)
lasso.fit$lambda.min

# Lasso does a worse job of identifying the underlying structure of the data.
# Although, lasso fails to identify underlying factors and chooses x5 and x7 but,
# it places a very low weight on x5 with a coefficient estimate = 0.065.
# Also the mse = 23.65 which is 10 times more than the best model from 
# best-subset selection.

# Clear variables for problem 8
rm(y, data, lasso.fit, mm, regfit, y, yhat, eps, mse, mst, x)
################################################################################
### Problem 9
################################################################################
set.seed(123)
idx = sample(1:nrow(College), size = nrow(College) * 0.8)

MSTOTAL.TEST = mean((College$Apps[-idx] - mean(College$Apps[-idx]))^2)

# Ordinary Least Squares
lm.fit = lm(Apps ~ ., data=College, subset = idx)
testLM.mse = mean((College$Apps[-idx] - predict(lm.fit, newdata = College[-idx,]))^2)
lm.testr2 = 1 - testLM.mse / MSTOTAL.TEST

# Ridge
mm = model.matrix(Apps ~ ., data=College)[,-1]
ridge.fit = cv.glmnet(x = mm[idx,], y = College$Apps[idx], alpha = 0)
testRidge.mse = mean((College$Apps[-idx] - predict(ridge.fit,
                                                   newx = mm[-idx,], 
                                                   s = "lambda.min"))^2)
ridge.testr2 = 1 - testRidge.mse / MSTOTAL.TEST

# Lasso
lasso.fit = cv.glmnet(x = mm[idx,], y = College$Apps[idx], alpha = 1)
testLasso.mse = mean((College$Apps[-idx] - predict(lasso.fit,
                                                   newx = mm[-idx,], 
                                                   s = "lambda.min"))^2)
lasso.testr2 = 1 - testLasso.mse / MSTOTAL.TEST

# PCR
pcr.fit = pcr(Apps ~ ., data = College, subset = idx, scale = T)
summary(pcr.fit)

testPCR.mse = mean((College$Apps[-idx] -  predict(pcr.fit, newdata=College[-idx,],
                                                  type="response", ncomp=1:9))^2)
pcr.testr2 = 1 - testPCR.mse / MSTOTAL.TEST

# Like partial correlation plots
plot(pcr.fit, ncomp =1:9, which="test", newdata = College[-idx,])

# PLS
pls.fit = plsr(Apps ~ ., data = College, subset = idx, scale = T)
summary(pls.fit)

testPLS.mse = mean((College$Apps[-idx] -  predict(pls.fit, newdata=College[-idx,],
                                                  type="response", ncomp=1:9))^2)
pls.testr2 = 1 - testPLS.mse / MSTOTAL.TEST
rm(list=ls())
#Results
r2 = data.frame(method = c("Linear_Model","Ridge", "Lasso", "PCR", "PLS"),
                R2 = c(lm.testr2, ridge.testr2, lasso.testr2, pcr.testr2,
                        pls.testr2))
estimates = data.frame("Linear_Model" = coef(lm.fit),
                       "Ridge" = predict(ridge.fit, s = "lambda.min", type="coefficients")[,1],
                       "Lasso" = predict(lasso.fit, s = "lambda.min", type="coefficients")[,1],
                       "PCR" = as.numeric(coef(pcr.fit, ncomp=9, intercept = T)),
                       "PLS" = as.numeric(coef(pls.fit, ncomp = 9, intercept = T)))
estimates$est_names = rownames(estimates)
rownames(estimates) = 1:nrow(estimates)

ggplot(r2, aes(x = reorder(method, -R2), y = R2)) +
  geom_point(aes(size=R2), color="blue") +
  ylim(c(0.6, 0.9)) +
  geom_text(aes(label = round(R2, 2)), vjust = -1.0) +
  labs(x = "Method", y ="R-squared", title="R-squared Vs Method",
       subtitle = "College dataset from ISLR package") +
  theme(legend.position = "None")

# Converting from wideview to longview
d = gather(estimates, key=method, value=estimates, Linear_Model:PLS,
           factor_key = T)

ggplot(d, aes(method, estimates)) +
  geom_point(aes(color=method)) +
  labs(color = "Method", y = "Estimates",
       title = "Coefficient estimates for different methods",
       subtitle = "College dataset from ISLR package") +
  facet_wrap(~est_names, scales = "free_y", nrow = 6) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.x = element_blank())
  
# Comments
# Among all the models, the highest test r-squared is 87% which is achieved
# by OLS and Lasso, Ridge is slightly worse at 86% following PLS with 83%.
# Of all these methods performance on test data is worse by PCR.
# Thus, using Least Squares and Lasso model we can predict 87% of number of 
# applications received by universities.
#
# Lasso sets 4 variables (Top25perc, F.Undergrad, Books, perc.alumni) to 
# exactly 0. Although, Ridge and Least Squares estimates Top25perc and 
# perc.alumni to be highly important with coefficients > |3| and the other
# two to be less important with estimates less than |1|.
# PCR and PLS are worse at estimating these coefficients with values for 
# some that are > |250|, this discrepancy in estimates is also reflected
# by the test erro and test R-squared for these models.
#
# Thus based on the approach to select the simplest model that explains
# the data well, in this case with the given dataset and split of 80:20
# between training and test set, Lasso performs the best.
#
# Below is the model fit with the full dataset.
mm = model.matrix(Apps ~ ., data=College)[,-1]
lasso.fit = cv.glmnet(x = mm, y = College$Apps, alpha = 1, standardize = T)
plot(lasso.fit)
predict(lasso.fit, s = "lambda.1se", type="coefficients")
mean((College$Apps - predict(lasso.fit, s="lambda.min", newx = mm))^2)

# One caviat is that based on the full dataset, Lasso does not shrink any of
# the estimates to 0 for the minimum value of lambda.
# 
# Using the Generalized Linear Hypothesis test on model with minimum lambda
# as the full model and the model with lambda 1 standard error away as the 
# reduced model. (Reduced model only has 3 non-zero parameters and the 
# intercet)
# H0: Reduced and full model are the same
# H1: Reduced and full model are different
#
# Based on the F-test below the p-value is significant, thus we reject H0 and
# claim H1.
# Since, the test is significant, reduced and full models explain the response
# with significant difference. Thus we want the model that explains the response
# well, thus we choose the full model.
df.reduced = nrow(College) - 4
df.full = nrow(College) - 18
mse.reduced = sum((College$Apps - predict(lasso.fit, s="lambda.1se", newx = mm))^2) / df.reduced
mse.full = sum((College$Apps - predict(lasso.fit, s="lambda.min", newx = mm))^2) / df.full
f.statistic = (mse.reduced - mse.full)/(df.reduced - df.full) / mse.full
pf(f.statistic, df1 = (df.reduced - df.full), df2 = df.full)
rm(list=ls())
################################################################################
### Problem 10
################################################################################
n = 1000
eps = rnorm(n)
data = data.frame(x1 = rnorm(n), 
                  x2 = rnorm(n), 
                  x3 = rnorm(n),
                  x4 = rnorm(n), 
                  x5 = rchisq(n, df=20),
                  x6 = rnorm(n),
                  x7 = rf(n, df1 = 5, df2 = 20),
                  x8 = rnorm(n),
                  x9 = rnorm(n),
                  x10 = rt(n, df = 6), 
                  x11 = rgamma(n, 2, 1),
                  x12 = rbeta(n, 1,1), 
                  x13 = rlogis(n), 
                  x14 = rchisq(n, df=20),
                  x15 = rt(n, df = 6), 
                  x16 = rgamma(n, 2, 1),
                  x17 = rbeta(n, 1,1), 
                  x18 = rlogis(n), 
                  x19 = rcauchy(n),
                  x20 = rcauchy(n))

data$y = as.matrix(cbind(rep(1,n), data[,1:12]), ncol=13) %*%
                  matrix(c(1,2,0.9,-1,0.5, 6, -5, 10, -1.1, -7,3.3, -1, 1), 
                         ncol=1)+eps
set.seed(0)
idx = sample(1:n, 100)

best.subset = regsubsets(y ~ ., data[idx,], nvmax = 20)
summary(best.subset)

predict.mse = function(object, newdata, id, response, ...){
  # The call has each argument to the regsubsets function indexed.
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, data = newdata)
  coefi = coef(object, id = id)
  # get names of coefficient variables
  xvars = names(coefi)
  # matrix multiplication of X * beta
  yPred = mat[,xvars] %*% coefi
  return(mean((newdata[,response] - yPred)^2))
}

test.mse = rep(0, 20)
train.mse = rep(0, 20)
for(i in 1:20){
  test.mse[i] = predict.mse(best.subset, data[-idx,], i, "y")
  train.mse[i] = predict.mse(best.subset, data[idx,], i, "y")
}
pts = data.frame(variables = rep(1:20),
                 train.mse = train.mse, 
                 test.mse = test.mse)

min.labels = data.frame(var = c(which.min(pts$test.mse), which.min(pts$train.mse)),
                        mse = c(round(min(pts$test.mse),2), 
                                round(min(pts$train.mse),2)))

ggplot(pts, aes(variables)) +
  geom_line(aes(y = train.mse), colour = "green4") +
  geom_point(aes(which.min(train.mse), min(train.mse)), colour ="green4") +
  geom_line(aes(y = test.mse), colour = "orange") +
  geom_point(aes(which.min(test.mse), min(test.mse)), colour ="orange") +
  geom_text(data=min.labels, aes(var, mse, label = mse), 
                      vjust = -0.6, colour = c("orange","green4")) +
  theme(legend.position = "None")

coef(best.subset, id=11)

