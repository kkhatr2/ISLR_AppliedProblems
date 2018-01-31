library(ISLR)
library(glmnet)
library(leaps)
library(pls)

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

# Lasso does a worse job of identifying the underlying structure of the data.
# Although, lasso fails to identify underlying factors and chooses x5 and x7 but,
# it places a very low weight on x5 with a coefficient estimate = 0.065.
# Also the mse = 23.65 which is 10 times more than the best model from 
# best-subset selection.


