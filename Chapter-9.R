library(e1071)
library(ggplot2)
library(ROCR)
library(ISLR)
library(caret)

# conceptual exercise 3
data = data.frame(x.1 = c(3,2,4,1,2,4,4),
                  x.2 = c(4,2,4,4,1,3,1),
                  y = c(rep("Red", 4), rep("Blue", 3)))

ggplot(data, aes(x.1, x.2)) +
  geom_point(aes(color=y)) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype=2) +
  geom_abline(slope = 1, intercept = -1, color = "red", linetype=2) +
  geom_abline(slope = 1, intercept = -0.5, color = "black", size = 1) +
  geom_abline(slope = 0.7, intercept = 0.5, color="purple", size = 1, linetype = 5)

# The Hyperplane in this case is a line for two random variables X1 and X2. Thus,
# b0 + b1 * X1 + b2 * X2 = 0
# -0.5 + 0.61 * X1 0.61 * X2 = 0
# Above equating got by the sum(b's^2) = 1 and the fact that in this case
# b1 = b2 and the exact value for b1 = b2 = sqrt(3/8)
#
# Thus, if we code "Red" = 1 and "Blue" = -1, then the classification rule is:
# b0 + b1 * X1 + b2 * X2 > 0 is "Red" and
# b0 + b1 * X1 + b2 * X2 < 0 is "Blue"
#
# Marign for the maximal margin hyperplane for the given data is:
# Margin is the distance from the maximal seprating hyperplane to its support
# vectors. Thus the margin for the given data set is: 1/2 = 0.5
#
# Support vectors for the maximal margin classifier are:
# For Red: (1,2), (3,4)
# For Blue: (2,2), (4,4)
#
# The 7th observation is not a support vector thus its movement will not change
# how many support vectors are requited to seperate the given data.
#
# On the plot above, the purple dashed line is not a maximal separating hyperplane
# and, any point within the margins of the maximal separating hyperplane will 
# lead to the maximal separating hyperplane NOT separable.

################################################################################
#### Applied Problem 4
################################################################################
set.seed(2)
train = sample(1:200, 100)
x = matrix(c(rnorm(200) + rnorm(200)^2 - rnorm(200)^3, 
             rnorm(200) + rnorm(200)^2), ncol = 2)
x[1:100,] = 1.5 * (x[1:100,]) + 2
x[101:200,] = x[101:200,] - 2
y = c(rep(1, 100), rep(-1,100))

data = data.frame(x = x, y = as.factor(y))
ggplot(data, aes(x.1,x.2)) +
  geom_point(aes(color = y))

# Linear kernel
linear.svm = svm(y  ~ ., data=data[train,], kernel = "poly", d = 1)
plot(linear.svm, data[train,])
summary(linear.svm)

linear.pred = predict(linear.svm, newdata = data[-train,])
linear.tab = table(data$y[-train], linear.pred)
linear.misclass = 1 - sum(diag(linear.tab))/sum(linear.tab)

# Polynomial kernel
poly.svm = svm(y ~ ., data=data[train,], kernel = "poly", d = 3)
plot(poly.svm, data[train,])
summary(poly.svm)

poly.pred = predict(poly.svm, newdata = data[-train,])
poly.tab = table(data$y[-train], poly.pred)
poly.misclass = 1 - sum(diag(poly.tab))/sum(poly.tab)

# Radial kernel
radial.svm = svm(y ~ ., data = data[train,], kernal = "radial", gamma = 0.5, cost = 1)
plot(radial.svm, data[train,])
summary(radial.svm)

radial.pred = predict(radial.svm, newdata = data[-train,])
radial.tab = table(data$y[-train], radial.pred)
radial.misclass = 1 - sum(diag(radial.tab)) / sum(radial.tab)

# Results
res = data.frame(method = c("Linear", "Quadratic", "Radial"),
                 misclass = c(linear.misclass, poly.misclass, radial.misclass))

# Based on the results for this simulated dataset, Quadratic kernel gave the 
# worse misclassification error of the test data followed by the linear and
# radial kernel was the best.
# This has been consistent for all generated datasets.
ggplot(res, aes(reorder(method, -misclass), misclass)) +
  geom_bar(aes(fill = method), stat = "identity") +
  labs(x = "Method", y = "Misclassification Error",
       title = "Misclassification Error of Test Data") +
  theme(legend.position = "none")

rm(list = ls())
###############################################################################
#### Problem 7, using the Auto data set to classify high or low mileage
###############################################################################
auto = Auto[,-c(1, 9)]
auto$mpg = as.factor(ifelse(Auto$mpg > median(Auto$mpg), 1, -1))

set.seed(123)
train = sample(1:nrow(auto), nrow(auto)*0.7)
pairs(auto)
# Trying the linear kernel
set.seed(1)
linear.tune = tune("svm", mpg ~ ., data = auto[train,], kernel = "poly", d = 1,
                   ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                                 gamma = c(0.5, 1, 2, 3, 4)))

summary(linear.tune)
summary(linear.tune$best.model)

linear.pred = predict(linear.tune$best.model, newdata = auto[-train,])
linear.tab = table(auto$mpg[-train], linear.pred)
linear.misclass = 1 - sum(diag(linear.tab)) / sum(linear.tab)

plot(linear.tune$best.model, auto[train,], year ~ weight)

# Comments:
# Best cross validated error rate using the training data is 0.07 with 
# cost = 1 and gamma = 0.5.
# A lower value of cost suggests that the margin is highly non-linear and uses
# 65 support vectors out of 274 total which is about 24% of the training data.
# Thus with the linear kernel, a high misclassification error should be expected.
# BUt, surprisingly the misclassification rate is 10%! with the test data.

# Now trying a polynomial kernel with various degree, cost and gamma params.
set.seed(1)
poly.tune = tune("svm", mpg ~ ., data = auto, kernel = "poly",
                 ranges = list(d = c(2,3,4,5),
                               cost = c(0.1,1,10,100,1000),
                               gamma = c(0.5, 1, 2, 3, 4)))
summary(poly.tune)
summary(poly.tune$best.model)
poly.pred = predict(poly.tune$best.model, newdata = auto[-train,])
poly.tab = table(auto$mpg[-train], poly.pred)
poly.misclass = 1 - sum(diag(poly.tab)) / sum(poly.tab)

plot(poly.tune$best.model, auto[train,], origin ~ displacement)

# Comments:
# Best cross validated error rate using the training data is 0.07 with
# degree = 3, cost = 1 and gamma = 0.5
# A cubic fit suggests that the margin is highly non-linear and uses 79 support
# vectors which is about 29% of the training data. Whcih also may suggest that
# the margins of cubic polynomial kernel are not as rigid as the linear kernel.
# As a comparison cross validated error for the linear kernel is also the same
# and the best linear kernel model uses only 65 support vectors. This suggests
# that the cubic polynomial kernel does not offer any more information about the 
# response and may overfit.
# But the polynomial misclassification error on the testing data is much better
# at 0.04 which more than twice as better as the linear kernel. Thus, the cubic
# polynomial kenel does give a better predicting model even though the cross
# vadiated error is the same.

# Now trying the radial kenel
radial.tune = tune("svm", mpg ~ ., data = auto[train,], kernel = "radial",
                   ranges = list(gamma = c(0.5, 1,2,3,4),
                                 cost = c(0.1, 1, 10, 50, 100)))
summary(radial.tune)
summary(radial.tune$best.model)

radial.pred = predict(radial.tune$best.model, newdata = auto[-train,])
radial.tab = table(auto$mpg[-train], radial.pred)
radial.misclass = 1 - sum(diag(radial.tab)) / sum(radial.tab)

# Comments:
# Again the cross validated errors are the same as the linear and polynomial
# kernel. 
# Crossvalidated best parameters for the radial kernel are:
# gamma = 1 and cost = 1.
# Although the cv errors are the same, radial kernel with the above paramaters
# use 144 support vectors, which is 52% of the training data. Suggesting that
# the radial kernel's margins are very irregular and the radial kernel possibly
# overfits the data.
# This overfit is reflected in the misclassification error of test data of 0.07.
# This error rate is slightly better than linear kernel but the complexity is 
# not worth only an improvement of 3%

res = data.frame(method = c("Linear", "Cubic", "Radial"),
                 misclass = c(linear.misclass, poly.misclass, radial.misclass))

# Based on the results for this dataset, cubic kernel gave the 
# best misclassification error of the test data followed by the radial and
# linear kernel.
ggplot(res, aes(reorder(method, -misclass), misclass)) +
  geom_bar(aes(fill = method), stat = "identity") +
  labs(x = "Method", y = "Misclassification Error",
       title = "Misclassification Error of Test Data (Auto data set)") +
  theme(legend.position = "none")

rm(list = ls())
###############################################################################
#### Problem 8, using the OJ data set from ISLR package
###############################################################################
set.seed(1)
train = sample(1:nrow(OJ), 800)

linear.svm = caclSVM(OJ, "linear", 1, train, list(d = 1, cost = 0.01))
linear.train.misclass = calcMisclass(OJ$Purchase[train],linear.svm$fitted)
linear.pred = predict(linear.svm, newdata = OJ[-train,])
linear.misclass = calcMisclass(OJ$Purchase[-train], linear.pred)

# With Cost = 0.01, i.e. a very wide margin the training error rate= 0.166 and
# the test error rate = 0.18. Because the training and testing error rates
# are not too different, the results are not too bad uinsg the linear kernel. 
# Although, the support vectors are 432 i.e. 54% of the training dat, thus
# the boundaries are very flexible and the linear kernel probably underfits the 
# data

linear.tune = tuneSVM(OJ, "linear", 1, train, 
                      list(cost = seq(0.01, 10, length.out = 10)))

# Cross validation does not produce any better results, the CV cost is still 0.01

# Using the Radial kernel
radial.svm = calcSVM(OJ, "radial", 1, train, list(gamma = 0.5, cost = 0.01))
radial.train.misclass = calcMisclass(OJ$Purchase[train], radial.svm$fitted)
radial.pred = predict(radial.svm, newdata = OJ[-train,])
radial.misclass = calcMisclass(OJ$Purchase[-train], radial.pred)

radial.tune = tuneSVM(OJ, "radial", 1, train, list(gamma= c(0.5,1,2,3,4), 
                                                   cost=seq(0.01, 10, length.out = 10)))

radial.tune.pred = predict(radial.tune$best.model, newdata = OJ[-train,])
radial.tune.misclass = calcMisclass(OJ$Purchase[-train], radial.tune.pred)

# Radial kernel gives a wose fit with cost = 0.01, the CV cost = 1.12 and
# the default and CV gamma = 0.5 with a miscalassification rate of the test
# data = 0.41 and 0.38 respectively for each cost.

# Polynomial kernel
poly.tune = tuneSVM(OJ, "poly", 1, train, 
                    list(d= 2, cost = seq(0.01, 10, length.out = 10)))
summary(poly.tune$best.model)
poly.pred = predict(poly.tune$best.model, newdata=  OJ[-train,])
poly.misclass = calcMisclass(OJ$Purchase[-train], poly.pred)

# The polynomial kernel produces about the same results as the linear kenel
# although the support vectors a less than when using the linear kernel.
# So, I will suggest the best model as the polynomila kernel SVM with
# degree = 2 and cost = 5.56


tuneSVM = function(data, kernel, responseIndex, traindIDX, params){
  nm = names(data)
  
  form = as.formula(paste(nm[responseIndex], " ~", 
                          paste(nm[-responseIndex], collapse = "+")))
  set.seed(1)
  myTune = tune("svm", form, data = data[traindIDX,],
                kernel = kernel, ranges = params)
  print(summary(myTune))
  return(myTune)
}

calcSVM = function(data, kernel, responseIndex, trainIDX, params){
  gamma = 0.5
  cost = 1
  d = 1
  if(kernel == "poly")
    d = params$d
  
  if(kernel == "radial")
    gamma = params$gamma
  
  if(!is.na(params$cost))
    cost = params$cost

  nm = names(data)
  
  form = as.formula(paste(nm[responseIndex], " ~", 
                          paste(nm[-responseIndex], collapse = "+")))
  my.svm = svm(form, data = data[trainIDX,], 
               kernel = kernel, cost = cost, gamma = gamma, d=d)
  print(summary(my.svm))
  return (my.svm)
}

calcMisclass = function(true, pred){
  tab = table(true, pred)
  mc = 1 - sum(diag(tab)) / sum(tab)
  return (mc)
}






