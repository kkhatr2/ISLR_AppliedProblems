library(ggplot2)
library(e1071)
library(ISLR)
library(ROCR)

set.seed(1)
x = matrix(rnorm(20*2), ncol = 2)
y = c(rep(-1, 10), rep(1, 10))
x[y == 1,] = x[y == 1,] + 1
plot(x, col=3-y)

dat = data.frame(x = x, y = as.factor(y))

# With a smaller cost, the  margins are wide and,
# with a larger cost, margins are narrow.
# i.e. if cost = 1 then 10 support vectors and cost = 10 then 7 support vectors
svmfit = svm(y ~ ., data=dat, kernel = "linear", cost = 10, scale = F,
             probability = T)

plot(svmfit, dat)

# Support Vectors observation numbers
svmfit$index

summary(svmfit)

# Using "tune()" to do cross-validation 
# Best model is fit and is returned from using tune()
set.seed(1)
tune.out = tune("svm", y ~ ., data = dat, kernel = "linear", 
              ranges = list(cost = c(0.001,0.01,0.1,1,5,10,100)))

summary(tune.out)

bestmod = tune.out$best.model

# Generating a test dataset for prediction based on the best model
xtest = matrix(rnorm(20 * 2), ncol=2)
ytest = sample(c(-1,1), 20, replace = T)
xtest[ytest == 1,] = xtest[ytest == 1,] + 1
testdata = data.frame(x = xtest, y = as.factor(ytest))

ypred = predict(bestmod, newdata = testdata)

# Confusion matrix of prediction and true values
# shows that only one observation is misclassified.
# So, a cost of 0.01 is good for this simulated dataset.
table(predict = ypred, truth = testdata$y)


# Now, further separating the two vectors in the original dataset.
# Further separating, we see that only one observation is on the wrong side
# so, we have a barely linearly separable observations.
x[y == 1,] = x[y == 1] + 0.5
plot(x, col = (y + 5)/2, pch = 19)

# fitting a svm with very large value of cost so that no observations are
# misclassified. 
# Recall: large cost = narrow margin.
dat = data.frame(x = x, y = as.factor(y))

svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 1e+5)
plot(svmfit, dat)
summary(svmfit)

# Support Vector Machine
# Using non-linear kernel
set.seed(1)
x = matrix(rnorm(200 * 2), ncol = 2)
x[1:100,] = x[1:100,] + 2
x[101:200,] = x[101:200,] - 2
y = c(rep(1, 150), rep(2, 50))
dat = data.frame(x = x, y = as.factor(y))

# Because of induced separation of the simulated dataset and, because of 
# intentionally overlapping the class labels. The class boundary is non-linear
ggplot(dat, aes(x[,1], x[,2])) +
  geom_point(aes(color = factor(y)))

# Splitting the data and fitting using the svm() function with radial kernel
# and, gamma = 1
# Results are indeed non-linear with high inaccuracy
set.seed(123)
train = sample(1:200, 100)
svmfit = svm(y ~ ., data=dat[train,], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat)
summary(svmfit)

# Now, using a higher cost so that the margins are narrow.
# The support vectors have reduced from 60 to 40 and the boundary is highly
# non-linear with pockets of classes instead of a contiguous boundary.
svmfit = svm(y ~ ., data=dat[train,], kernel = "radial", gamma = 1, cost = 1e5)
plot(svmfit, dat)
summary(svmfit)

# Using cross-validation to select the best values for gamma cost.
# With a cost = 1 and gamma = 0.5, cross-validation results are the lowest
set.seed(1)
tune.out = tune("svm", y ~ ., data=dat[train,], kernel = "radial",
                ranges = list(cost=c(0.1,1,10,100,1000),
                              gamma = c(0.5,1,2,3,4)))
summary(tune.out)
plot(tune.out$best.model, dat[train,])

# Predicting With the best model
# 31% observations are misclassified by this model
ypred = predict(tune.out$best.model, newdata = dat[-train,])
table(true = dat[-train,"y"], preict = ypred)

# Function to predict and plot ROC curve
library(pROC)
library(ROCR)
rocplot = function(pred, truth,...){
  predob = prediction(pred, truth)
  perf = performance(predob, "fpr","tpr")
  plot(perf, ...)
}
# Probabilities in the SVM case do not make sense because, whether the observation
# belongs to one class or the other is resolved by the sign of fitted observation
# if the sign is -ve then the observation belongs to one class and vice-versa.
# When "decision.values=TRUE" is used, predict() will give the fitted values
# with associated sign which we can use for the ROC plot.
bestMod = svm(y ~ ., data=dat[train,], kernel = "radial",
              gamma = 0.5, cost = 1, decision.values = T)
fitted = attributes(predict(bestMod, dat[train,], decision.values=T))$decision.values

rocplot(fitted, dat$y[train])

proc = roc(dat$y[train], as.vector(fitted))
plot(proc, main="Training Data")

# Preparing a ROC curve for the test data
fittedTest = attributes(
  predict(bestMod, dat[-train,], decision.values = T))$decision.values

procTest = roc(dat$y[-train], as.vector(fittedTest))
plot(procTest, main = "Test Data")

# Now using a multi class classification using SVM
# Appending new data and new class label to the above dataset
set.seed(1)
x = rbind(x, matrix(rnorm(50*2), ncol = 2))
y = c(y, rep(0, 50))
x[y == 0,] = x[y==0,] + 2
dat = data.frame(x = x, y = as.factor(y))

ggplot(dat, aes(x.1, x.2)) +
  geom_point(aes(color = factor(y)))

svmfit = svm(y ~ ., data=dat, kernel = "radial", cost = 10, gamma = 1)
plot(svmfit, dat)
summary(svmfit)









