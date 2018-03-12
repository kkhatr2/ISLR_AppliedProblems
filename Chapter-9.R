library(e1071)
library(ggplot2)
library(ROCR)

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
# worse misclassification error of the test data forllowed by the linear and
# radial kernel was the best.
# This has been consistent for all generated datasets.
ggplot(res, aes(reorder(method, -misclass), misclass)) +
  geom_bar(aes(fill = method), stat = "identity") +
  labs(x = "Method", y = "Misclassification Error",
       title = "Misclassification Error of Test Data") +
  theme(legend.position = "none")