library(ggplot2)
library(ISLR)
library(MASS)
library(car)

x = seq(from = -2, to = 2, length.out = 100)
e = rnorm(100, mean=0, sd=1)

y = 2 + 3 * x + e

data = data.frame(y=y, x=x, e=e)

ggplot(data, aes(x, y)) +
  geom_point() +
  geom_abline(slope = 3, intercept = 2, color="red") +
  geom_smooth(method="lm")

credit <- read.csv("http://www-bcf.usc.edu/~gareth/ISL/Credit.csv",
                     sep=',', header=T)
advert <- read.csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv",
                     header = T, sep = ",")

ad_sales <- lm(sales ~ .-X, data=advert)
summary(ad_sales)

ad_sales <- lm(sales ~ TV + radio, data=advert)
summary(ad_sales)

plot(fitted(ad_sales), rstudent(ad_sales))

ad_sales <- lm(sales ~ TV*radio, data=advert)
summary(ad_sales)
plot(fitted(ad_sales), rstudent(ad_sales))

lm.gender <- lm(Balance ~ Gender, data=credit)
summary(lm.gender)


fit = lm(mpg ~ horsepower, data=Auto)
polyfit = lm(mpg ~ poly(horsepower, 2), data=Auto)

r <- range(Auto$horsepower)
pdata = data.frame(horsepower = seq(from=r[1], to=r[2], by=5))

d = predict(fit, newdata = pdata)
p = predict(polyfit, newdata= pdata)

pdata$linear <- d
pdata$polynomial <- p

ggplot(pdata, aes(horsepower, polynomial)) +
  geom_line( color='orange') +
  geom_line(aes(horsepower, linear), color = "blue") +
  geom_point(data=Auto, aes(horsepower, mpg), shape=".")


plot(x=fit$fitted.values, resid(fit))
plot(x=polyfit$fitted.values, resid(fit))


#########################################################################
# Lab: Linear Regression

ggplot(Boston, aes(lstat, medv)) +
  geom_point(shape = 'o') +
  geom_smooth(method="lm")

lm.fit <- lm(medv ~ lstat, data = Boston)
summary(lm.fit)

coef(lm.fit)
confint(lm.fit)
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="confidence")
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="prediction")

fit <- data.frame(pred = predict(lm.fit), rs = resid(lm.fit), 
                  rst = rstudent(lm.fit), hii = hatvalues(lm.fit))

ggplot(fit, aes(pred, rs)) +
  geom_point()

ggplot(fit, aes(pred, rst)) +
  geom_point()

plot(fit$hii)

vif(lm.fit)

poly.fit = lm(medv ~ poly(lstat,2), data=Boston)
summary(poly.fit)

###############################################################################
# Applied Problem 8
###############################################################################
a <- lm(mpg ~ horsepower, data = Auto)
summary(a)
predict(a, data.frame(horsepower=98), interval='prediction')
# Comments
# 1: Since the horsepower range is between 46-230 and the coefficient is < 1
#    suggests that the relationship between mpg and horsepower is not that 
#    strong, although the rlationship is . Also, the R^2 suggests that horsepower explains only about 60% of
#    the variation in mpg.
# Based on this model, a car with 98 horsepower will achieve 24.47 mpg on average
# and this prediction with 95% confidence under repeated observations will lie 
# between (14.81, 34.12)mpg.
ggplot(Auto, aes(horsepower, mpg)) +
  geom_point(shape='o') +
  geom_abline(intercept = coef(a)[1], slope = coef(a)[2], color="blue",
              size=1)
# Diagnostics 
# The resudual plot suggests a non-linear relationship
# i.e. the variance is not constant over the domain of predictor variable.
# Thus, the errors are not normally distributed with constant variance.
resp <- data.frame(pred = predict(a), rst = rstudent(a))
# Based on the delete 1 residuals, rstudent, there are multiple outliers
ggplot(resp, aes(pred, rst)) +
  geom_point() +
  geom_abline(intercept = c(-2,2), slope=c(0,0), color='red')
# There are multiple leverage points
# Thus a linear model is not adequate to fit the data.
plot(hatvalues(a))
abline(h = 4/nrow(Auto), col="red")




