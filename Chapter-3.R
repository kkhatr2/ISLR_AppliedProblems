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



