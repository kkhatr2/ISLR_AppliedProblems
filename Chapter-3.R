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

###############################################################################
# Applied Problem 9 (multiple Linear Regression)
###############################################################################
library(corrgram)
pairs(Auto)
cor(Auto[,-9])
corrgram(Auto)

# C: Multiple Regression Model
fit.mul <- lm(mpg ~ .-name, data=Auto)
summary(fit.mul)
# Based on multiple regression, horsepower is no longer significant.
# The model does suggest that as the year increases by one unit, mpg also
# increases by 0.75 units.
# Now trying some other models with significant variables and their interactions
# with non-significant variables.
fit.mul1 <- lm(mpg ~ displacement+weight+year+origin+horsepower+
                 weight:horsepower, data=Auto)
summary(fit.mul1)
# Now displacement is insignificant and horsepower is significant along with
# its interaction with weight.
# Trying another interaction of displacement and cylinders
fit.mul2 <- lm(mpg ~ displacement+weight*horsepower+year+origin+
                 displacement*cylinders, data=Auto)
summary(fit.mul2)
# With displacement, cylinders and their interaction, the model suggests that
# all these are non-significant.
# Plotting mpg VS displacement does show a non-linear decreasing relationship.
ggplot(Auto, aes(cylinders, mpg))+ geom_point()
ggplot(Auto, aes(displacement, mpg))+ geom_point()

# Thus trying a transformation of displacement
fit.mul3 <- lm(mpg ~ weight+year+origin, data=Auto)
shapiro.test(rstudent(fit.mul3))
summary(fit.mul3)
confint(fit.mul3)
# Log and sqrt transformationsof non-significant variables are not enough to 
# help explain mpg.
# Thus the best model that explains ~86% of the variation in mpg includes
# weight, horsepower their interaction with year and origin.
# Even then, the additional variables produce a lot of noise and lead to an 
# excess number of leverage points. 
par(mfrow=c(2,2))
plot(fit.mul3)

# GLHT on the model without interaction and with interaction terms and variables
# is considered significantly different from fit.mul3.
fit4 <- lm(mpg ~ weight*horsepower+year+origin, data=Auto)
summary(fit4)
anova(fit.mul3, fit4)

resp <- data.frame(pred = predict(fit.mul3), rst = rstudent(fit.mul3),
                   hii = hatvalues(fit.mul3))

ol <- subset(resp, resp$rst > 3, select=c("rst", "pred"))

ggplot(resp, aes(pred, rst)) +
  ylim(-4.5,4.5) +
  geom_point() +
  geom_point(data=ol,aes(pred, rst), color="red") +
  geom_text(data=ol, aes(pred, rst), label=rownames(ol), vjust = -0.6, size=3) +
  geom_abline(intercept = c(-3,3), slope=c(0,0), color="orange",
              linetype="dotted", size=1.2) +
  labs(x="Predicted", y="R-Student", title="Predicted Vs R-Student",
       subtitle="Points in Red are outliers")


lev <- subset(resp, resp$hii > 2 * length(coef(fit.mul3))/nrow(Auto))

ggplot(resp, aes(x=as.numeric(rownames(resp)), y=hii)) + 
  geom_point() +
  geom_point(data=lev, aes(x=as.numeric(rownames(lev)), y=hii), color='red') +
  geom_abline(intercept=2 * length(coef(fit.mul3))/nrow(Auto), slope=0,
              color="orange") +
  geom_text(data=lev, aes(as.numeric(rownames(lev)), y=hii), 
            label=rownames(lev), vjust=-0.6, size=3) +
  labs(x="Index", y="Hat Diag", title="Leverage Points")















