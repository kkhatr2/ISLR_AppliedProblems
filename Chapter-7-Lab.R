library(ISLR)
library(ggplot2)
library(splines)
library(gam)
###################################################################################
######## Polynomial Regression and Step Functions
###################################################################################
fit = lm(wage ~ poly(age, 4), data=Wage)
summary(fit)

age.grid = data.frame(age = seq(from = min(Wage$age), to = max(Wage$age), by=2))
preds = predict(fit, newdata = age.grid, se=T)
age.grid$wage = preds$fit
age.grid$se = preds$se.fit

par(mfrow=c(1,2), mar=c(4.5,4.5,1,1), oma=c(0,0,4,0))
plot(Wage$age, Wage$wage, xlim = range(Wage$age), cex=0.5, col="darkgrey")
title("Degree-4 Polynomial", outer=T)
plot(age.grid$age, age.grid$preds, lwd=2, col="blue")
matlines(age.grid$age, cbind(age.grid$preds-2*age.grid$se, 
                             age.grid$preds+2*age.grid$se), lwd=1, col="blue",
         lty=3)
par(mfrow=c(1,1))

ggplot(data=Wage, aes(age, wage)) +
  geom_point(alpha= 0.1) +
  geom_line(data=age.grid, size=1, colour="red") +
  geom_ribbon(data=age.grid,aes(ymin = preds+2*se, ymax = preds-2*se), 
              alpha = 0.3, colour = "blue", fill = "blue")
  
# Logistic regression
# The I() function will create a binary vector with TRUE and FALSE on the fly.
fit1 = glm(I(wage > 250) ~ poly(age, 4), data=Wage, family=binomial(link=logit))
summary(fit1)
preds = predict(fit1, newdata = age.grid, se=T)

se.bands.logit = cbind(preds$fit - 2 * preds$se.fit, 
                       preds$fit + 2 * preds$se.fit)

age.grid$wage = 1 / (1 + exp(-preds$fit))
age.grid$sel = 1 / (1 + exp(-se.bands.logit[,1]))
age.grid$seu = 1 / (1 + exp(-se.bands.logit[,2]))

ggplot(age.grid, aes(age, wage)) +
  geom_line() +
  geom_ribbon(data=age.grid,aes(ymin = sel, ymax = seu), alpha = 0.3) +
  geom_rug(sides="bl")

# Step function regression
step.fit = lm(wage ~ cut(age, 4), data=Wage)
summary(step.fit)


###################################################################################
######## Splines
###################################################################################
sp.fit = lm(wage ~ bs(age, knots = c(25,40,60)), data=Wage)
summary(sp.fit)

rm(age.grid)
age.grid = data.frame(age=seq(18,80,2))
p = predict(sp.fit, newdata = age.grid, se=T)
age.grid$wage = p$fit
age.grid$se = p$se.fit

ggplot(age.grid, aes(age, wage)) +
  geom_point(data=Wage, alpha=0.1) +
  geom_line() +
  geom_ribbon(aes(ymin=wage-2*se, ymax=wage + 2*se), alpha=0.2, fill="green4")


# In the above equation we have specified the knots explicitly
# Basis splines have restrictions to be continuous at the knots and have
# smooth first and second derivatives
# So, there are 4 regions with a cubic in each which amounts to 4 * 3 = 12 d.f
# but because of restrictions at the knots i.e. 2 for each knot, continuous and
# smoothness of derivatives, 2 * 3 = 6
# We have 12 - 6 = 6 degrees of freedom
#
# So, for the bs() function we could have specified a bs(x, df=6) argument and
# R would have chosen uniform knots for us as in below
b = bs(Wage$age, df=6)
attr(b, which="knots")
matplot(Wage$age, b)

# Fitting a natural spline there are further restrions for edge cases
# i.e. linearity at the edges so to generate a natural spline basis for 3 knots
# we need 4 d.f.
n = ns(Wage$age, df=4)
attr(n, which="knots")
matplot(Wage$age, n)

sp.fit = lm(wage ~ ns(age, df=4), data=Wage)
summary(sp.fit)

rm(age.grid)
age.grid = data.frame(age=seq(18,80,length.out=32))

p = predict(sp.fit, newdata = age.grid, se=T)

age.grid$wage = p$fit
age.grid$se = p$se.fit

ggplot(age.grid, aes(age, wage)) +
  geom_point(data=Wage, alpha=0.1) +
  geom_line() +
  geom_ribbon(aes(ymin=wage-2*se, ymax=wage + 2*se), alpha=0.2, fill="green4")

# Smoothing Spline
# When specifying degrees of freedom the function determines what value of
# lambda leads to the specified degrees of freedom.
# For cross validation, LOOCV, it determines the best value of lambda for 
# minimum error.
fit1 = smooth.spline(Wage$age, Wage$wage, df=16)
fit2 = smooth.spline(Wage$age, Wage$wage, cv=T)

plot(fit1, col="red", type="l")
lines(fit2, col="blue", type='l')

# Local Regression
fit1 = loess(wage ~ age, data=Wage, span=0.2)
fit2 = loess(wage ~ age, data=Wage, span=0.5)

p = predict(fit1, data.frame(age=seq(18,80,2)))
dim(p$y)
age.grid$wage = p$y
  
plot(age.grid$age, age.grid$wage$age)

rm(list = ls())
###################################################################################
######## GAM
###################################################################################
# If using the same basis functions as in regression splines, we can just use
# the appropriate basis functions in "lm"
# In gam package, s() is for smoothing spline, "lo" is for lowess
fit = lm(wage ~ ns(age, df=5) + ns(year, df=4) + education, data=Wage)
plot.gam(fit)
summary(fit)

gam.m3 = gam(wage ~ s(age, 5) + s(year, 4) + education, data=Wage)
summary(gam.m3)
par(mfrow=c(1,3))
plot(gam.m3, se=T, col="blue")

par(mfrow=c(1,1))

gam.m1 = gam(wage ~ s(age, 5) + education, data=Wage)
gam.m2 = gam(wage ~ year + s(age, 5) + education, data=Wage)

# Anova shows that a linear predictor for "year" is sufficient
anova(gam.m1, gam.m2, gam.m3, test="F")

summary(gam.m2)

