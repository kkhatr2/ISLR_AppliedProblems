library(ISLR)
library(pls)

####################################################################################
####### Lab 3, PCR (principal component regression) and PLS (Partial Least Squares)
####################################################################################
hitters = Hitters[!is.na(Hitters$Salary),]

set.seed(2)
pcr.fit = pcr(Salary ~ ., data=hitters, scale = T, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")

set.seed(0)
idx = sample(1:nrow(hitters), nrow(hitters) * 0.8, replace = F)

pcr.fit = pcr(Salary ~ ., data=hitters, subset = idx, scale = T, validation="CV")
validationplot(pcr.fit, val.type = "MSEP")
summary(pcr.fit)

pcr.pred = predict(pcr.fit, newdata = hitters[-idx,], ncomp = 7)
mean((pcr.pred - hitters$Salary[-idx])^2)

pcr.fit = pcr(Salary ~ ., data=hitters, sale=T, ncomp=8)
summary(pcr.fit)

# Partial Least Squares
set.seed(1)
pls.fit = plsr(Salary ~ ., data=hitters, scale=T, subset=idx, validaiton = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")

pls.pred = predict(pls.fit, hitters[-idx,], ncomp=2)
mean((pls.pred - hitters$Salary[-idx])^2)
1 - mean((pls.pred - hitters$Salary[-idx])^2) / 
  mean((hitters$Salary - mean(hitters$Salary))^2)