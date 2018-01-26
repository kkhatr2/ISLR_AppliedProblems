library(ISLR)
library(leaps)

###################################################################################
########## Lab
###################################################################################

# Removing players without recorded Salary
hitters = Hitters[!is.na(Hitters$Salary),]

regfitFull = regsubsets(Salary ~ ., data=hitters)
summary(regfitFull)
# Summary Suggests that Hits CRBI and PutOUts are the most important variables
# because they occur in almost all the models.
# By default "regsubsets" computes only the best models with 8 variables
# we can changed it with the nvmax argument
# Below, asking regsubsets to generate 1 best model upto and including all the 19
# variables
regfitFull = regsubsets(Salary ~ ., data=hitters, nbest=2, nvmax = 19)
regSummary = summary(regfitFull)
# In the above model also the best variables are Hits, CRBI and PutOuts

# plotting model statistics
par(mfrow=c(2,2))
plot(1-regSummary$adjr2)
points(x=which.max(regSummary$adjr2), y=1-max(regSummary$adjr2), col="red")
plot(regSummary$rss)
points(x=which.min(regSummary$rss), y=min(regSummary$rss), col="red")
plot(regSummary$bic)
points(x=which.min(regSummary$bic), y=min(regSummary$bic), col="red")
plot(regSummary$cp)
points(x=which.min(regSummary$cp), y=min(regSummary$cp), col="red")

par(mfrow=c(1,1))
plot(regfitFull, scale="bic")
plot(regfitFull, scale="Cp")

# Based on BIC the best is a 6 variable model
# in the coef function, an additional argument of "id" is to be provided to 
# tell a model with how many coefficients is required.
# If the best 2 models were fit then to get coefficients of both the models
# id = 1 gives the first best model with 1 variable
# id = 2 gives the second best model with 1 variable
coef(regfitFull, id=11)
coef(regfitFull, id=12)


