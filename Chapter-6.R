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
rm(regfitFull, regSummary)

# The above methods were using all the available data, so the results are
# not reflective of external data and model accuracy is not directly deducable.
# So splitting the data 80:20 for training and testing.
set.seed(1)
idx = sample(1:nrow(hitters), size = 0.8*nrow(hitters), replace = F) # 210 training

regFull = regsubsets(Salary ~ . , data = hitters[idx,], nvmax = 19)

# Since regsubsets does not have a predict method we will write a predict
# method and supply regsubsets object, newdata and the id to calculate test MSE
predict.mse = function(object, newdata, id, ...){
  # The call has each argument to the regsubsets function indexed.
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  # get names of coefficient variables
  xvars = names(coefi)
  # matrix multiplication of X * beta
  yPred = mat[,xvars] %*% coefi
  return(mean((newdata$Salary - yPred)^2))
}

predVal = rep(NA, 19)
for(i in 1:19){
  predVal[i] = predict.mse(regFull, hitters[-idx,], i)
}
which.min(predVal)
rm(predVal)

# The above was only testing each model with the test set, but to get a better
# answer for for selecting a model, we have to do cross validation.
# So, cross validation has to be done on each of the 19 model 10 times for a 10
# fold cross validation with training and validation set.
k = 10
set.seed(10)
folds = sample(1:k, size = nrow(hitters), replace = T)
cv.err = matrix(NA, nrow = k, ncol = 19, dimnames = list(NULL, paste(1:19)))

for(i in folds){
  best.fit = regsubsets(Salary ~ . , data=hitters[folds != i,], nvmax = 19)
  
  for(j in 1:19){
    cv.err[i,j] = predict.mse(best.fit, hitters[folds == i,], j)
  }
  rm(best.fit)
}
rm(i, j, folds, k)
# Each column represents a model with k variables
# Averaging for each column gives the best cross validation MSE of each model
# with k variables.
# Model with k = 10 variables is the best model based on cross validated and 
# averaged errors.
mean.cv.err = apply(cv.err, 2, mean)
plot(mean.cv.err)

# Now find the best model with 10 variables
best10 = regsubsets(Salary ~ ., data=hitters, nvmax = 19)
plot(coef(best10, id=10)[-1])








