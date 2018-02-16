library(ISLR)
library(randomForest)
library(gbm)
library(rpart)
library(partykit)
library(MASS)
library(ggplot2)
library(caret)
library(doParallel)
library(data.table)
#### 7
cl = makePSOCKcluster(4)
registerDoParallel(cl, cores=4)

rf.fit =  foreach(ntree = seq(85, 510, 85)) %dopar%{
            library(caret)
            library(randomForest)
            library(MASS)
            inTraining = createDataPartition(Boston$medv, p = 0.7)
            inTesting = list(Test1 = rownames(Boston[-inTraining$Resample1,]))
            
            fitControl = trainControl(search="grid",
                                      allowParallel = T,
                                      index = inTraining)
            
            train(medv ~ ., data = Boston,
                 method= "rf",
                 trControl = fitControl,
                 tuneGrid = data.frame(mtry = 1:13),
                 ntree = ntree)
          }
stopCluster(cl)

res.df = data.frame(ntree = as.factor(rep(seq(85,510,85), 13)),
                    mtry = rep(1:13, 6),
                    rmse = c(rf.fit[[1]]$results$RMSE,
                             rf.fit[[2]]$results$RMSE,
                             rf.fit[[3]]$results$RMSE,
                             rf.fit[[4]]$results$RMSE,
                             rf.fit[[5]]$results$RMSE,
                             rf.fit[[6]]$results$RMSE)
                    )

ggplot(res.df, aes(mtry, rmse)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label = round(rmse, 2)), vjust=-0.6, size = 3, color="blue") +
  scale_x_continuous(breaks = 1:13) +
  facet_wrap(~ntree) +
  labs(y = "RMSE", title= "RMSE Vs mtry for various \"ntree\"")

rm(list=ls())
# Comments
# As the plot above suggests, the best model among the parameters used is
# the random forest model with "ntree" = 170 and "mtry" = 5.
# Also, an interesting thing to note is that a bagged model is with mtry = 13
# and for trees less than 510 it oscillates between 2.9 - 3.2 and the minimum
# for a bagged model is with 510 trees.
#
# finalModel object from "caret" is based on the best performance on testing set

## Problem 8
# Using the Carseats dataset from ISLR package for regression.
inTraining = createDataPartition(Carseats$Sales, p = 0.75, list=F)

rpart.tree = rpart(Sales ~ ., data = Carseats, subset = inTraining)
pred = predict(rpart.tree, newdata = Carseats[-inTraining,])

mean((Carseats$Sales[-inTraining] - pred)^2)
rpart.tree
plot(as.party(rpart.tree))
plotcp(rpart.tree, upper="splits")
rpart.tree$variable.importance
rpart.tree$cptable

# Comments
# With the full tree without any pruning MSE = 4.95
# The full tree has 16 internal nodes including the root and 17 leaves and uses
# 8 variables, Income and Urban are not used in the tree.
#
# Using cross-validation select the best model and select pruning parameters.
# Pruning parameters for cp are used from the range of the above method.
training = createDataPartition(Carseats$Sales, p = 0.75, list=F)
fitTrain = trainControl(method = "cv",
                        number = 10,
                        p = 0.8)
set.seed(0)
rp.fit = train(Sales ~ ., data = Carseats[training,],
               method ="rpart",
               trControl = fitTrain,
               metric = "RMSE",
               maximize = F,
               tuneLength = 13)

rp.fit$bestTune
fm = rp.fit$finalModel
plot(as.party(fm))
pred1 = predict(rp.fit, newdata = Carseats[-training,])

mean((pred1 - Carseats$Sales[-training])^2)

# Based on 10 fold cross-validation and training on 75% of the data and testing
# on 25% of the data. 
# The full model with 16 splits results in a MSE of 4.45037 and 
# the reduced mdodel with 6 splits (nodes) and 7 leaves gives a very comparable
# MSE of 4.474901 and cp = 0.02597716
# So, the reduced model does give a similar performing model with much less 
# complexity using only 3 variables instead of 8 in the full model and 6 splits
# in the reduced as opposed to 15 in the full model.
# Variables used for the regression tree are:
# ShelveLocation, Price, Age
#
#
# Using Bagging for the Carseata dataset.
set.seed(1)
cs.bag = randomForest(x = Carseats[training,-1], y = Carseats[training, 1],
                      xtest = Carseats[-training, -1], ytest = Carseats[-training,1],
                      mtry = 10, importance = T, keep.forest = T)
varImpPlot(cs.bag)
cs.bag

# Partial plots for the most important variables
partialPlot(cs.bag, pred.data = Carseats[training,-1], x.var = "ShelveLoc", plot = T)
partialPlot(cs.bag, pred.data = Carseats[training,-1], x.var = "Price", plot = T)
partialPlot(cs.bag, pred.data = Carseats[training,-1], x.var = "CompPrice", plot = T)
rm(list=ls())
# Test set MSE of Bagging with 500 trees results an Test_MSE = 2.75 with an
# Test_R-squared = 67.6%
set.seed(0)
training = createDataPartition(Carseats[,1], p = 0.75, list = F)

cl = makePSOCKcluster(16)
registerDoParallel(cl, cores=8)
set.seed(1)
cs.rf = train(x = Carseats[,-1], y = Carseats[,1],
              subset = training,
              tuneGrid = data.frame(mtry = 1:10),
              method = "rf", ntree = 300, importance = T,
              trControl = trainControl(search = "grid", allowParallel = T,
                                       savePredictions = T))
stopCluster(cl)
rm(cl)

plot(cs.rf)
cs.rf$bestTune
varImpPlot(cs.rf$finalModel)
cs.rf$finalModel
cs.rf$results

# 10 Fold Cross-validation suggests the best Test_MSE = 2.71 with mtry = 7
# As the number of predictors to split by increases, the error rate decreases.
# This is the case in this dataset because only 2 of the variables are deemed
# important and as the probability of them being in the model increases, the
# MSE goes down. It is only because of these two variables that causes a major
# decrease in MSE and increase in node purity.

