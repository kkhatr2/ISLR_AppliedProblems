library(ISLR)
library(randomForest)
library(gbm)
library(rpart)
library(partykit)
library(MASS)
library(ggplot2)
library(caret)
library(doParallel)

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

# Comments
# As the plot above suggests, the best model among the parameters used is
# the random forest model with "ntree" = 170 and "mtry" = 5.
# Also, an interesting thing to note is that a bagged model is with mtry = 13
# and for trees less than 510 it oscillates between 2.9 - 3.2 and the minimum
# for a bagged model is with 510 trees.
#
# finalModel object from "caret" is based on the best performance on testing set