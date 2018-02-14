library(rpart)
library(ISLR)
library(ggplot2)
library(MASS)
library(partykit)

################################################################################
#### Classification trees
################################################################################
# If Sales are more than 8 is success
High = ifelse(Carseats$Sales <= 8, "No", "Yes")
# Attach the binary variable to original data
Carseats$High = High

# The resulting binary separation is not balanced
# No = 236; Yes = 164
ggplot(Carseats, aes(High)) +
  geom_bar()

# Try out tree method with the whole dataset
tree.carseats = rpart(High ~ .-Sales, Carseats, method="class")
print(tree.carseats)
plotcp(tree.carseats, upper = "splits")
pred = predict(tree.carseats, newdata = Carseats, type = "class")
table(pred, High = Carseats[,12]) # Misclassification error = 0.1525

# Splitting the data into 50:50
set.seed(1)
idx = sample(1:nrow(Carseats), size=nrow(Carseats)/2, replace = F)

tree.carseats = rpart(High ~ .-Sales, data=Carseats, method="class", subset = idx)
print(tree.carseats)
plot(tree.carseats)
text(tree.carseats, use.n = T)

# Predicting on the test indices
pred = predict(tree.carseats, newdata = Carseats[-idx,], type = "class")
table(pred, High = Carseats[-idx, 12]) # Misclassification error = 0.285

# Pruning the tree
# Size of the tree is the depth
# For the full dataCP suggests that a tree with depth 8 gives the best error rate
plotcp(tree.carseats, upper = "size")
printcp(tree.carseats)
tree.pr = prune(tree.carseats, cp = 0.053)

plot(tree.pr)
text(tree.pr, use.n = T)
pred = predict(tree.pr, newdata=Carseats[-idx,], type="class")
a = table(pred, High = Carseats[-idx,12])
sum(diag(a))/sum(a) # Misclassification error after pruning = 0.255
# Thus pruning the tree produced a silpler tree with better prediction accuracy.
rm(list=ls())
################################################################################
#### Regression trees
################################################################################
set.seed(1)
# Doing a 50:50 split for training and testing the dataset
idx = sample(1:nrow(Boston), size = nrow(Boston)/2)

# Using the partykit glmtree method
tree.boston = glmtree(medv ~ lstat | ., data=Boston, subset = idx, family = gaussian)
summary(tree.boston)
AIC(tree.boston)
BIC(tree.boston)

plot(tree.boston)
plot(tree.boston, terminal_panel = NULL)

pred = predict(tree.boston, newdata = Boston[-idx,], type = "response")

1 - mean((pred - Boston$medv[-idx])^2)/mean((Boston$medv[-idx] - mean(Boston$medv[-idx]))^2)

pred.tr = predict(tree.boston, newdata = Boston[idx,], type="response")

1 - mean((pred.tr - Boston$medv[idx])^2)/mean((Boston$medv[idx] - mean(Boston$medv[idx]))^2)
# Model based regression tree performs significantly better than a regression
# tree that creates splits by exhaustive selection of SSE using all variables
# Model based Regression tree TEST R-SQUARED = 0.785
# Pruned Regression tree TEST R-SQUARED = 0.7004

# Using rpart package
tree.rp.boston = rpart(medv ~ ., data = Boston, subset = idx, method = "anova")
print(tree.rp.boston)
plot(as.party.rpart(tree.rp.boston))

# Prediction on the default dataset
pred = predict(tree.rp.boston, newdata = Boston[-idx,])
1 - mean((pred - Boston[-idx,"medv"])^2) / mean((Boston[-idx,"medv"] - mean(Boston[-idx,"medv"]))^2)
# Test R-sq = 0.706 using the full tree.

# Pruning the tree
# Cp suggests that a tree with number of splits = 6 gives the low error
plotcp(tree.rp.boston, upper = "splits")
printcp(tree.rp.boston)

tree.rp.prune = prune.rpart(tree.rp.boston, cp = 0.016523)
plot(as.party.rpart(tree.rp.prune))

pred = predict(tree.rp.prune, newdata = Boston[-idx,])
1 - mean((pred - Boston[-idx,"medv"])^2) / mean((Boston[-idx,"medv"] - mean(Boston[-idx,"medv"]))^2)
# A tree with one less split produces about the same test R-sq (0.70)as the 
# full tree, thus the tree is simpler and offers the same prediction as the 
# full tree.

###############################################################################
######### Bagging and Random Forest
###############################################################################
library(randomForest)
# Performing Bagging on the Boston dataset.
# since bagging uses only 2/3 of the training data for model fitting,
# I will split the data into 70:30 for testing and training
set.seed(1)
idx = sample(1:nrow(Boston), nrow(Boston) * 0.7, replace = F)

bag.boston = randomForest(medv ~ ., data=Boston, subset = idx, ntree = 60,
                         mtry = ncol(Boston) - 1, importance = T)
summary(bag.boston)
bag.boston

yhat.bag = predict(bag.boston, newdata = Boston[-idx,])
plot(yhat.bag, Boston$medv[-idx])
abline(0, 1)

# Bagging with 500 trees produced a test R-Squared = 0.818
# based on 70:30 split
1 - sum((yhat.bag - Boston$medv[-idx])^2) / sum((Boston$medv[-idx]-mean(Boston$medv[-idx]))^2)
# MSE of predicted "medv" is 5.5 lower than mean total sum of squares.
mean((yhat.bag - Boston$medv[-idx])^2)
mean((Boston$medv[-idx]-mean(Boston$medv[-idx]))^2)

#####
# Random Forest
#####
# Growing a random forest proceedes in the same way as bagging but, random
# forest takes a smaller value for "mtry" with p/3 for regressionand sqrt(p) for
# classification. I'm not using exactly p/3 here.
set.seed(1)
rf.boston = randomForest(medv ~ ., data=Boston, subsest = idx, mtry = 6,
                         importance = T)
rf.boston
rf.pred = predict(rf.boston, newdata = Boston[-idx,])
plot(rf.pred, Boston$medv[-idx])
abline(0,1,col="red")
1 - mean((rf.pred - Boston$medv[-idx])^2)/mean((Boston$medv[-idx]-mean(Boston$medv[-idx]))^2)
mean((rf.pred - Boston$medv[-idx])^2)
# Amazingly, random forest predicts "medv" with 97% accuracy for the test data.
# and the MSE = 1.77 i.e. the variance is very low.
importance(rf.boston)
varImpPlot(rf.boston)
# Variance Importance gives 2 columns of importance measures.
# 1. %IncMSE: Mean decrease of accuracy in predictions on the out of bag
#             samples, when the variable is excluded from the model.
# 2. IncNotePurity: Total Decrease in node impurity from splits over that variable.
# Read above comments like a statistician would think.

#######
### Boostin
#######
library(gbm)

set.seed(1)
boost.boston = gbm(medv ~ ., data=Boston[idx,], distribution = "gaussian",
                   n.trees = 5000, shrinkage = 0.01, interaction.depth = 2,
                   bag.fraction = 0.66, train.fraction = 0.7,
                   cv.folds = 10, verbose = F, n.cores = 4)

summary(boost.boston)

# Partial plots: These illustrate marginal effect of the selected variables on
#                the response after integrating out the other variables.
# These plots show marginal effect of the selected variable to the response.
# In this case, as the number of rooms "rm" increase, "medv" increases and,
# as "lstat," lower socio economic status increases median house value "medv"
# goes down. Which is intuitive foe all general purposes.
par(mfrow=c(1,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")
par(mfrow=c(1,1))

yhat.boost = predict(boost.boston, newdata = Boston[-idx,], n.trees = 5000)
1 - mean((yhat.boost-Boston$medv[-idx])^2)/mean((Boston$medv[-idx]-mean(Boston$medv[-idx]))^2)
mean((yhat.boost-Boston$medv[-idx])^2)

# Gradient boosting in this case gives worse results than random forest but 
# about the same as boosting. Doing a grid search on the parameter space could
# give better results.
# Below results are on test data:
# Boosting R-Square = 0.858
# Random Fore R-Square = 0.979




