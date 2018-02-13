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
tree.boston = glmtree(medv ~ ., data=Boston, subset = idx, family = gaussian)
summary(tree.boston)
AIC(tree.boston)
BIC(tree.boston)

plot(tree.boston)
plot(tree.boston, tp_args = list(cdplot = TRUE))
plot(tree.boston, terminal_panel = NULL)

pred = predict(tree.boston, newdata = Boston[-idx,], type = "response")

1 - mean((pred - Boston$medv[-idx])^2)/mean((Boston$medv[-idx] - mean(Boston$medv[-idx]))^2)

pred.tr = predict(tree.boston, newdata = Boston[idx,], type="response")

1 - mean((pred.tr - Boston$medv[idx])^2)/mean((Boston$medv[idx] - mean(Boston$medv[idx]))^2)

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