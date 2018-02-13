library(rpart)
library(ISLR)
library(ggplot2)

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

################################################################################
#### Regression trees
################################################################################





















