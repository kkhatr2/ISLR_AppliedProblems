library(ISLR)
library(ggplot2)
library(mvtnorm)
library(MASS)
library(class)

# Lab Chapter 4
glm.fits <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                data=Smarket, family=binomial(link = logit))
summary(glm.fits)

glm.pred <- ifelse(predict(glm.fits, type="response") > 0.5, "Up", "Down")
table(glm.pred, Smarket$Direction)

train <- Smarket$Year < 2005
trainData <- Smarket[!train,]
dir.2005 <- Smarket$Direction[!train]

glm.fits <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                data=Smarket, family=binomial(link = logit), subset = train)

glm.probs <- predict(glm.fits, newdata = trainData, type="response")

glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")

table(glm.pred, dir.2005)
mean(glm.pred == dir.2005)

# Using Linear Discriminant Analysis
lds.fit <- lda(Direction ~ Lag1+Lag2, data=Smarket, subset=train)
lda.pred <- predict(lds.fit, newdata = trainData)

table(lda.pred$class, dir.2005)
mean(lda.pred$class == dir.2005)

# Fitting Quadratic Discriminant Analysis
qda.fit <- qda(Direction ~ Lag1+Lag2, data=Smarket, subset=train)
print(qda.fit)

qda.pred <- predict(qda.fit, newdata = trainData, type="response")
# Accuracy
mean(qda.pred$class == dir.2005)

# Using KNN
set.seed(0)
knn.fit <- knn(train = Smarket[train, c(2,3)], 
               test = Smarket[!train, c(2,3)],
               cl= Smarket[train, 9],
               k = 3)

table(knn.fit, dir.2005)
mean(knn.fit == dir.2005)


# Using the Caravan dataset
dim(Caravan)  # Dataset has 5822 observations and 86 predictors

# Because the Caravan dataset has variables that are measured on different scales
# i.e. AGE in years and SALARY in dollars and a difference of 1 year vs difference
# of $1 is enormous, we should scale all the variables so that they have 
# mean of zero and variance of one
scaled.x <- scale(Caravan[,-86])

test <- 1:1000
train.x <- scaled.x[-test,]
train.y <- Caravan$Purchase[-test]
test.x <- scaled.x[test,]
test.y <- Caravan$Purchase[test]

for(i in 1:5){
  set.seed(1)
  caravan.knn <- knn(train = train.x,
                     test = test.x,
                     cl = train.y,
                     k = i)
  ta <- table(caravan.knn, test.y)
  print(ta)
  # Misclassification Rate = 11.6%
  print(paste("Misclass: ", mean(test.y != caravan.knn)))
  print(paste("Correct Classification: ", ta[2,2]/sum(ta[2,])))
}


# Classification Applied Problems
# Problem 10 using the Weekly dataset in ISLR
summary(Weekly)

# All the lag variables are shifted by the Lag number,
# other than that they're same. Thus, their distribution is the same.
ggplot(Weekly, aes(Lag1)) +
  geom_density(fill = "blue", alpha=0.2) +
  geom_density(aes(Lag2), fill="orange", alpha=0.2) +
  geom_density(aes(Lag3), fill="green", alpha=0.2) +
  geom_density(aes(Lag4), fill="grey5", alpha=0.2) +
  geom_density(aes(Lag5), fill="yellow", alpha=0.2)

# Distribution of the Direction Variable
ggplot(Weekly, aes(Direction)) +
  geom_histogram(aes(fill=Direction), stat = "count") +
  scale_y_continuous(breaks=seq(0, 600, by=100)) +
  theme(legend.position = "None") 

# Distribution of variable Today
ggplot(Weekly, aes(seq(1, nrow(Weekly), by=1),Today)) +
  geom_point()

# Distribution of Variable Today
ggplot(Weekly, aes(Today)) +
  geom_density(color="blue") +
  geom_histogram(stat = "density", fill="blue", alpha=0.3)

# Distribution of Year Variable
ggplot(Weekly, aes(as.factor(Year))) +
  geom_bar(fill = "grey", alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Year", y = "Count", title = "Distribution of Year Variable")

# Volume is very skewed to the right
# Volume is measured in Billions
ggplot(Weekly, aes(Volume)) +
  geom_density(fill="Orange", color="pink", alpha = 0.5) +
  labs(title="Distribution of Volume (in Billions)", y = "Density")


# Using Logistic Regression to model Direction using all the predictors
# except year
weekly.glm <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                  data = Weekly, family = binomial(link=logit))
summary(weekly.glm)
confint(weekly.glm)
# Only Lag2 with a p-value of 0.02 and CI: (0.0062, 0.1117)
# is significant
# The Null Model Likelihood Ratio is 0.044 suggesting
# that at least one of the estimates is != 0 and, Lag2 != 0
dchisq(weekly.glm$null.deviance - weekly.glm$deviance, 
       df = weekly.glm$df.null - weekly.glm$df.residual)

weekly.pred <- ifelse(predict(weekly.glm, type="response") > 0.5, "Up", "Down")

tr <- table(weekly.pred, Weekly$Direction)

print(paste(cat("Misclassification Rate: ", 1 - round(sum(diag(tr))/sum(tr),4),
            "\nThe Model is Misclassifying ", (1 - round(sum(diag(tr))/sum(tr),4))*100,
            "% of times.\nIn this case there are a lot of false negatives.")))


# Logistic Regression using only the most significant variable Lag2
# Training the model on data until 2008 and testing the model
# for data in 2009 and 2010
train <- Weekly$Year < 2009   # Logical Vector
train.data <- Weekly[train,]
test.x <- Weekly[!train, -9]
test.y <- Weekly$Direction[!train]


weekly.glm1 <- glm(Direction ~ Lag2, data=train.data, family = binomial(link=logit))
summary(weekly.glm1)
weekly.test <- ifelse(predict(weekly.glm1, newdata = test.x, type="response") > 0.5, 
                      "Up", "Down")

tr1 <- table(weekly.test, test.y)


print(paste(cat("Test Data Misclassification Rate: ", 
                1 - round(sum(diag(tr1))/sum(tr1),4),
                "\nThe Model is Misclassifying ", 
                (1 - round(sum(diag(tr1))/sum(tr1),4))*100,
                "% of times.\nIn this case also there are a lot of false negatives.")))

# Using Linear Discriminant Analysis on the above test and train dataset
weekly.lda = lda(Direction ~ Lag2, data = train.data)
print(weekly.lda)

lda.pred <- predict(weekly.lda, newdata = test.x, type="response")$class
tr2 <- table(lda.pred, test.y)

# The LDA Model has exactly the same misclassification rate as Logistic Reg
print(paste(cat("Test Data Misclassification Rate: ", 
                1 - round(sum(diag(tr2))/sum(tr2),4),
                "\nThe Model is Misclassifying ", 
                (1 - round(sum(diag(tr2))/sum(tr2),4))*100,
                "% of times.\nIn this case also there are a lot of false negatives.")))

# Using QDA on the train and test set.
weekly.qda <- qda(Direction ~ Lag2, data = train.data)
print(weekly.qda)

qda.pred <- predict(weekly.qda, newdata = test.x)$class

tr3 <- table(qda.pred, test.y)

print(cat("The QDA Model does not predict that the market will go down
          at all and, it is wrong 43 out of 104 times.\nAll the error is
          from false negatives and the misclassification rate is: ",
          1 - sum(diag(tr3)/sum(tr3))))

# Using KNN method for classification
res <- data.frame(misclass = rep(NA, 5), 
                  method = c("knn(1)", "knn(3)", "knn(5)", "knn(7)", "knn(9)"))

for( i in seq(1, 9, by=2)){
  set.seed(i)
  weekly.knn <- knn(train = train.data[,-9], 
                    test = test.x, 
                    cl = train.data$Direction,
                    k = i)
  tr <- table(weekly.knn, test.y)
  res[which(res$method == paste("knn(",i,")", sep="")),"misclass"] = 
    round(1 - sum(diag(tr))/sum(tr), 4)
}

res <- rbind(res, data.frame(misclass = c(1 - round(sum(diag(tr3))/sum(tr3), 4),
                                          1 - round(sum(diag(tr2))/sum(tr2), 4),
                                          1 - round(sum(diag(tr1))/sum(tr1),4)),
                             method = c("QDA",
                                        "LDA",
                                        "Logistic")))

ggplot(res, aes(reorder(method, -misclass), misclass)) +
  geom_bar(aes(fill=method), stat="identity") +
  geom_text(aes(label = misclass), vjust = -0.6, size=4) +
  labs(x = "Method", y = "Misclassification Rate",
       title="Misclass Rate of Different models for Classification") +
  theme(legend.position = "None")

###############################################################################
###### Problem 11
###############################################################################
mpg01data <- Auto[,-1]
mpg01data$mpg01 <- ifelse(Auto$mpg > median(Auto$mpg),1,0)

# the correlation plot suggests that many of the variables have very strong
# correlation to each other:
# correlation between cylinders and displacement = 0.95
# correlation between weight and displacement = 0.93
# correlation between weight and horsepower = 0.86
# correlation between weight and cylindes = 0.89
# There are more correlations suggesting that an ill conceived model if 
# all the variables are used.
corrplot::corrplot(cor(mpg01data[,-8]), method="ellipse", diag = F)

# Splitting the data into training and test set
# 20% to test and 80% to train
set.seed(123)
idx <- sample(1:nrow(mpg01data), 0.2 * nrow(mpg01data), replace = F)
train <- mpg01data[-idx,]
test <- mpg01data[idx,]

# Using LDA to model mpg01 on related data from the training set
# variables were chosen based on linear correlation to mpg01
lda.fit <- lda(mpg01 ~ weigth+year+, data=train)
print(lda.fit)

lda.test <- predict(lda.fit, newdata=test)

tr <- table(lda.test$class, test$mpg01)
print(tr)
print(paste("Misclassification rate for LDA: ", 
            1 - round(sum(diag(tr))/sum(tr), 2)))

# test error is very good. there are some false negatives
# i.e model predicts high mpg when it actually is low mpg

# Using the same variables as for the LDA in QDA
qda.fit <- qda(mpg01 ~ weight+year, data=train)
print(qda.fit)

qda.test <- predict(qda.fit, newdata = test)

tr1 <- table(qda.test$class, test$mpg01)
print(tr1)
print(paste("Misclassification rate for QDA: ", 
            1 - round(sum(diag(tr1))/sum(tr1), 2)))


# Test error for the QDA model is even better,
# the model predicts 1 less false negative than LDA model to improve by 1%

# Using the same variables as for the LDA in Logistic regression
glm.fit <- glm(mpg01 ~ weight+year, data=train)
summary(glm.fit)

glm.test <- predict(glm.fit, newdata = test, type = "response")

glm.test.class <- ifelse(glm.test > 0.5, 1, 0)

tr2 <- table(glm.test.class, test$mpg01)
print(tr2)
print(paste("Misclassification rate for Logistic: ", 
            1 - round(sum(diag(tr2))/sum(tr2), 2)))

# Using knn classification for various values of k
res <- data.frame(method = rep(NA, times=13), misclass = rep(-1, times=13))
for(k in 1:10){
  set.seed(k)
  knn.fit <- knn(train[,c(4,6)], 
                 test[,c(4,6)], 
                 as.factor(train$mpg01),
                 k)
  
  int.tab <- table(knn.fit, test$mpg01)
  res[k,] <- c(paste("knn(", k, ")", sep = ""), 
               1 - round(sum(diag(int.tab))/sum(int.tab), 3))
}

res[11,] <- c("LDA", 1 - round(sum(diag(tr))/sum(tr), 2))
res[12,] <- c("QDA", 1 - round(sum(diag(tr1))/sum(tr1), 2))
res[13,] <- c("Logistic", 1 - round(sum(diag(tr2))/sum(tr2), 2))
res[,2] <- as.numeric(res[,2])

ggplot(res, aes(reorder(method, -misclass), misclass)) +
  geom_bar(aes(fill = method), stat="identity") +
  geom_text(aes(label = misclass), size = 3, vjust= -0.6) +
  labs(x = "Method", y = "Misclassification Rate",
       title ="Test data Misclassification Rate for Auto dataset") +
  theme(legend.position = "None")




  
  
  
  
  









