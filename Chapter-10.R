library(ISLR)
library(ggplot2)

####################################################################################
###### Problem 7
# Similarity of Eucledian distance and correlations of standardized variables.
####################################################################################
arrests.sc = scale(USArrests)
arrests.dist = 1 / dist(arrests.sc)^2
arrests.corr = as.dist(1 - cor(t(arrests.sc)))

lo = round(arrests.dist - arrests.corr, 1)
table(lo)

# Based on the table all the differences are between (-1.9, 0.8)
# So, yes these the measures of distance and correlation are about the same as
# the proportional square.


##################################################################################
###### Problem 8
# Computing Percent Variance Explained (PVE) of principal components
##################################################################################
# Using the simple approach by using std. dev. from using prcomp
arrests.pca = prcomp(arrests.sc, center = F, scale. = F)
pca.pve = arrests.pca$sdev^2 / sum(arrests.pca$sdev^2)
plot(pca.pve, type = "b", col="red", ylim = c(0,1))
lines(cumsum(pca.pve), type = "b", col="blue")

# Using the formula
# Sum each column of the principal components and square them to get the numerator
den.sum = sum(apply(arrests.sc, MARGIN = 2, FUN = function(x) sum(x^2)))
num.sum = apply(arrests.pca$x, MARGIN = 2, FUN = function(x) sum(x^2))

pve = num.sum/den.sum
plot(pve, type = "b", col = "red", ylim = c(0,1))
lines(cumsum(pve), col = "blue")

rm(list = ls())
##################################################################################
###### Problem 9
# Clustering States based on crime data
##################################################################################
# using the raw data.
arrests = USArrests
arrests.dist = dist(arrests)
arrests.hc = hclust(arrests.dist, method = "complete")
plot(arrests.hc, cex = 0.6)
cutree(arrests.hc, k = 3)
rect.hclust(arrests.hc, k = 3, border = c("red","blue","cyan"))

# Using scaled data
arrests.sc = scale(USArrests)
arrests.sc.dist = dist(arrests.sc)
arrests.sc.hc = hclust(arrests.sc.dist)
plot(arrests.sc.hc, cex = 0.7)
rect.hclust(arrests.sc.hc, k = 2, border = c("red","blue","cyan"))

# Comments
# Since the variables in this data set were measured at different scales and have
# different variances, it is best to scale the data. After scaling the data,
# the distance between different variables is on the same scale and the clusters
# will be more representative of the different variables in the data set.

#################################################################################
###### Problem 10
# PCA and K-means on simulated data.
#################################################################################
# Generating 60 observations in three classes with 50 variables.
set.seed(1)
data = data.frame(c(rnorm(20) + 5, rnorm(20), rnorm(20) -5))

for(i in 2:50){
  set.seed(i)
  tmp = c(rnorm(20) + 5, rnorm(20), rnorm(20) -5)
  data = cbind(data, tmp)
  rm(tmp)
}
rm(i)
colnames(data) = make.names(1:50)
data$col = c(rep("red", 20), rep("cyan",20),rep("blue",20))

# Performing PCA and plotting the first 2 principal components
# The first two components show maximum separation betwen classes.
# In the data set, classes were adjusted with a mean of -5,0,5 but the first
# two principal components show a difference of about -35,0,35.
# So, the first two components very confidently explain difference in classes.
sim.pca = prcomp(data)
ggplot(as.data.frame(sim.pca$x), aes(PC1, PC2)) +
  geom_point(aes(color = data$col)) +
  theme(legend.position = "none")

# Using K-means to cluster the classes using all 50 predictors.
# Since all the variables are generated with a SD = 1, we do not need to
# recenter and scale the variables.
set.seed(123)
sim.km = kmeans(data[,1:50], centers = 3, nstart = 20)

table(sim.km$cluster)

sim.dist = dist(data[,1:50])

# Toal SS for K-means
# is sum of squares of difference of each variable's mean with
# each observation of that variable.
#sum(apply(data[,1:50], 2, FUN = function(x) sum((x - mean(x))^2)))
#
# From this total SS, we want the between SS to be as large as possible
# i.e. we want the ratio of between SS and total SS to be close to 1
# In this data set, this ratio is 0.946 thus suggesting that this clustering
# with 3 centroids is very good because, total SS is about equal to between SS.
# Thus, there are very few observations that are far from cluster centers.
# In terms of R-squared, 94.6% variation in the data is explained by the clusters.
sim.km$betweenss/sim.km$totss # = 0.946

# Now Comparing the above statistic with different number of centroids
ss_ratio = rep(0, 6)
for(i in 1:6){
  tmp.km = kmeans(data[,1:50], centers = i+1, nstart = 20)
  ss_ratio[i] = tmp.km$betweenss / tmp.km$totss
  rm(tmp.km)
}
rm(i)

# Based on the results and the plot below.
# Clustering with 3 and more centers gives the best results and, based on a 
# parsimonious model, clustering with 3 centers should be advised.
# Also, since this is a simulated dataset and we know that there are 3 clusters,
# the comparison does confirm our model.
res = data.frame(ss = ss_ratio, "K" = 2:7)
ggplot(res, aes(K, ss)) +
  geom_point(color = "black") +
  lims(y = c(-0.01,1)) +
  geom_text(aes(label = round(ss, 3)), vjust =-0.6)

# K-means with PCA
# Using K-means algorithm on the first 2 components of PCA
# Using K-means in conjunction to PCA gives a between SS to total Ss ratio
# of 0.995!!!!
# Amazing!! But this was expected because, from the plot above of the first two
# principal components, maximum difference between the three classes is 
# explained by the first two components. And, the difference is substancial.
sim.pca.km = kmeans(sim.pca$x[,1:2], centers = 3, nstart = 20)

sim.pca.km$betweenss / sim.pca.km$totss




