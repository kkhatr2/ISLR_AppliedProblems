library(ISLR)
library(ggplot2)

states = row.names(USArrests)

# Examining means descriptive stats for each variable
barplot(apply(USArrests, 2, FUN = mean))
barplot(apply(USArrests, 2, var))

# Each variable has a very different mean than the other and
# also their variances are very different.
# Thus, to perform PCA where equal weight is placed on eigen-values we have to 
# scale the variables.
#
# Performing PCA using prcomp()
# $center and $scale gives the mean and s.d. of variables before doing PCA
# The rotation matrix provides the principal component loadings.
# This function names it "rotation" matrix because when we multiply this matrix with
# X then we get the principal component scores. (n x p) * (p x p) = n x p
pr.out = prcomp(USArrests, scale = T)

biplot(pr.out, scale = 0) # This plot is inverted when compared to one in the book

pr.out$x = -1 * pr.out$x
pr.out$rotation = -1 * pr.out$rotation

biplot(pr.out, scale = 0)

# The prcomp() also gives us the std. deviation of each principal component score
# which we can square the get the variance and compute the variance explained
# by each score vector.
# The first principal component explains 62% of the data, second 25% and the 
# other two about 13%. Thus, 
pr.var = pr.out$sdev^2
pve = pr.var / sum(pr.var)
pve
plot(pve, type = "b", ylim = c(0,1))
plot(cumsum(pve), type ="b", ylim = c(0,1))

scaled.arrests = scale(USArrests)
km = kmeans(scaled.arrests, centers = 2, iter.max = 100)

# Plotting clusters using the frist two components
plot(pr.out$x[,c(1,2)])
points(pr.out$x[km$cluster == 1,c(1,2)], col="red", pch = 16)
points(pr.out$x[km$cluster == 2,c(1,2)], col="blue", pch = 16)
points(pr.out$x[km$cluster == 3,c(1,2)], col="cyan", pch = 16)
points(pr.out$x[km$cluster == 4,c(1,2)], col="magenta", pch = 16)

rm(list = ls())
################################################################################
#### Lab 2: Clustering
################################################################################
# Simulated dataset
set.seed(2)
x = matrix(rnorm(25 * 2), ncol = 2)
x[1:25,1] = x[1:25,1] + 3
x[1:25,2] = x[1:25,2] - 4

# K-Means clustering
# nstart = 20 uses 20 random assignments in step 1 of k-means algorithm
# step 1 is to randomly assign observations to clusters.
set.seed(123)
km.out = kmeans(x, centers = 2, nstart = 40)
print(km.out)

plot(x, col = km.out$cluster + 1, pch = 20, cex = 2, xlab = "", ylab = "")

# Heirarchical Clustering
hc.dist = dist(x)
hc.complete = hclust(hc.dist, method = "complete")
hc.average = hclust(hc.dist, method = "average")
hc.single = hclust(hc.dist, method = "single")

par(mfrow=c(1,3))
plot(hc.complete, main = "Complete Link", xlab = "", sub = "")
rect.hclust(hc.complete, k = 2, border = c("red","cyan"))

plot(hc.average, main = "Average Link", xlab = "", sub = "")
rect.hclust(hc.average, k = 3, border = c("red","cyan","green4"))

plot(hc.single, main = "Single Link", xlab = "", sub = "")
rect.hclust(hc.single, k = 3, border = c("red","cyan","green4"))
par(mfrow=c(1,1))

# Correlation based heirarchical clustering makes sense only for data sets with
# 3 or more variables.
set.seed(123)
x = matrix(rnorm(30*3), ncol = 3)
dd = as.dist(1 - cor(t(x)))
hc.corr = hclust(dd, method = "complete")
plot(hc.corr, main = "Correlation-Based Distance", xlab = "", sub = "")

rm(list = ls())
################################################################################
#### Lab 3: NC160 Data Example
# Data has 64 observations with cancer types in $labs
# Data has gene data for 64 patients with different cancers in a matrix 64x6830
################################################################################
# Function to assign different colors to different cancer types
Cols = function(vec){
  cols = rainbow(length(unique(vec)))
  return (cols[as.numeric(as.factor(vec))])
}
nci.labs = NCI60$labs
nci.data = NCI60$data
# Cancer types for cell lines are not balanced
barplot(table(nci.labs))

# Using PCA on the dataset to visualize the data
# Based on the plots as a whole, although some cancer types overlap, there is
# some segmentation between different cancer types using the first feq PC's
pr.out = prcomp(nci.data, scale = F)
par(mfrow = c(1,2))
plot(pr.out$x[,1:2], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z2")
plot(pr.out$x[,c(1,3)], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z3")
par(mfrow= c(1,1))

pve = 100 * pr.out$sdev^2 / sum(pr.out$sdev^2)
plot(pve, type = "o", main = "Percent Variance Explained by each PC", col="blue")
plot(cumsum(pve), type = "o", main = "Cumulative PVE for each PC", col="red")

# The sharp turn after 7th PC indicates that the first 7 PC's are enough to 
# explain most of the data.

# Clustering observations of NCI60 data set
par(mfrow = c(1,3))
data.dist = dist(nci.data)
hc.out = hclust(data.dist)
plot(hc.out, labels = nci.labs, main = "Complete Linkage",
     xlab = "", sub = "", ylab = "", cex = 0.5)

plot(hclust(data.dist, method = "average"), labels = nci.labs, 
     main = "Average Linkage", xlab = "", sub = "", ylab = "", cex = 0.5)

plot(hclust(data.dist, method = "single"), labels = nci.labs, 
     main = "Single Linkage", xlab = "", sub = "", ylab = "", cex = 0.5)
par(mfrow = c(1,1))

hc.clusters = cutree(hc.out, 4)

# Comparing with K-means
set.seed(2)
km.out = kmeans(nci.data, 4, nstart = 20)
km.clusters = km.out$cluster
table(km.clusters, hc.clusters)

# There is almost no concensus of clusters by heirarchical or k-means clustering
# Now using the first 7 principle components for the hclust algorithm
hc.pr.dist = dist(pr.out$x[,1:7])
hc.pr.hclust = hclust(hc.pr.dist)
plot(hc.pr.hclust, labels = nci.labs, main = "Heir. Clust with PCA",cex=0.5)
rect.hclust(hc.pr.hclust, k = 6, border = c("red","cyan","green4"))

hc.pr.clusters = cutree(hc.pr.hclust, 6)

km.pr = kmeans(pr.out$x[,1:7], 6, nstart = 20)
table(hc.pr.clusters, km.clusters = km.pr$cluster)
