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
