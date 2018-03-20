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