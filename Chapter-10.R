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