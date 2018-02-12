library(ISLR)
library(ggplot2)
library(splines)
library(gam)
library(MASS)
library(leaps)
library(tidyr)

###################################################################################
######## Problem 9
###################################################################################
# Using "dis" as the predictor and "nox" as the response from Boston Dataset.
poly.fit = lm(nox ~ poly(dis, 3), data=Boston)
summary(poly.fit)

ggplot(Boston, aes(dis, nox)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method="lm", formula = y ~ poly(x,3), se=F, color="blue") +
  labs(x = "Weighted mean distances to 5 emplyment centers",
       y = "Nitrogen Dioxide levels parts per 10 million",
       title = "Cubic regression to model nox to dist")

# Polynomial regression suggests that all the the predictors i.e. coefficients
# for each x, x^2 and x^3 are significantly different than zero.
# Along with MSE = 0.00385, suggesting a very good fit with a very low error from
# the model predicted values, to the actual values.
# The low error is also confirmed by a high R-square = 71.5%, which states that
# about 71% of "nox" is explained by a cubic polynomial of "dis."

best_subset = regsubsets(nox ~ poly(dis, 10), data=Boston, nvmax = 10)

coef(best_subset, id=10)

dis = seq(min(Boston$dis), max(Boston$dis), length.out = 100)
mm = model.matrix(~ poly(dis, 10))

calc_poly = function(data, degree, response, predictor){
  form = as.formula(paste(response, "~", "poly(", predictor, ",", degree
                          , ")", sep=" "))
  poly_fit = lm(formula = form, data=data)
  poly_resid = residuals(poly_fit)
  
  ret_data = data.frame(res = residuals(poly_fit), preds = fitted(poly_fit))
  
  return (ret_data)
}

resp_data = data.frame(nox = Boston$nox)
resp_mse = rep(-1, 10)

for(i in 1:10){
  cc = calc_poly(Boston, i, "nox", "dis")
  resp_data = cbind(resp_data, cc$preds)
  colnames(resp_data)[i+1] = paste("poly", i, sep="")
  print(paste(dim(resp_data), " ", names(resp_data)))
  resp_mse[i] = sum(cc$res^6)
}

plot(resp_mse, type="b")
points(which.min(resp_mse), min(resp_mse), col="red")

rs = gather(resp_data[,-1], key="method", value="fitted")
rs$dis = rep(Boston$dis, 10)

ggplot(rs[1:506,], aes(x=dis)) +
  geom_point(data = Boston, aes(dis, nox), alpha=0.3) +
  geom_line(data=rs, aes(y = fitted, color=method)) +
  facet_wrap(~method, nrow=2) +
  labs(title="Polynomial fits of various degrees",
       y = "NOX in parts per 10 million",
       x = "Weighted mean distances to 5 emplyment centers",
       color="Polynomial\nDegree")  

best_subset = regsubsets(nox ~ poly(dis, 10), nvmax = 10, data = Boston)
plot(best_subset, scale="Cp")
plot(best_subset, scale = "bic")

# Best subset selection identifies coefficients up to the cubed predictors
# across all the best models so, also from the above computation shows that
# the cubed model results in the least SSE. Thus, a cubic polynomial will
# be the best for this data.

# Using Splines with 4 knots
# df: 4(knots) * 4(paramenters) = 16,
# 2 restrictions on 4 knots: 2 * 4 = 8
# Therefore df = 16 - 8 = 8
# bs(), for 4 knots uses, 7(df) - 3(parameters) = 4 knots, for splies without
# an intercept and, 8(df) - 4(parameters) = 4 knots, for spline with an intercept
b_base = bs(Boston$dis, df = 5, intercept = F)
matplot(Boston$dis, b_base)

attr(b_base, which="knots")

sp_fit = lm(nox ~ b_base, data=Boston)
summary(sp_fit)

sp_data = data.frame(dis = Boston$dis, 
                     fit = fitted(sp_fit),
                     nox = Boston$nox)

ggplot(sp_data, aes(dis, nox)) +
  geom_point() +
  geom_line(aes(dis, fit), colour ="blue", size=1) +
  labs(title="B-spline fit for Boston dataset",
       subtitle="B-splines with 2 knots is a good enough fit",
       x = "Distance to 5 major job centers",
       y = "NOX, parts per 10 millon")


# Computing b-splines for varying number of knots
res_data = data.frame(dis = Boston$dis)
res_sse = data.frame(knots = as.factor(rep(1:10)),
                     sse = rep(-1,10))

for(knot in 1:10){
  b_base = bs(Boston$dis, df = 3+knot, intercept = F)
  sp_fit = lm(nox ~ b_base, data=Boston)
  res_data = cbind(res_data, fitted(sp_fit))
  colnames(res_data)[knot+1] = as.character(knot)
  res_sse[knot,2] = sum(residuals(sp_fit)^2)
}

ggplot(res_sse, aes(knots, sse)) +
  geom_point(aes(color=knots))

rs = gather(res_data, key="knots", value = "fitted", as.character(1:10),
            factor_key = T)

ggplot(Boston, aes(dis, nox)) +
  geom_point() +
  geom_line(data=rs, aes(y=fitted, color=knots)) +
  geom_line(data=subset(rs, knots == "7"), aes(dis, fitted), color="red",size=1)+
  labs(title="B-Spline fits with different knots",
       subtitle="Knots 7: Bold Red line, has lowest MSE",
       x = "Distance to 5 major job centers",
       y = "NOX, parts per 10 millon")
  

