library(rpart)
library(ISLR)
library(ggplot2)

# If Sales are more than 8 is success
High = ifelse(Carseats$Sales <= 8, "No", "Yes")
# Attach the binary variable to original data
Carseats$High = High

# The resulting binary separation is not balanced
# No = 236; Yes = 164
ggplot(Carseats, aes(High)) +
  geom_bar()