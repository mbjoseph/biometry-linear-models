
# Computing deviance for linear models ------------------------------------

data(trees)
str(trees)

# split into training and test sets
trees$train <- sample(c(0, 1), nrow(trees), replace = TRUE)

# fit a linear regression model and a second degree polynomial
