
# Computing deviance for linear models ------------------------------------

data(trees)
str(trees)

# split into training and test sets
trees$train <- rep_len(c(0, 1), nrow(trees))
train <- subset(trees, train == 1)
test <- subset(trees, train == 1)

# visualize our data
plot(x = trees$Girth, 
     y = trees$Volume, 
     col = trees$train + 1, 
     pch = 19, 
     xlab = "Girth", 
     ylab = "Volume")

legend("topleft", 
       col = c(1, 2), 
       pch = 19, 
       legend = 
         c("Training data", "Test data"), 
       bty = "n")


# fit a linear regression model and a second degree polynomial
m1 <- lm(Volume ~ poly(Girth, 1), data = train)
m2 <- lm(Volume ~ poly(Girth, 2), data = train)

# compute deviance for both models: -2 * sum(loglik)


# add predictors to the model and compare deviance



# compare the three models with AIC
