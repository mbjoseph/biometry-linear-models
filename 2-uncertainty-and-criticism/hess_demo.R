
# Demonstrating Hessians and the curvature of a likelihood ----------------

# We'll assume a simple model: 
# - we only want to estimate a standard deviation
# - our variable is normally distributed
# - we know the true value of the standard deviation is one

n1 <- 5
true_mean <- 1
y <- rnorm(n1, mean = true_mean)



# negative log likelihood function
nll <- function(par, y) {
  -sum(dnorm(y, mean = par, log = TRUE))
}



# get our MLE
fit_1 <- optim(0, nll, y = y, hessian = TRUE, method = "L-BFGS-B")
hess_1 <- fit_1$hessian



# evaluate the negative log likelihood across a range of values
par_range <- seq(-4, 20, length.out = 100)
nll_values <- sapply(par_range, nll, y = y)

plot(par_range, nll_values, 
     xlab = "Mean", 
     ylab = "Negative log lik", 
     ylim = c(-100, 1000), type = "l")
abline(v = true_mean, lty = 2, col = "grey")








# now do the same thing with more data
n2 <- 20
y <- rnorm(n2, mean = true_mean)
lines(par_range, sapply(par_range, nll, y), col = 2)

fit_2 <- optim(0, nll, y = y, hessian = TRUE, method = "L-BFGS-B")
hess_2 <- fit_2$hessian











# now do the same with even more data
n3 <- 100
y <- rnorm(n3, mean = true_mean)
lines(par_range, sapply(par_range, nll, y), col = 3)

fit_3 <- optim(0, nll, y = y, hessian = TRUE, method = "L-BFGS-B")
hess_3 <- fit_3$hessian

legend("bottomright", 
       col = 1:3, 
       lty = 1, 
       legend = paste("Hessian =", round(c(hess_1, hess_2, hess_3), 3), 
                      ", n =", c(n1, n2, n3)), 
       bty = "n")

title("Negative log likelihood profiles\nfor the mean")

