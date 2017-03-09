# Linear regression from scratch with amniote data ------------------------

library(tidyverse)

# Read data and filter to only keep rodents adult mass & birth weight ----

d <- read_csv("http://esapubs.org/archive/ecol/E096/269/Data_Files/Amniote_Database_Aug_2015.csv", 
              na = c("-999", "NA")) %>%
  filter(order == "Rodentia") %>%
  select(adult_body_mass_g, birth_or_hatching_weight_g) %>%
  na.omit


# Define our explanatory variable and response ----------------------------
x <- log(d$adult_body_mass_g)
y <- log(d$birth_or_hatching_weight_g)


# Visualize ---------------------------------------------------------------
plot(x, y)

# Define a negative log likelihood function -------------------------------
nll <- function(pars, x, y) {
  # unpack parameter vector
  beta0 <- pars[1]
  beta1 <- pars[2]
  sigma <- exp(pars[3])
  
  y_hat <- beta0 + beta1 * x
  -sum(dnorm(y, mean = y_hat, sd = sigma, log = TRUE))
}

# Minimize the negative log likelihood using optim() ----------------------
m_fit <- optim(c(0, 3, 0), nll, x = x, y = y, hessian = TRUE)


# Draw line on plot -------------------------------------------------------
abline(a = m_fit$par[1], b = m_fit$par[2], col = 2)





# Get standard errors for all parameters ----------------------------------






# Compute confidence intervals for all parameters -------------------------






# Compute residual and total sums of squares ------------------------------






# Compute R^2 -------------------------------------------------------------


