roll_dice <- function(rolls, sides) {
  sample(sides, size = rolls, replace = TRUE)
}

roll_dice(20, 6)

# calculate theoretical information entropy
-sum(1 / 6 * log(1 / 6))

p <- rep(1/6, times = 6)

# generate a sample to estimate the probability of each outcome
x <- roll_dice(20, 6)

# ger the empirical frequencies of each outcome (your estimate)
q <- tabulate(x) / length(x)

plot(q, ylim = c(0, .5))
points(1:6, rep(1/6, times = 6), col = "red")

# calculate KL divergence
sum(p * log(p/q))

