roll_dice <- function(rolls, sides) {
  sample(sides, size = rolls, replace = TRUE)
}

roll_dice(1000, 6)





# define the number of times to roll and the number of sides
n_rolls <- 1000000
n_sides <- 20

# calculate theoretical information entropy
-sum(rep((1 / n_sides) * log(1 / n_sides), n_sides))


# generate "true" probability of each outcome
p <- rep(1/n_sides, times = n_sides)

# generate a sample to estimate the probability of each outcome
x <- roll_dice(n_rolls, n_sides)

# ger the empirical frequencies of each outcome (your estimate)
q <- tabulate(x) / length(x)

plot(q, ylim = c(0, .1))
points(1:n_sides, rep(1/n_sides, times = n_sides), col = "red")

# calculate KL divergence
sum(p * log(p/q))

