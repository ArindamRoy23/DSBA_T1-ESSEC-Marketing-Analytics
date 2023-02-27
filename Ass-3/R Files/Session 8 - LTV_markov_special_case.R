# Constants
num_periods = 50
discount_rate = .05

# Create the transition matrix in a Markov chain
transition = cbind(c(.95, .00),
                   c(.05, 1.0))
print(transition)

# Init the segments, populate first period
segments = matrix(nrow = 2, ncol = num_periods)
segments[1:2, 1] = c(1, 0)

# Compute for each an every period (and round output for clarity)
for (i in 2:num_periods) {
   segments[, i] = segments[, i-1] %*% transition
}

# Compute LTV
discount = 1 / ((1 + discount_rate) ^ ((1:num_periods) - 1 ))
discounted = segments[1, ] * discount
print(discounted)
print(sum(discounted[2:num_periods]))
