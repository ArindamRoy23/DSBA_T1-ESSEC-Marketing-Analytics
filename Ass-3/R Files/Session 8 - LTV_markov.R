# Constants
num_periods = 20
discount_rate = .15

# Create the transition matrix in a Markov chain
transition = cbind(c(.50, .10, .05, .01, .00),
                   c(.20, .50, .25, .09, .00),
                   c(.30, .40, .00, .00, .00),
                   c(.00, .00, .70, .00, .00),
                   c(.00, .00, .00, .90, 1.0))
print(transition)

# Init the segments, populate first period
segments = matrix(nrow = 5, ncol = num_periods)
segments[1:5, 1] = c(3600, 14400, 9500, 6200, 21900)

# Number of new customers every year
# new_customers = c(500, 2000, 0, 0, 0)

# Compute for each an every period (and round output for clarity)
for (i in 2:num_periods) {
   segments[, i] = segments[, i-1] %*% transition
   # segments[, i] = segments[, i] + new_customers
}
plot(segments[5, ], ylim = c(0, 40000))
lines(segments[1, ])
lines(segments[2, ])
lines(segments[3, ])
lines(segments[4, ])
lines(segments[5, ])
print(round(segments))

# Yearly revenue per segment
yearly_revenue = c(250, 37, 0, 0, 0)

# Compute revenue per segment
revenue_per_segment = yearly_revenue * segments
print("Revenue per segment:")
print(revenue_per_segment)

# Compute yearly revenue
yearly_revenue = colSums(revenue_per_segment)
print("Yearly revenue:")
print(yearly_revenue)
plot(yearly_revenue)
lines(yearly_revenue)

# Compute cumulated revenue
cumulated_revenue = cumsum(yearly_revenue)
print("Cumulated revenue:")
print(cumulated_revenue)
plot(cumulated_revenue)
lines(cumulated_revenue)

# Create a discount factor
# Other version: discount = (1 - discount_rate) ^ (1:num_periods)
discount = 1 / ((1 + discount_rate) ^ ((1:num_periods) - 1))
print(discount)

# Compute discounted yearly revenue
disc_yearly_revenue = yearly_revenue * discount
print("Discounted yearly_revenue:")
print(disc_yearly_revenue)
plot(yearly_revenue)
lines(yearly_revenue)
lines(disc_yearly_revenue)

# Compute discounted cumulated revenue
disc_cumulated_revenue = cumsum(disc_yearly_revenue)
print("Cumulated revenue:")
print(cumulated_revenue)
print("Discounted cumulated revenue:")
print(disc_cumulated_revenue)
plot(cumulated_revenue)
lines(disc_cumulated_revenue)

# What is the database worth?
print(disc_cumulated_revenue[num_periods] - yearly_revenue[1])
