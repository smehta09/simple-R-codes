#NPV
# Define the cash flows
cash_flows <- c(0, rep(3000, 3), rep(1000, 2))

# Define the discount factors
discount_factors <- (1 + 0.05) ^ - (0:5)

# Calculate the net present value
net_present_value <- sum(cash_flows * discount_factors) - 10000
net_present_value
