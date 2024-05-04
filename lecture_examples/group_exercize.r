# Define your time series
x <- c(1.03, -0.92, 0.25, 1.15, -0.38, 0.81, -0.14, -0.44, 0.55, 0.50)

# Calculate autocorrelation values
acf_result <- acf(x, lag.max = 5, plot = FALSE)

# Print the autocorrelation values
print(acf_result$acf)