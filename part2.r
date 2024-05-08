source("funcs/kum-mu-phi.r")
source("funcs/karma.fit.r")
source("funcs/karma.r")
source("funcs/barma.fit.r")
source("funcs/barma.r")

# Split the data set in to test and validation part, separating the last
# 10 observed values to evaluate the forecasting;
data <- scan("data/unemployment.txt")
test_data <- tail(data, 10)
validation_data <- head(data, -10)

# Check the data behavior to identify suitable approaches to ﬁt such data;
# Fit the Gaussian-base and model based on suitable distributions. To ﬁt,
# for example, the beta and Kumaraswamy-based ARMA models (βARMA
# model and KARMA model, respectively), you can use the functions available
# on Canvas.
pdf(paste("img/unemployment_time_series.pdf", sep = ""))
print(plot(data, type = "l", main = "Time Series Data"))
dev.off()

sink(file = paste("out_time_series.log", sep = ""))
data_ts <- ts(data, start = 1, end = length(data))



print("BARMA model")
barma_model <- barma(
    y = data_ts,
    ar = c(1, 2),
    ma = c(1, 2),
    link = "logit",
    diag = 1, h = 6
)
print("CREATED")
print("KARMA model")
karma_model <- karma(
    yab = data_ts,
    ar = c(1, 2),
    ma = c(1, 2),
    a = 0,
    b = 1,
    diag = 2,
    h = 10
)
print("CREATED")


# In conclusion: what is the most accurate model for such data?
# Compare AIC values and residual analysis to determine the most accurate model
