library(forecast)

source("funcs/kum-mu-phi.r")
source("funcs/karma.fit.r")
source("funcs/karma.r")
source("funcs/barma.fit.r")
source("funcs/barma.r")
pdf("acf_pacf_plots.pdf")

data <- scan("data/unemployment.txt")
test_data <- tail(data, 10)
validation_data <- head(data, -10)

pdf(paste("img/unemployment_time_series.pdf", sep = ""))
print(plot(data,
           type = "l",
           main = "Hidden unemploment rate over time",
           xlab = "Time",
           ylab = "Hidden unemployment rate"))
dev.off()
sink()
sink(file = paste("out_time_series.log", sep = ""))
data_ts <- ts(data, start = 1, end = length(data))
print("ARMA suggestion:")
acf <- acf(data_ts)
pacf <- pacf(data_ts)
print("ACF:")
print(acf)
print("PACF:")
print(pacf)
dev.off()

pdf("BARMA_plots.pdf")
print("BARMA model")
barma_model <- barma(
    y = data_ts,
    ar = c(1),
    ma = c(1,2),
    link = "logit",
    diag = 1, h = 6
)
print("CREATED")
dev.off()
pdf("KARMA_plots.pdf")
print("KARMA model")
karma_model <- karma(
    yab = data_ts,
    ar = c(1,2),
    ma = c(1,2),
    a = 0,
    b = 1,
    diag = 2,
    h = 10
)
dev.off()
print("CREATED")
