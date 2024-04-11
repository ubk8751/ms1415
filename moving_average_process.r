set.seed(10)
# simulation a MA processes
ma1 <- arima.sim(model = list(ma = 0.7), n = 200)
ma2 <- arima.sim(model = list(ma = 0.9), n = 200)
ma3 <- arima.sim(model = list(ma = -0.001), n = 200)
plot.ts(cbind(ma1, ma2, ma3),
    main = "MA Simulated Processes"
)
# ploting the confidence interval (acf)
par(mfrow = c(1, 3))
acf(ma1)
acf(ma2)
acf(ma3)
