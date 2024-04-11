set.seed(10)
# simulation a AR processes
ar1 <- arima.sim(
    model = list(ar = 0.25),
    n = 200
)
ar2 <- arima.sim(
    model = list(ar = 0.8),
    n = 200
)
ar3 <- arima.sim(
    model = list(ar = 0.001),
    n = 200
)
plot.ts(cbind(ar1, ar2, ar3),
    main = "AR Simulated Processes"
)
# ploting the confidence interval (acf)
par(mfrow = c(1, 3))
acf(ar1)
acf(ar2)
acf(ar3)
