set.seed(10)
# simulation a ARMA processes
arma1 <- arima.sim(
    model = list(ar = 0.2, ma = 0.4),
    n = 200
)
arma2 <- arima.sim(
    model = list(ar = 0.9, ma = -0.65),
    n = 200
)
arma3 <- arima.sim(
    model = list(ar = 0.35, ma = 0.95),
    n = 200
)
plot.ts(cbind(arma1, arma2, arma3),
    main = "ARMA Simulated Processes"
)
# ploting the confidence interval (acf)
par(mfrow = c(1, 3))
acf(arma1)
acf(arma2)
acf(arma3)
