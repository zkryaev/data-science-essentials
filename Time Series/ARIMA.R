data <- read.table("~/jj.dat", header = TRUE, sep = "\t")
time_series <- ts(data$X.71, start = 1960, frequency = 4)
plot(time_series,type="l", xlab = "Время", ylab = "EPS") + grid()

diff_time_series <- diff(time_series)
plot(diff_time_series, type = "l", xlab = "Время", ylab = "Разница EPS", 
     main = "Разности квартальных прибылей на акцию") + grid()

log_eps <- log10(time_series)
diff_log_eps <- diff(log_eps)
plot(log_eps, type = "l", xlab = "Время", ylab = "log10(EPS)", 
     main = "log10(квартальной прибыли на акцию)") + grid()

plot(diff_log_eps, type = "l", xlab = "Время", ylab = "Разность log10(EPS)", 
     main = "diff(log10(квартальной прибыли на акцию))") + grid()


acf(diff_log_eps, main = "ACF разностей log10(EPS)")
pacf(diff_log_eps, main = "PACF разностей log10(EPS)")

arima_model_2 <- arima(diff_log_eps, order = c(2, 1, 2))
arima_model_3 <- arima(diff_log_eps, order = c(3, 1, 3))

aic_2 <- AIC(arima_model_2)
aic_3 <- AIC(arima_model_3)

cat("AIC для ARIMA(2,1,2):", aic_2, "\n")
cat("AIC для ARIMA(3,1,3):", aic_3, "\n")

#___________________________________________________________
ts_sim_AR4 <- arima.sim(n = 10000, list(ar = c(0.9, -0.5, 0.2, -0.3)))
plot(ts_sim_AR4, type = "l", main = paste("AR(", 4, ") Series")) + grid()

pacf(ts_sim_AR4, main = paste("PACF for AR(",4,") Series"))

ts_sim_AR3 <- arima.sim(n = 10000, list(ar = c(0.9, -0.5, 0.2)))
plot(ts_sim_AR3, type = "l", main = paste("AR(", 3, ") Series")) + grid()

pacf(ts_sim_AR3, main = paste("PACF for AR(",3,") Series"))

ts_sim_AR2 <- arima.sim(n = 10000, list(ar = c(0.9, -0.5)))
plot(ts_sim_AR2, type = "l", main = paste("AR(", 2, ") Series")) + grid()

pacf(ts_sim_AR2, main = paste("PACF for AR(",2,") Series"))

ts_sim_AR1 <- arima.sim(n = 10000, list(ar = c(0.9)))
plot(ts_sim_AR1, type = "l", main = paste("AR(", 1, ") Series")) + grid()

pacf(ts_sim_AR1, main = paste("PACF for AR(",1,") Series"))
#___________________________________________________________

sim_MA4 <- arima.sim(n = 10000, list( ma = c(-1.9, 1.7, -1.5, 1.5)))
plot(sim_MA4, type = "l", main = paste("MA(", 4, ") Series")) + grid()

acf(sim_MA4, main = paste("ACF for MA(", 4, ") Series"))

sim_MA3 <- arima.sim(n = 10000, list( ma = c(-1.9, 1.7, -1.5)))
plot(sim_MA3, type = "l", main = paste("MA(", 3, ") Series")) + grid()

acf(sim_MA3, main = paste("ACF for MA(", 3, ") Series"))

sim_MA2 <- arima.sim(n = 10000, list( ma = c(-1.9, 1.7)))
plot(sim_MA2, type = "l", main = paste("MA(", 2, ") Series")) + grid()

acf(sim_MA2, main = paste("ACF for MA(", 2, ") Series"))

sim_MA1 <- arima.sim(n = 10000, list( ma = c(-1.9)))
plot(sim_MA1, type = "l", main = paste("MA(", 1, ") Series")) + grid()

acf(sim_MA1, main = paste("ACF for MA(", 1, ") Series"))

#___________________________________________________________