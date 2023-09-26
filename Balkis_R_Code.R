library(fpp2)
library(urca)
rm(list=ls())


df <- read.csv("C:\\Users\\Lenovo\\Desktop\\MSBA\\Spring 2023\\Forecasting Analytics MSBA317\\Project\\balkis total sales.csv")
ts <- ts(df$total.sales, frequency = 12, start = c(2000,1))
# Add a small value to avoid zero instances
small_value <- 0.001  
ts <- ts + small_value


#plotting the whole dataset
autoplot(ts)
# fix the value in the year 2004
window(ts, start = 2004, end = c(2004, 12))
# The entry error is in April 2004 

#seasonal plot
ggseasonplot(ts)

#subseries plot
ggsubseriesplot(ts)

#polar seasonal plot
ggseasonplot(ts, polar = TRUE)

#autocorrelation plot
ggAcf(ts)

#lag plot
gglagplot(ts)

#####################################################
#train test split (splitting after September 2004 to keep only significant values)
train <- window(ts, start = c(2004, 9), end = c(2017, 12))
test <- window(ts, start = 2018, end = c(2019, 10))
####################################################
autoplot(train)

#mean naive fit
fit1 <- meanf(train)
autoplot(fit1)
accuracy(fit1, test)
fits1 <- fitted(fit1)
autoplot(train,series="Data")	+
  autolayer(fits1,series="Fitted")	+  xlab("Month") +ylab ("Total Sales (LBP)") + 
  ggtitle("Mean Naive")

#############################################################################
#seasonal naive fit
fit2 <- snaive(train)
autoplot(fit2)
accuracy(fit2, test)
fits2 <- fitted(fit2)
autoplot(train,series="Data")	+
  autolayer(fits2,series="Fitted")	+  xlab("Month") +ylab ("Total Sales (LBP)") +
  ggtitle("Seasonal Naive")
#############################################################################
#naive fit
fit3 <- naive(train)
autoplot(fit3)
accuracy(fit3, test)
fits3 <- fitted(fit3)
autoplot(train,series="Data")	+
  autolayer(fits3,series="Fitted")	+  xlab("Month") +ylab ("Total Sales (LBP)") +
  ggtitle("Naive")
#############################################################################
#drift fit
fit4 <- rwf(train, drift=TRUE)
autoplot(fit4)
accuracy(fit4, test)
fits4 <- fitted(fit4)
autoplot(train,series="Data")	+
  autolayer(fits4,series="Fitted")	+  xlab("Month") + ylab("Total Sales (LBP)") +
  ggtitle("Drift")
#############################################################################
## Naive methods with CV
# mean naive fit with CV
e <- tsCV(ts, meanf)
sqrt(mean(e^2,na.rm=TRUE))
# RMSE = 168443442

#seasonal naive fit with CV
e <- tsCV(ts, snaive)
sqrt(mean(e^2,na.rm=TRUE))
# RMSE = 77773337

#naive fit with CV
e <- tsCV(ts, naive)
sqrt(mean(e^2,na.rm=TRUE))
# RMSE = 110316729

#drift fit with CV
e <- tsCV(ts, rwf,drift=TRUE)
sqrt(mean(e^2,na.rm=TRUE))
# RMSE = 110935300

#############################################################################
#############################################################################
# Time Series Decomposition

fit <- stl(train, s.window=7)
autoplot(fit) + xlab("Time")


ggsubseriesplot(seasonal(fit))


autoplot(train, series="Data") +  
  autolayer(trendcycle(fit), series="Trend-cycle")

# Forecasting using seasonal decomposition
#plotting the forecast
fit %>% forecast(h=12) %>% autoplot()


#############################################################################
#############################################################################
##ARIMA Manually

# Using Kpss to determine if the data is stationary
summary(ur.kpss(train))
# test-statistic = 2.0227> 1% critical value => Reject H0 => Data is not stationary

# it needs one order of difference
# check the AR(p) and MA(q) using ACF and PACF
ggtsdisplay(train)
# ACF is sinusoidal and the PACF has a significant spike at lag 1 but none beyond it
# AR(1) with differencing of order d=1
#############################################################################

# ARIMA(1, 1, 0)

(fit5 <- Arima(train, order = c(1, 1, 0)))

# checking the residuals of fit5
checkresiduals(fit5)

#plotting the forecast
fit5 %>% forecast(h=12) %>% autoplot()

# Generate out-of-sample forecasts for the test data and check the accuracy
fc5 <- forecast(fit5, h = length(test))

# Plot the fitted and forecasted data
autoplot(train) + 
  autolayer(fitted(fit5), series = "Fitted") +
  autolayer(fc5, series = "Forecast") +
  xlab("Time") + ylab("Value") +
  ggtitle("ARIMA(1, 1, 0)")

# Calculate the RMSE
rmse <- sqrt(mean((fc5$mean - test)^2))

# Calculate the MAPE
mape <- mean(abs(fc5$mean - test)/test) * 100

# Print the results
cat("ARIMA(1,1,0) model accuracy metrics:\n")
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

#############################################################################

# ARIMA(1, 2, 0)

(fit6 <- Arima(train, order = c(1, 2, 0)))

# checking the residuals of fit6
checkresiduals(fit6)

#plotting the forecast
fit6 %>% forecast(h=12) %>% autoplot()

# Generate out-of-sample forecasts for the test data and check the accuracy
fc6 <- forecast(fit6, h = length(test))

# Plot the fitted and forecasted data
autoplot(train) + 
  autolayer(fitted(fit6), series = "Fitted") +
  autolayer(fc6, series = "Forecast") +
  xlab("Time") + ylab("Value") +
  ggtitle("ARIMA(1, 2, 0)")

# Calculate the RMSE
rmse <- sqrt(mean((fc6$mean - test)^2))

# Calculate the MAPE
mape <- mean(abs(fc6$mean - test)/test) * 100

# Print the results
cat("ARIMA(2,1,0) model accuracy metrics:\n")
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

#############################################################################

# Auto Arima
(fit7 <- auto.arima(train, seasonal = FALSE))
# auto.arima = ARIMA(0, 1, 0)

# checking the residuals of fit7
checkresiduals(fit7)

#plotting the forecast
fit7 %>% forecast(h=12) %>% autoplot()

# Generate out-of-sample forecasts for the test data and check the accuracy
fc7 <- forecast(fit7, h = length(test))

# Plot the fitted and forecasted data
autoplot(train) + 
  autolayer(fitted(fit7), series = "Fitted") +
  autolayer(fc7, series = "Forecast") +
  xlab("Time") + ylab("Value") +
  ggtitle("ARIMA(0, 1, 0)")

# Calculate the RMSE
rmse <- sqrt(mean((fc7$mean - test)^2))

# Calculate the MAPE
mape <- mean(abs(fc7$mean - test)/test) * 100

# Print the results
cat("ARIMA(0,1,0) model accuracy metrics:\n")
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")
#############################################################################
#############################################################################
##Seasonal ARIMA
train %>% diff(lag=12) %>% ggtsdisplay()
train %>% diff(lag=12) %>% diff() %>% ggtsdisplay()
# The significant spike at lag 1 in the ACF suggests a non-seasonal MA(1)
# The significant spike at lag 12 in the ACF suggests a non-seasonal MA(1)
# We will begin with ARIMA(0, 1, 1)(0, 1, 1)[12]

#############################################################################

# ARIMA(0, 1, 1)(0, 1, 1)[12]
(fit8 <- Arima(train, order = c(0, 1, 1), seasonal = c(0, 1, 1)))

# checking the residuals of fit8
checkresiduals(fit8)

# ggtsdisplay of the residuals
ggtsdisplay(residuals(fit8))

#plotting the forecast
fit8 %>% forecast(h=12) %>% autoplot()

# Generate out-of-sample forecasts for the test data and check the accuracy
fc8 <- forecast(fit8, h = length(test))

# Plot the fitted and forecasted data
autoplot(train) + 
  autolayer(fitted(fit8), series = "Fitted") +
  autolayer(fc8, series = "Forecast") +
  xlab("Time") + ylab("Value") +
  ggtitle("ARIMA(0, 1, 1)(0, 1, 1)[12]")

# Calculate the RMSE
rmse <- sqrt(mean((fc8$mean - test)^2))

# Calculate the MAPE
mape <- mean(abs(fc8$mean - test)/test) * 100

# Print the results
cat("ARIMA(0, 1, 1)(0, 1, 1) model accuracy metrics:\n")
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

# both the ACF and PACF of the residuals show significant spikes at lag 1 and lag 12
# this indicates that some additional seasonal and non-seasonal terms need to be included in the model

#############################################################################

#ARIMA(0, 1, 2)(0, 1, 1)[12]

(fit9 <- Arima(train, order = c(0, 1, 2), seasonal = c(0, 1, 1)))

# checking the residuals of fit9
checkresiduals(fit9)

# ggtsdisplay of the residuals
ggtsdisplay(residuals(fit9))

#plotting the forecast
fit9 %>% forecast(h=12) %>% autoplot()

# Generate out-of-sample forecasts for the test data and check the accuracy
fc9 <- forecast(fit9, h = length(test))

# Plot the fitted and forecasted data
autoplot(train) + 
  autolayer(fitted(fit9), series = "Fitted") +
  autolayer(fc9, series = "Forecast") +
  xlab("Time") + ylab("Value") +
  ggtitle("ARIMA(0, 1, 2)(0, 1, 1)[12]")

# Calculate the RMSE
rmse <- sqrt(mean((fc9$mean - test)^2))

# Calculate the MAPE
mape <- mean(abs(fc9$mean - test)/test) * 100

# Print the results
cat("ARIMA(0, 1, 2)(0, 1, 1) model accuracy metrics:\n")
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

#############################################################################

#ARIMA(0, 1, 2)(0, 1, 2)[12]

(fit10 <- Arima(train, order = c(0, 1, 2), seasonal = c(0, 1, 2)))

# checking the residuals of fit10
checkresiduals(fit10)

# ggtsdisplay of the residuals
ggtsdisplay(residuals(fit10))

#plotting the forecast
fit10 %>% forecast(h=12) %>% autoplot()

# Generate out-of-sample forecasts for the test data and check the accuracy
fc10 <- forecast(fit10, h = length(test))

# Plot the fitted and forecasted data
autoplot(train) + 
  autolayer(fitted(fit10), series = "Fitted") +
  autolayer(fc10, series = "Forecast") +
  xlab("Time") + ylab("Value") +
  ggtitle("ARIMA(0, 1, 2)(0, 1, 2)[12]")

# Calculate the RMSE
rmse <- sqrt(mean((fc10$mean - test)^2))

# Calculate the MAPE
mape <- mean(abs(fc10$mean - test)/test) * 100

# Print the results
cat("ARIMA(0, 1, 2)(0, 1, 2) model accuracy metrics:\n")
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")
#############################################################################
#ARIMA(0, 2, 2)(0, 1, 1)[12]

(fit11 <- Arima(train, order = c(0, 2, 2), seasonal = c(0, 1, 2)))

# checking the residuals of fit11
checkresiduals(fit11)

# ggtsdisplay of the residuals
ggtsdisplay(residuals(fit11))

#plotting the forecast
fit11 %>% forecast(h=12) %>% autoplot()

# Generate out-of-sample forecasts for the test data and check the accuracy
fc11 <- forecast(fit11, h = length(test))

# Plot the fitted and forecasted data
autoplot(train) + 
  autolayer(fitted(fit11), series = "Fitted") +
  autolayer(fc11, series = "Forecast") +
  xlab("Time") + ylab("Value") +
  ggtitle("ARIMA(0, 2, 2)(0, 1, 1)[12]")

# Calculate the RMSE
rmse <- sqrt(mean((fc11$mean - test)^2))

# Calculate the MAPE
mape <- mean(abs(fc11$mean - test)/test) * 100

# Print the results
cat("ARIMA(0, 2, 2)(0, 1, 1) model accuracy metrics:\n")
cat("RMSE:", rmse, "\n")
# RMSE = 176515728
cat("MAPE:", mape, "%\n")
#############################################################################
# Auto seasonal Arima
(fit12 <- auto.arima(train, stepwise=FALSE, approximation=FALSE))
# auto.arima = (1,0,0)(1,1,0)[12]

# checking the residuals of fit12
checkresiduals(fit12)

#plotting the forecast
fit12 %>% forecast(h=12) %>% autoplot()

# Generate out-of-sample forecasts for the test data and check the accuracy
fc12 <- forecast(fit12, h = length(test))

# Plot the fitted and forecasted data
autoplot(train) + 
  autolayer(fitted(fit12), series = "Fitted") +
  autolayer(fc12, series = "Forecast") +
  xlab("Time") + ylab("Value") +
  ggtitle("Auto ARIMA(1,0,0)(1,1,0)[12]")

# Calculate the RMSE
rmse <- sqrt(mean((fc12$mean - test)^2))

# Calculate the MAPE
mape <- mean(abs(fc12$mean - test)/test) * 100

# Print the results
cat("ARIMA(1,0,0)(0,1,1)[12] model accuracy metrics:\n")
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")


# Our identified model ARIMA(0, 2, 2)(0, 1, 1)[12] performs much better with AICc = 7830.4

#############################################################################
#############################################################################
#Exponential Smoothing

#############################################################################
# Simple Exponential Smoothing
(fit13 <- ets(train))

# checking the residuals of fit13
checkresiduals(fit13)

# Plotting the forecast
fit13 %>% forecast(h=12) %>% autoplot()

# Generate out-of-sample forecasts for the test data and check the accuracy
fc13 <- forecast(fit13, h = length(test))

# Plot the fitted and forecasted data
autoplot(train) + 
  autolayer(fitted(fit13), series = "Fitted") +
  autolayer(fc13, series = "Forecast") +
  xlab("Time") + ylab("Value") +
  ggtitle("Simple Exponential Smoothing")

# Calculate the RMSE
rmse <- sqrt(mean((fc13$mean - test)^2))

# Calculate the MAPE
mape <- mean(abs(fc13$mean - test)/test) * 100

# Print the results
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")
#############################################################################
# Holt’s Exponential Smoothing
(fit14 <- holt(train))

# checking the residuals of fit14
checkresiduals(fit14)

# Plotting the forecast
fit14 %>% predict(fit14, n.ahead = 12) %>% autoplot()

# Generate out-of-sample forecasts for the test data and check the accuracy
fc14 <- predict(fit14, n.ahead = 12)

# Plot the fitted and forecasted data
autoplot(train) + 
  autolayer(fitted(fit14), series = "Fitted") +
  autolayer(fc14, series = "Forecast") +
  xlab("Time") + ylab("Value") +
  ggtitle("Holt’s Exponential Smoothing")

# Calculate the RMSE
rmse <- sqrt(mean((fc14$mean - test)^2))

# Calculate the MAPE
mape <- mean(abs(fc14$mean - test)/test) * 100

# Print the results
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

#############################################################################
# Holt’s Damped Exponential Smoothing
(fit15 <- holt(train, damped = TRUE))

# checking the residuals
checkresiduals(fit15)

# Plotting the forecast
fit15 %>% predict(fit15, n.ahead = 12) %>% autoplot()

# Generate out-of-sample forecasts for the test data and check the accuracy
fc15 <- predict(fit15, n.ahead = 12)

# Plot the fitted and forecasted data
autoplot(train) + 
  autolayer(fitted(fit15), series = "Fitted") +
  autolayer(fc15, series = "Forecast") +
  xlab("Time") + ylab("Value") +
  ggtitle("Holt’s Damped Exponential Smoothing")

# Calculate the RMSE
rmse <- sqrt(mean((fc15$mean - test)^2))

# Calculate the MAPE
mape <- mean(abs(fc15$mean - test)/test) * 100

# Print the results
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

#############################################################################
# Holt-Winters Additive Method
(fit16 <- hw(train,seasonal="additive"))

# checking the residuals of fit14
checkresiduals(fit16)

# Plotting the forecast
fit16 %>% forecast(h=12) %>% autoplot()

# Generate out-of-sample forecasts for the test data and check the accuracy
fc16 <- forecast(fit16, h = length(test))

# Plot the fitted and forecasted data
autoplot(train) + 
  autolayer(fitted(fit16), series = "Fitted") +
  autolayer(fc16, series = "Forecast") +
  xlab("Time") + ylab("Value") +
  ggtitle("Holt-Winters Additive Exponential Smoothing")

# Calculate the RMSE
rmse <- sqrt(mean((fc16$mean - test)^2))

# Calculate the MAPE
mape <- mean(abs(fc16$mean - test)/test) * 100

# Print the results
cat("Holt-Winters Additive Exponential Smoothing model accuracy metrics:\n")
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

#############################################################################
# Damped Holt-Winters Additive Method
(fit17 <- hw(train,seasonal="additive", damped = TRUE))

# checking the residuals of fit14
checkresiduals(fit17)

# Plotting the forecast
fit17 %>% forecast(h=12) %>% autoplot()

# Generate out-of-sample forecasts for the test data and check the accuracy
fc17 <- forecast(fit17, h = length(test))

# Plot the fitted and forecasted data
autoplot(train) + 
  autolayer(fitted(fit17), series = "Fitted") +
  autolayer(fc17, series = "Forecast") +
  xlab("Time") + ylab("Value") +
  ggtitle("Holt-Winters Additive Exponential Smoothing")

# Calculate the RMSE
rmse <- sqrt(mean((fc17$mean - test)^2))

# Calculate the MAPE
mape <- mean(abs(fc17$mean - test)/test) * 100

# Print the results
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

#############################################################################
# Holt-Winters Multiplicative Method
(fit18 <- hw(train,seasonal="multiplicative"))
# checking the residuals of fit15
checkresiduals(fit18)

# Plotting the forecast
fit18 %>% forecast(h=12) %>% autoplot()

# Generate out-of-sample forecasts for the test data and check the accuracy
fc18 <- forecast(fit18, h = length(test))

# Plot the fitted and forecasted data
autoplot(train) + 
  autolayer(fitted(fit18), series = "Fitted") +
  autolayer(fc18, series = "Forecast") +
  xlab("Time") + ylab("Value") +
  ggtitle("Holt-Winters Multiplicative Exponential Smoothing")

# Calculate the RMSE
rmse <- sqrt(mean((fc18$mean - test)^2))

# Calculate the MAPE
mape <- mean(abs(fc18$mean - test)/test) * 100

# Print the results
cat("Holt-Winters Multiplicative Exponential Smoothing model accuracy metrics:\n")
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

#############################################################################
# ETS model AAA 
# Fit ETS model with specified parameters
fit19 <- ets(train, model = "AAA")

summary(fit19)
# AICc = 6567.975

# plotting the components of the ETS fitted function
autoplot(fit19)

# checking the residuals 
cbind('Residuals' = residuals(fit19),
      'Forecast errors' = residuals(fit19,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")

checkresiduals(fit19)

# Plotting the forecast
fit19 %>% forecast(h=12) %>% autoplot()

# Generate out-of-sample forecasts for the test data and check the accuracy
fc19 <- forecast(fit19, h = length(test))

# Plot the fitted and forecasted data
autoplot(train) + 
  autolayer(fitted(fit19), series = "Fitted") +
  autolayer(fc19, series = "Forecast") +
  xlab("Time") + ylab("Value") +
  ggtitle("Exponential Smoothing AAA")

# Calculate the RMSE
rmse <- sqrt(mean((fc19$mean - test)^2))

# Calculate the MAPE
mape <- mean(abs(fc19$mean - test)/test) * 100

# Print the results
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

#############################################################################
# ETS model AAA with damping
fit20 <- ets(train, model = "AAA", damped = TRUE)

summary(fit20)
# AICc = 6568.595

# plotting the components of the ETS fitted function
autoplot(fit20)

# checking the residuals
cbind('Residuals' = residuals(fit20),
      'Forecast errors' = residuals(fit20,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")

checkresiduals(fit20)

# Plotting the forecast
fit20 %>% forecast(h=12) %>% autoplot()

# Generate out-of-sample forecasts for the test data and check the accuracy
fc20 <- forecast(fit20, h = length(test))

# Plot the fitted and forecasted data
autoplot(train) + 
  autolayer(fitted(fit20), series = "Fitted") +
  autolayer(fc20, series = "Forecast") +
  xlab("Time") + ylab("Value") +
  ggtitle("Exponential Smoothing AAA with damping")

# Calculate the RMSE
rmse <- sqrt(mean((fc20$mean - test)^2))

# Calculate the MAPE
mape <- mean(abs(fc20$mean - test)/test) * 100

# Print the results
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

#############################################################################
# ETS model MAM 
fit21 <- ets(train, model = "MAM")

summary(fit21)
# 6472.369
# plotting the components of the ETS fitted function
autoplot(fit21)

# checking the residuals 
cbind('Residuals' = residuals(fit21),
      'Forecast errors' = residuals(fit21,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")

checkresiduals(fit21)

# Plotting the forecast
fit21 %>% forecast(h=12) %>% autoplot()

# Generate out-of-sample forecasts for the test data and check the accuracy
fc21 <- forecast(fit21, h = length(test))

# Plot the fitted and forecasted data
autoplot(train) + 
  autolayer(fitted(fit21), series = "Fitted") +
  autolayer(fc21, series = "Forecast") +
  xlab("Time") + ylab("Value") +
  ggtitle("Exponential Smoothing MAM")

# Calculate the RMSE
rmse <- sqrt(mean((fc21$mean - test)^2))

# Calculate the MAPE
mape <- mean(abs(fc21$mean - test)/test) * 100

# Print the results
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

#############################################################################
# ETS model MAM with damping 
fit22 <- ets(train, model = "MAM", damped = TRUE)

summary(fit22)
# AICc = 6483.130

# plotting the components of the ETS fitted function
autoplot(fit22)

# checking the residuals
cbind('Residuals' = residuals(fit22),
      'Forecast errors' = residuals(fit22,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")

checkresiduals(fit22)

# Plotting the forecast
fit22 %>% forecast(h=12) %>% autoplot()

# Generate out-of-sample forecasts for the test data and check the accuracy
fc22 <- forecast(fit22, h = length(test))

# Plot the fitted and forecasted data
autoplot(train) + 
  autolayer(fitted(fit22), series = "Fitted") +
  autolayer(fc22, series = "Forecast") +
  xlab("Time") + ylab("Value") +
  ggtitle("Exponential Smoothing MAM with damping")

# Calculate the RMSE
rmse <- sqrt(mean((fc22$mean - test)^2))

# Calculate the MAPE
mape <- mean(abs(fc22$mean - test)/test) * 100

# Print the results
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

#############################################################################
# ETS model MAA with 
fit22 <- ets(train, model = "MAA", damped = FALSE)

summary(fit22)
# AICc = 6653.172

# plotting the components of the ETS fitted function
autoplot(fit22)

# checking the residuals
cbind('Residuals' = residuals(fit22),
      'Forecast errors' = residuals(fit22,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")

checkresiduals(fit22)

# Plotting the forecast
fit22 %>% forecast(h=12) %>% autoplot()

# Generate out-of-sample forecasts for the test data and check the accuracy
fc22 <- forecast(fit22, h = length(test))

# Plot the fitted and forecasted data
autoplot(train) + 
  autolayer(fitted(fit22), series = "Fitted") +
  autolayer(fc22, series = "Forecast") +
  xlab("Time") + ylab("Value") +
  ggtitle("Exponential Smoothing MAM with damping")

# Calculate the RMSE
rmse <- sqrt(mean((fc22$mean - test)^2))

# Calculate the MAPE
mape <- mean(abs(fc22$mean - test)/test) * 100

# Print the results
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

#############################################################################
# ETS model MAA with damping
fit23 <- ets(train, model = "MAA", damped = TRUE)

summary(fit23)
# AICc = 6564.338

# plotting the components of the ETS fitted function
autoplot(fit23)

# checking the residuals
cbind('Residuals' = residuals(fit23),
      'Forecast errors' = residuals(fit23,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")

checkresiduals(fit23)

# Plotting the forecast
fit23 %>% forecast(h=12) %>% autoplot()

# Generate out-of-sample forecasts for the test data and check the accuracy
fc23 <- forecast(fit23, h = length(test))

# Plot the fitted and forecasted data
autoplot(train) + 
  autolayer(fitted(fit23), series = "Fitted") +
  autolayer(fc23, series = "Forecast") +
  xlab("Time") + ylab("Value") +
  ggtitle("Exponential Smoothing MAM with damping")

# Calculate the RMSE
rmse <- sqrt(mean((fc23$mean - test)^2))

# Calculate the MAPE
mape <- mean(abs(fc23$mean - test)/test) * 100

# Print the results
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

#############################################################################

# Default ETS
fit24 <- ets(train)
summary(fit24)
# The model selected is ETS(M,A,M)

# plotting the components of the ETS fitted function
autoplot(fit24)

# checking the residuals of fit16 
cbind('Residuals' = residuals(fit24),
      'Forecast errors' = residuals(fit23,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")

checkresiduals(fit24)

# Plotting the forecast
fit24 %>% forecast(h=12) %>% autoplot()

# Generate out-of-sample forecasts for the test data and check the accuracy
fc24 <- forecast(fit24, h = length(test))

# Plot the fitted and forecasted data
autoplot(train) + 
  autolayer(fitted(fit24), series = "Fitted") +
  autolayer(fc24, series = "Forecast") +
  xlab("Time") + ylab("Value") +
  ggtitle("Exponential Smoothing")

# Calculate the RMSE
rmse <- sqrt(mean((fc24$mean - test)^2))

# Calculate the MAPE
mape <- mean(abs(fc24$mean - test)/test) * 100

# Print the results
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

#############################################################################



