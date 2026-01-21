#install packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("forecast")
install.packages("tseries")
install.packages("TTR")
install.packages("ggplot2")
install.packages("tsibble")
install.packages("fpp3")

#load the packages
library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(TTR)
library(ggplot2)
library(tsibble)


# Reading the data
gmz <- read_csv("gmz.csv")

#basic checks
glimpse(gmz)
summary(gmz)

#converting date column to data type date
gmz <- gmz %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
  arrange(Date)

# Focus on closing price only
gmz <- gmz %>% select(Date, Close)

# Missing values
sum(is.na(gmz$Close))

# Boxplot to detect extreme values
ggplot(gmz, aes(x = "", y = Close)) +
  geom_boxplot() +
  labs(title = "Boxplot of GMZ Closing Prices",
       x = "",
       y = "Closing Price") +
  theme_minimal()

# Histogram of Closing Prices
ggplot(gmz, aes(x = Close)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(
    title = "Distribution of GMZ Closing Prices",
    x = "Closing Price",
    y = "Frequency"
  ) +
  theme_minimal()

#timeseries plot of daily closing prices
ggplot(gmz, aes(x = Date, y = Close)) +
  geom_line() +
  labs(title = "GMZ Closing Price Over Time", x = "Date", y = "Close")

#USING MONTHLY DATA
gmz_monthly <- gmz %>%
  group_by(year = year(Date), month = month(Date)) %>%
  summarise(Close = last(Close), .groups = "drop") %>%
  mutate(ym = yearmonth(paste(year, month, "01", sep = "-")))

#creating a time series of the monthly data
gmz_ts <- ts(gmz_monthly$Close,
             start = c(2013, 11),
             frequency = 12)

#plotting the monthly time series
plot (gmz_ts, main="closing price time series")

#decomposing into its components parts
fund_decomp <- decompose(gmz_ts, type="additive")
plot(fund_decomp)

#Fit Simple Exponential Smoothing (SES)
# SES (no trend, no seasonality)
ses_model <- HoltWinters(gmz_ts,
                         beta  = FALSE,   # no trend
                         gamma = FALSE)   # no seasonality

ses_model
#a plot of the actual prices and the fitted model
ses_model$fitted
plot(ses_model)
ses_model$SSE

#making a 48 months forecast
ses_forecast <- forecast(ses_model, h = 48)
plot(ses_forecast)

#acf plot of the residuals
acf(ses_forecast$residuals, lag.max=20 , na.action = na.pass)

#ljung box test
Box.test(ses_forecast$residuals, lag=20, type="Ljung-Box")

#time series plot of the residuals
plot.ts(ses_forecast$residuals)

#plotting the forecast error function
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4 
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5 
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
    mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

ses_forecast$residuals <-
  ses_forecast$residuals[!is.na(ses_forecast$residuals)]

plotForecastErrors(ses_forecast$residuals)

#Fit Holt's method 
# Holt's exponential smoothing (trend, no seasonality)
holt_model <- HoltWinters(gmz_ts,
                          gamma = FALSE)   # allow trend, no seasonal

holt_model
holt_model$fitted
plot(holt_model)
holt_model$SSE

#making a 48 month forecast
holt_forecast <- forecast(holt_model, h = 48)
plot(holt_forecast)

# Residual diagnostics
#acf plot
acf(holt_forecast$residuals, lag.max = 20, na.action = na.pass)

#Box - Ljung test
Box.test(holt_forecast$residuals, lag = 20, type = "Ljung-Box")
plot.ts(holt_forecast$residuals)

#distribution of the residuals
holt_forecast$residuals <- holt_forecast$residuals[!is.na(holt_forecast$residuals)]
plotForecastErrors(holt_forecast$residuals)

# Holt-Winters exponential smoothing (trend + seasonality)
hw_model <- HoltWinters(gmz_ts)   # alpha, beta, gamma all estimated

hw_model

#fitting the model
hw_model$fitted

#plotting the fitted model
plot(hw_model)

#checking accuracy
hw_model$SSE

#48 months forecast
hw_forecast <- forecast(hw_model, h = 48)
plot(hw_forecast)

# Residual diagnostics
acf(hw_forecast$residuals, lag.max = 20, na.action = na.pass)

#Ljung box
Box.test(hw_forecast$residuals, lag = 20, type = "Ljung-Box")


plot.ts(hw_forecast$residuals)

#distribution of the residuals
hw_forecast$residuals <- hw_forecast$residuals[!is.na(hw_forecast$residuals)]
plotForecastErrors(hw_forecast$residuals)

#### SARIMA MODEL
#plot of monthly data
plot(gmz_ts, main = "Raw Closing Price Series")

#acf of raw monthly data
#plotting acf
Acf(gmz_ts, main="ACF of Raw Series")

#differencing
diff1 <- diff(gmz_ts, differences = 1)
plot(diff1, main = "First Difference (d=1)")
Acf(diff1)
Pacf(diff1)

#remove seasonality
diff_seasonal <- diff(diff1, lag = 12, differences = 1)
plot(diff_seasonal, main = "Seasonally Differenced Series (d=1, D=1)")
Acf(diff_seasonal)
Pacf(diff_seasonal)

#fitting the sarima model
sarima_model <- arima(gmz_ts, 
                      order = c(0,1,1),
                      seasonal = list(order = c(0,1,1), period = 12))

sarima_model

sarima_fitted <- gmz_ts - residuals(sarima_model)

plot(gmz_ts,
     main = "Actual vs SARIMA Fitted",
     ylab = "Closing Price",
     xlab = "Time",
     col = "black", lwd = 2)
lines(sarima_fitted, col = "blue", lwd = 2)

legend("topright",
       legend = c("Actual", "SARIMA fitted"),
       col = c("black", "blue"), lwd = 2)

#forecast
sarima_forecast <- forecast(sarima_model, h = 48)

plot(sarima_forecast,
     main = "SARIMA(0,1,1)(0,1,1)[12] Forecast",
     xlab = "Time",
     ylab = "Closing Price")

# Residual diagnostics

# Plotting the ACF
acf(sarima_forecast$residuals, lag.max = 20, na.action = na.pass)

# Ljung-Box test
Box.test(sarima_forecast$residuals, lag = 20, type = "Ljung-Box")

# Plot residuals as a time series
plot.ts(sarima_forecast$residuals)

# Distribution of the forecast errors
sarima_forecast$residuals <- sarima_forecast$residuals[!is.na(sarima_forecast$residuals)]
plotForecastErrors(sarima_forecast$residuals)
