# Business Forecasting - End Term Examination

# Name: Febin Francis
# REG.NO: 2327216
# Class: MBA BA

#Inserting the dataset into R Studio

mydata=read.csv("~/Desktop/Crude_Price_Exogenous_Factors.csv", header=T)
names(mydata)
attach(mydata)
View(mydata)
class(mydata)

summary(mydata)
dim(mydata)
str(mydata)
class(mydata)
print(head(mydata))
head(mydata)
tail(mydata)



#Decomposition of the Data

# Display the structure of the dataset
str(mydata)
# Convert Period column to Date format
mydata$Period <- as.Date(paste0(mydata$Period, "01"), format = "%Y %b %d")

# Create a time series object for the Issues column
Price_ts <- ts(mydata$Crude_Price, start = c(2014, 1), frequency = 12)

# View the time series object
plot(Price_ts, main="Crude Oil Price (2014 - 2024)", ylab="Price", xlab="Monthly")


# Decompose the time series
decomposed_issues <- decompose(Price_ts)

# Plot the decomposition
plot(decomposed_issues)


## Arima Model


# Load necessary libraries
library(forecast)
library(tseries)

# Convert Date column to Date format
mydata$Date <- as.Date(paste0(mydata$Period, "01"), format = "%Y %b %d")

# Create a time series object for the Issues column
Price_ts <- ts(mydata$Crude_Price, start = c(2014, 7), end = c(2024, 7), frequency = 12)

# Print the time series object
print(Price_ts)

# Plot the time series with custom Y-axis (in increments of 50) and X-axis (months)
plot(Price_ts, type = "l", xaxt = "n", yaxt = "n", xlab = "Months", ylab = "Crude Price", main = "Crude Price Over Time")
axis(1, at = seq(2014, 2024, by = 1), labels = seq(2014, 2024, by = 1))
axis(2, at = seq(min(Price_ts, na.rm = TRUE), max(Price_ts, na.rm = TRUE), by = 20))

# Plot the time series
plot(Price_ts, type = "l", xaxt = "n", xlab = "Time", ylab = "Issues", main = "Crude Price Over Time")
years <- seq(from = 2014, to = 2024, by = 1)
axis(1, at = years, labels = years)
points(years, Price_ts[(years - 2014) * 12 + 1], col = "red", pch = 35)

# Plot ACF and PACF
acf(Price_ts)  #graphis dying down. so p value in p,d,q in 0
pacf(Price_ts) #two spike is above blue line..so q is 2

# Perform ADF test for stationarity
adf.test(Price_ts) #p value is 0.088 ..>0.05 so its not stationary..make it stationary

# Determine the number of differences needed to make the series stationary
ndiffs(Price_ts)  # no. of differentiation is 1 d=1

# Fit the ARIMA model
Crude_Price_ARIMA <- arima(Price_ts, order = c(0, 1, 2))  # You can adjust the order based on ACF, PACF, and ndiffs results
print(Crude_Price_ARIMA)

# Forecast using the ARIMA model
Crude_Price_ARIMA_Forecast <- forecast(Crude_Price_ARIMA , level = c(95), h = 6)  # Forecasting 6 months ahead
print(Crude_Price_ARIMA_Forecast)

# Plot the forecast
plot(Crude_Price_ARIMA_Forecast, main = "Forecast of Crude Price", xlab = "Months", ylab = "Crude Price")
points(years, Price_ts[(years - 2014) * 12 + 1], col = "blue", pch = 25)

# Validate the forecast with Ljung-Box test
Box.test(Crude_Price_ARIMA_Forecast$residuals, lag = 2, type = "Ljung-Box")
Box.test(Crude_Price_ARIMA_Forecast$residuals, lag = 4, type = "Ljung-Box")
Box.test(Crude_Price_ARIMA_Forecast$residuals, lag = 6, type = "Ljung-Box")


# Calculate AIC
aic_value <- AIC(Crude_Price_ARIMA)
print(paste("AIC:", aic_value))

# Calculate BIC
bic_value <- BIC(Crude_Price_ARIMA)
print(paste("BIC:", bic_value))

# Calculate RMSE
fitted_values <- fitted(Crude_Price_ARIMA)
rmse_value <- sqrt(mean((Price_ts - fitted_values)^2, na.rm = TRUE))
print(paste("RMSE:", rmse_value))


#Auto ARIMA

# Automatically fit the best ARIMA model
autoarr <- auto.arima(Price_ts)
print(autoarr)

# Check residuals for auto ARIMA model
acf(ts(autoarr$residuals))
pacf(ts(autoarr$residuals))

#Forecast using Auto ARIMA
forecast_autoarr <- forecast(autoarr, level = c(95), h = 6)
print(forecast_autoarr)
plot(forecast_autoarr)

# Calculate AIC
aic_value <- AIC(autoarr)
print(paste("AIC:", aic_value))

# Calculate BIC
bic_value <- BIC(autoarr)
print(paste("BIC:", bic_value))

# Calculate RMSE
fitted_values <- fitted(autoarr)
rmse_value <- sqrt(mean((Price_ts - fitted_values)^2, na.rm = TRUE))
print(paste("RMSE:", rmse_value))



##SARIMA


# Fit the SARIMA model
# The order is chosen based on ACF, PACF, and seasonal decomposition, but can be adjusted.
# (p, d, q) = (non-seasonal AR order, non-seasonal differencing, non-seasonal MA order)
# (P, D, Q) = (seasonal AR order, seasonal differencing, seasonal MA order)
# 's' is the seasonal period (12 for monthly data)

# You might want to adjust the (p, d, q) and (P, D, Q) 
Crude_Price_SARIMA <- arima(Price_ts, order = c(1, 1, 1), 
                            seasonal = list(order = c(1, 1, 1), period = 12))

# Print the SARIMA model summary
print(Crude_Price_SARIMA)

# Forecast using the SARIMA model
Crude_Price_SARIMA_Forecast <- forecast(Crude_Price_SARIMA, level = c(95), h = 6)  # Forecasting 6 months ahead
print(Crude_Price_SARIMA_Forecast)

# Plot the forecast
plot(Crude_Price_SARIMA_Forecast, main = "SARIMA Forecast of Crude Price", xlab = "Months", ylab = "Crude Price")

# Validate the SARIMA model forecast with Ljung-Box test
Box.test(Crude_Price_SARIMA_Forecast$residuals, lag = 2, type = "Ljung-Box")
Box.test(Crude_Price_SARIMA_Forecast$residuals, lag = 4, type = "Ljung-Box")
Box.test(Crude_Price_SARIMA_Forecast$residuals, lag = 6, type = "Ljung-Box")

# Calculate AIC for SARIMA model
aic_value_sarima <- AIC(Crude_Price_SARIMA)
print(paste("AIC:", aic_value_sarima))

# Calculate BIC for SARIMA model
bic_value_sarima <- BIC(Crude_Price_SARIMA)
print(paste("BIC:", bic_value_sarima))

# Calculate RMSE for SARIMA model
fitted_values_sarima <- fitted(Crude_Price_SARIMA)
rmse_value_sarima <- sqrt(mean((Price_ts - fitted_values_sarima)^2, na.rm = TRUE))
print(paste("RMSE:", rmse_value_sarima))


#Auto SARIMA

auto_sarima <- auto.arima(Price_ts, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)

# Print the summary of the auto SARIMA model
print(auto_sarima)

# Forecast using the auto SARIMA model
auto_sarima_forecast <- forecast(auto_sarima, level = c(95), h = 6)  # Forecasting 6 months ahead
print(auto_sarima_forecast)

# Plot the forecast
plot(auto_sarima_forecast, main = "Auto SARIMA Forecast of Crude Price", xlab = "Months", ylab = "Crude Price")

# Validate the auto SARIMA model forecast with Ljung-Box test
Box.test(auto_sarima_forecast$residuals, lag = 2, type = "Ljung-Box")
Box.test(auto_sarima_forecast$residuals, lag = 4, type = "Ljung-Box")
Box.test(auto_sarima_forecast$residuals, lag = 6, type = "Ljung-Box")

# Calculate AIC for auto SARIMA model
aic_value_auto_sarima <- AIC(auto_sarima)
print(paste("AIC:", aic_value_auto_sarima))

# Calculate BIC for auto SARIMA model
bic_value_auto_sarima <- BIC(auto_sarima)
print(paste("BIC:", bic_value_auto_sarima))

# Calculate RMSE for auto SARIMA model
fitted_values_auto_sarima <- fitted(auto_sarima)
rmse_value_auto_sarima <- sqrt(mean((Price_ts - fitted_values_auto_sarima)^2, na.rm = TRUE))
print(paste("RMSE:", rmse_value_auto_sarima))




##ARIMAX



# Prepare the exogenous variables (USD_Currency_Index, US_Inflation)
# Ensure they are numeric and convert to a matrix
exog_vars <- as.matrix(mydata[, c("USD_Currency_Index","US_Inflation", "US_CPI" )])

# Fit the ARIMAX model with the exogenous variables
Crude_Price_ARIMAX <- auto.arima(Price_ts, xreg = exog_vars)

# Print the ARIMAX model summary
summary(Crude_Price_ARIMAX)

# Forecast using the ARIMAX model
# To make a forecast, you'll need future values of the exogenous variables
# For simplicity, using the last known values (this can be replaced with actual forecasts of exog_vars)
future_exog_vars <- tail(exog_vars, 6)  # Assuming a 6-month forecast
Crude_Price_ARIMAX_Forecast <- forecast(Crude_Price_ARIMAX, xreg = future_exog_vars, level = c(95), h = 6)

# Print the forecast
print(Crude_Price_ARIMAX_Forecast)

# Plot the forecast
plot(Crude_Price_ARIMAX_Forecast, main = "ARIMAX Forecast of Crude Price", xlab = "Months", ylab = "Crude Price")

# Validate the ARIMAX model forecast with Ljung-Box test
Box.test(Crude_Price_ARIMAX_Forecast$residuals, lag = 2, type = "Ljung-Box")
Box.test(Crude_Price_ARIMAX_Forecast$residuals, lag = 4, type = "Ljung-Box")
Box.test(Crude_Price_ARIMAX_Forecast$residuals, lag = 6, type = "Ljung-Box")

# Calculate AIC for ARIMAX model
aic_value_arimax <- AIC(Crude_Price_ARIMAX)
print(paste("AIC:", aic_value_arimax))

# Calculate BIC for ARIMAX model
bic_value_arimax <- BIC(Crude_Price_ARIMAX)
print(paste("BIC:", bic_value_arimax))

# Calculate RMSE for ARIMAX model
fitted_values_arimax <- fitted(Crude_Price_ARIMAX)
rmse_value_arimax <- sqrt(mean((Price_ts - fitted_values_arimax)^2, na.rm = TRUE))
print(paste("RMSE:", rmse_value_arimax))




##SARIMAX



# Prepare the exogenous variables (USD_Currency_Index, US_Inflation, US_CPI)
# Ensure they are numeric and convert to a matrix
exog_vars <- as.matrix(mydata[, c("USD_Currency_Index", "US_Inflation", "US_CPI")])

# Fit the Auto SARIMAX model with the exogenous variables
Crude_Price_Auto_SARIMAX <- auto.arima(Price_ts, xreg = exog_vars, seasonal = TRUE)

# Print the Auto SARIMAX model summary
summary(Crude_Price_Auto_SARIMAX)

# Forecast using the Auto SARIMAX model
# Assuming you want to forecast 6 months ahead
# Use the last available values for the exogenous variables or forecasted values if available
future_exog_vars <- tail(exog_vars, 6)  # Adjust this based on your needs
Crude_Price_Auto_SARIMAX_Forecast <- forecast(Crude_Price_Auto_SARIMAX, xreg = future_exog_vars, level = c(95), h = 6)

# Print the forecast
print(Crude_Price_Auto_SARIMAX_Forecast)

# Plot the forecast
plot(Crude_Price_Auto_SARIMAX_Forecast, main = "Auto SARIMAX Forecast of Crude Price", xlab = "Months", ylab = "Crude Price")

# Validate the Auto SARIMAX model forecast with Ljung-Box test
Box.test(Crude_Price_Auto_SARIMAX_Forecast$residuals, lag = 2, type = "Ljung-Box")
Box.test(Crude_Price_Auto_SARIMAX_Forecast$residuals, lag = 4, type = "Ljung-Box")
Box.test(Crude_Price_Auto_SARIMAX_Forecast$residuals, lag = 6, type = "Ljung-Box")

# Calculate AIC for Auto SARIMAX model
aic_value_auto_sarimax <- AIC(Crude_Price_Auto_SARIMAX)
print(paste("AIC:", aic_value_auto_sarimax))

# Calculate BIC for Auto SARIMAX model
bic_value_auto_sarimax <- BIC(Crude_Price_Auto_SARIMAX)
print(paste("BIC:", bic_value_auto_sarimax))

# Calculate RMSE for Auto SARIMAX model
fitted_values_auto_sarimax <- fitted(Crude_Price_Auto_SARIMAX)
rmse_value_auto_sarimax <- sqrt(mean((Price_ts - fitted_values_auto_sarimax)^2, na.rm = TRUE))
print(paste("RMSE:", rmse_value_auto_sarimax))





#ARCH and GARCH






# Load necessary libraries
library(tseries)
library(fGarch)
library(rugarch)

# Convert the 'Period' column to a Date format
mydata$Period <- as.Date(paste0(mydata$Period, "01"), format = "%Y %b %d")

# Create a time series object for the 'Issues' column
Crude_Price_ts <- ts(mydata$Crude_Price, start = c(2014, 1), frequency = 12)

# Plot the time series to visualize it
plot(Crude_Price_ts, main="Crude Price Time Series", ylab="Crude Price", xlab="Period")

# Fit an ARCH model
arch_model <- garch(Crude_Price_ts, order = c(0, 1))
summary(arch_model)

# Plot the residuals of the ARCH model
ts.plot(arch_model$residuals, main="ARCH Model Residuals")

# Fit a GARCH(1,1) model using rugarch
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0)),
  distribution.model = "std"
)
garch_model <- ugarchfit(spec = garch_spec, data = Crude_Price_ts)
summary(garch_model)

# Plot the residuals of the GARCH model
ts.plot(residuals(garch_model), main="GARCH Model Residuals")

# Plot the fitted GARCH volatility
plot(sigma(garch_model), type="l", main="GARCH Model Volatility", ylab="Volatility", xlab="Period")

# Forecast the next 6 months using the GARCH model
garch_forecast <- ugarchforecast(garch_model, n.ahead = 6)
forecast_values <- fitted(garch_forecast)
print(forecast_values)
forecast_volatility <- sigma(garch_forecast)
plot(forecast_values, main = "Garch Forecast of Crude Price", xlab = "Months", ylab = "Crude Price")

# Calculate AIC and BIC for the GARCH model
aic_garch <- infocriteria(garch_model)[1]
bic_garch <- infocriteria(garch_model)[2]
print(paste("AIC for GARCH model:", aic_garch))
print(paste("BIC for GARCH model:", bic_garch))


