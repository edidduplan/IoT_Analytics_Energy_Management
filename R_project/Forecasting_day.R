# Loading libraries
library(stats)
library(forecast)
library(prophet)

# Splitting the data
trainsize <- round(nrow(energy_day)*.8)

train_ind <- seq(from = 1, to = trainsize, by = 1)
trainset <- energy_day[train_ind,]
testset <- energy_day[-train_ind,]

# Creating TS objects on total_wh
ts_train <- ts(trainset$total_wh, frequency = 8766, start = 2007)
ts_test <- ts(testset$total_wh, frequency = 8766, start = c(2010, 1081))

autoplot(ts_train)
autoplot(ts_test)

# auto ARIMA

ts_train_arima <- auto.arima(ts_train, approximation=FALSE, stepwise=FALSE)


class(ts_train_arima)
checkresiduals(ts_train_arima)

summary(ts_train_arima)

fc_ts_train_arima <- forecast::forecast(ts_train_arima, 6480)

#pr_ts_train_arima <- predict(ts_train_arima, n.ahead = 9)
autoplot(fc_ts_train_arima)

#plot(pr_ts_train_arima$pred)

#testset$pred <- fc_ts_train_arima$pred


# Simple forecasting methods
fc_ts_train_snaive <- snaive(ts_train, h = 9)

autoplot(fc_ts_train_snaive)

# PLOTTING prediction togehter with train + testsets

#ts.plot(ts_train, ts_test, fc_ts_train_arima$pred, fc_ts_train_snaive, 
#        col = c(1,1,2, 3))

# Cross validation
far2 <- function(x, h){forecast::forecast(auto.arima(x), h=h)}

cv_ts_train_arima <- tsCV(ts_train, far2, h = 1)

rmse_cv_ts_train_arima <- sqrt(mean(cv_ts_train_arima^2, na.rm = T))

# Accuracy
accuracy(fc_ts_train_arima, ts_test)
accuracy(fc_ts_train_snaive, ts_test)

# Plotting the models
autoplot(ts_train) +
  autolayer(ts_test) +
  autolayer(fc_ts_train_snaive, series="Snaive", PI= F) +
  autolayer(fc_ts_train_arima, series="Arima", PI=FALSE) +
  xlab("Year") + ylab("Global active power in kw") +
  ggtitle("Forecasts for Global active power in kw") +
  guides(colour=guide_legend(title="Forecast"))

#-------------------- Prophet -------------------------------

energy_day_power <- as.data.frame(energy_day)
energy_day_power <- select(energy_day_power, date, power_kw)
colnames(energy_day_power) <- c("ds", "y")
#energy_day_power <- filter(energy_day_power, date >= "2009-01-01")

#Splitting the data
trainsize <- round(nrow(energy_day_power)*.8)

train_ind <- seq(from = 1, to = trainsize, by = 1)
trainset <- energy_day_power[train_ind,]
testset <- energy_day_power[-train_ind,]

plot(trainset)
plot(testset)

#Creating the model
fit_day_power_prophet <- 
  prophet(trainset, yearly.seasonality = T, weekly.seasonality = F)

#Future dataset to test the Prophet model
future <- make_future_dataframe(fit_day_power_prophet, periods = nrow(testset))

#Creating the forecast
fc_day_power_prophet <- predict(fit_day_power_prophet, future)

#Plotting the forecast
plot(fit_day_power_prophet, fc_day_power_prophet)

#Cross validation and Computing the errors on the testset
cv_day_power_prophet <- 
  cross_validation(
    fit_day_power_prophet, 
    initial = 3 * nrow(testset),
    period = nrow(testset)/2,
    horizon = nrow(testset), 
    units = 'days')

per_day_power_prophet <- performance_metrics(cv_day_power_prophet)

plot_cross_validation_metric(cv_day_power_prophet, metric = 'rmse')

#Errors on the testset
testset_per <- cbind(testset, filter(fc_day_power_prophet, ds >= "2010-02-15"))

testset_per <- testset_per[,c(1:2,ncol(testset_per))]

testset_per$error_prophet <- testset_per$y - testset_per$yhat
mean(abs(testset_per$error_prophet))

#Creating ts objects
ggplot(testset_per, aes(x = ds)) +
  geom_line(aes(y = y), color = "black") + 
  geom_line(aes(y = yhat), color = "red")

