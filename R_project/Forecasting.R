# Loading libraries
library(stats)
library(forecast)

# Splitting the data
trainsize <- round(nrow(energy_month)*.8)

train_ind <- seq(from = 1, to = trainsize, by = 1)
trainset <- energy_month[train_ind,]
testset <- energy_month[-train_ind,]

# Creating TS objects
ts_train <- ts(trainset$power_kw, frequency = 12, start = c(2007,1))
ts_test <- ts(testset$power_kw, frequency = 12, start = c(2010,3))

autoplot(ts_train)
autoplot(ts_test)

#ARIMA

ts_train_arima_month <- auto.arima(ts_train, approximation=FALSE, stepwise=FALSE)


class(ts_train_arima_month)
checkresiduals(ts_train_arima_month)

summary(ts_train_arima_month)

fc_ts_train_arima_month <- forecast::forecast(ts_train_arima_month, 9)

pr_ts_train_arima <- predict(ts_train_arima, n.ahead = 9)
autoplot(fc_ts_train_arima_month)

plot(pr_ts_train_arima$pred)

testset$pred <- fc_ts_train_arima$pred


# Simple forecasting methods
fc_ts_train_snaive_month <- snaive(ts_train, h = 9)

autoplot(fc_ts_train_snaive_month)

# PLOTTING prediction togehter with train + testsets

ts.plot(ts_train, ts_test, fc_ts_train_arima_month$pred, fc_ts_train_snaive_month, 
        col = c(1,1,2, 3))

# Cross validation
far2 <- function(x, h){forecast::forecast(auto.arima(x), h=h)}

cv_ts_train_arima <- tsCV(ts_train, far2, h = 1)

rmse_cv_ts_train_arima <- sqrt(mean(cv_ts_train_arima^2, na.rm = T))

# Accuracy
accuracy(fc_ts_train_arima_month, ts_test)
accuracy(fc_ts_train_snaive_month, ts_test)

# Plotting the models
autoplot(ts_train) +
  autolayer(ts_test) +
  autolayer(fc_ts_train_snaive_month, series="Snaive", PI= F) +
  autolayer(fc_ts_train_arima_month, series="Arima", PI=FALSE) +
  xlab("Year") + ylab("Global active power in kw") +
  ggtitle("Forecasts for Global active power in kw") +
  guides(colour=guide_legend(title="Forecast"))

