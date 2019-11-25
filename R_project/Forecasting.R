# Loading libraries
library(stats)
library(forecast)

# Splitting the data
trainsize <- round(nrow(energy_month_2)*.8)

train_ind <- seq(from = 1, to = trainsize, by = 1)
trainset <- energy_month_2[train_ind,]
testset <- energy_month_2[-train_ind,]

# Creating TS objects
ts_train <- ts(trainset$power_kw, frequency = 12, start = c(2007,1))
ts_test <- ts(testset$power_kw, frequency = 12, start = c(2010,2))

autoplot(ts_train)
autoplot(ts_test)

#ARIMA

ts_train_arima <- auto.arima(ts_train, approximation=FALSE, stepwise=FALSE)


class(ts_train_arima)
checkresiduals(ts_train_arima)

summary(ts_train_arima)

fc_ts_train_arima <- forecast::forecast(ts_train_arima, 9)

pr_ts_train_arima <- predict(ts_train_arima, n.ahead = 9)
autoplot(fc_ts_train_arima)

plot(pr_ts_train_arima$pred)

testset$pred <- fc_ts_train_arima$pred


# Simple forecasting methods
fc_ts_train_snaive <- snaive(ts_train, h = 9)

autoplot(fc_ts_train_snaive)

# PLOTTING prediction togehter with train + testsets

ts.plot(ts_train, ts_test, fc_ts_train_arima$pred, fc_ts_train_snaive, 
        col = c(1,1,2, 3))

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
