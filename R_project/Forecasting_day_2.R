# Loading libraries
library(stats)
library(forecast)
library(prophet)

# Splitting the data
trainset <- filter(energy_day, date <= "2010-09-30")
testset <- filter(energy_day, date > "2010-09-30")


# Creating TS objects on total_wh
ts_train <- ts(trainset$total_kwh, frequency = 365.25, start = 2007)
ts_test <- ts(testset$total_kwh, frequency = 365.25, start = c(2010, 274))


#----------------------- auto ARIMA --------------------------------

ts_train_arima <- auto.arima(ts_train, approximation=FALSE, stepwise=FALSE)


class(ts_train_arima)
checkresiduals(ts_train_arima)

summary(ts_train_arima)

fc_ts_train_arima <- forecast::forecast(ts_train_arima, 57)

#pr_ts_train_arima <- predict(ts_train_arima, n.ahead = 9)
autoplot(fc_ts_train_arima)

#plot(pr_ts_train_arima$pred)

#testset$pred <- fc_ts_train_arima$pred

#---------------------- Function to get forecast error metrics ----------------

train_forecast_day <- function(col){
  ts <- ts(col, frequency = 365.25, start = 2007)
  ts_train <- window(ts, start = 2007, end =c(2010, 274))
  ts_test <- window(ts, start = c(2010, 274))
  ts_train_arima <- auto.arima(ts_train, approximation=FALSE, stepwise=FALSE)
  fc_ts_train_snaive <- snaive(ts_train, h = 57)
  fc_ts_train_arima <- forecast::forecast(ts_train_arima, 57)
  acc <- rbind(accuracy(fc_ts_train_arima, ts_test), 
        accuracy(fc_ts_train_snaive, ts_test))
  return(list(fc_arima = fc_ts_train_arima, fc_snaive = fc_ts_train_snaive, 
              accuracy = acc, test = ts_test))
}

train_total_day <- train_forecast_day(energy_day$total_kwh)

autoplot(train_total_day[["fc_arima"]]) +
  autolayer(train_total_day[["test"]])

#---------------------- Function to get forecast error metrics ----------------
#------------------------ h = 4 days
train_forecast_day <- function(col, d){
  ts <- ts(col, frequency = 365.25, start = 2007)
  ts_train <- window(ts, start = 2007, end =c(2010, 274))
  ts_test <- window(ts, start = c(2010, 274), end = c(2010, 274 + d))
  ts_train_arima <- auto.arima(ts_train, approximation=FALSE, stepwise=FALSE)
  fc_ts_train_snaive <- snaive(ts_train, h = d)
  fc_ts_train_arima <- forecast::forecast(ts_train_arima, d)
  acc <- rbind(accuracy(fc_ts_train_arima, ts_test), 
               accuracy(fc_ts_train_snaive, ts_test))
  return(list(fc_arima = fc_ts_train_arima, fc_snaive = fc_ts_train_snaive, 
              accuracy = acc, test = ts_test))
}

train_total_day_4 <- train_forecast_day(energy_day$total_kwh, 4)

autoplot(train_total_day[["fc_arima"]]) +
  autolayer(train_total_day[["test"]])

train_total_day_4[["accuracy"]]

#----------------------------- Cross validation
ts <- ts(energy_day$total_kwh, frequency = 365.25, start = 2007)
far2 <- function(x, h){forecast::forecast(auto.arima(x), h=h)}

cv_ts_arima <- tsCV(ts, far2, h = 1)

rmse_cv_ts_train_arima <- sqrt(mean(cv_ts_train_arima^2, na.rm = T))
rmse_cv_ts_train_arima <- sqrt(mean(cv_ts_train_arima/, na.rm = T))


#----------------------------- Accuracy
accuracy(fc_ts_train_arima, ts_test)
accuracy(fc_ts_train_snaive, ts_test)

#---------------------------- Plotting the models
autoplot(ts_train) +
  autolayer(ts_test) +
  autolayer(fc_ts_train_snaive, series="Snaive", PI= F) +
  autolayer(fc_ts_train_arima, series="Arima", PI=FALSE) +
  xlab("Year") + ylab("Global active power in kw") +
  ggtitle("Forecasts for Global active power in kw") +
  guides(colour=guide_legend(title="Forecast"))

#====================== Prophet =========================

trainset_prophet <- as.data.frame(trainset)
testset_prophet <- as.data.frame(testset)
trainset_prophet <- select(trainset_prophet, date, total_kwh)
colnames(trainset_prophet) <- c("ds", "y")
testset_prophet <- select(testset_prophet, date, total_kwh)
colnames(testset_prophet) <- c("ds", "y")

#Creating the model
fit_day_total_prophet <- 
  prophet(trainset_prophet, yearly.seasonality = T, weekly.seasonality = F)

#Future dataset to test the Prophet model
future <- make_future_dataframe(fit_day_total_prophet, 
                                periods = nrow(testset_prophet))

#Creating the forecast
fc_day_total_prophet <- predict(fit_day_total_prophet, future)

#Plotting the forecast
plot(fit_day_total_prophet, fc_day_total_prophet)

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
testset_per <- cbind(testset_prophet, 
                     filter(fc_day_total_prophet, ds >= "2010-10-01"))

testset_per <- testset_per[,c(1:2,ncol(testset_per))]

testset_per$error_prophet <- abs(testset_per$y - testset_per$yhat)
testset_per$perror_prophet <- testset_per$error_prophet/testset_per$y*100
mean(abs(testset_per$error_prophet))
mean(testset_per$perror_prophet)

#Creating ts objects
ggplot(testset_per, aes(x = ds)) +
  geom_line(aes(y = y), color = "black") + 
  geom_line(aes(y = yhat), color = "red")

#