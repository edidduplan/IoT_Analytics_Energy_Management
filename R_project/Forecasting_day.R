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
#------------------------ h = 4 days
train_forecast_day_d <- function(col, d){
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

train_total_day_1 <- train_forecast_day_d(energy_day$total_kwh, 1)

train_total_day_57 <- train_forecast_day_d(energy_day$total_kwh, 57)

#------------------- Error of forecasting a whole month ----------------
#------------ ARIMA ---------------
ts <- ts(energy_day$total_kwh, frequency = 365.25, start = 2007)
ts_train <- window(ts, start = 2007, end =c(2010, 274))
ts_test <- window(ts, start = c(2010, 274), end = c(2010, 274 + 57))
mean_arima <- train_total_day_57[["fc_arima"]][["mean"]]


forecast_57_d <- as.data.frame(testset$date)
forecast_57_d$y <- as.data.frame(ts_test)
forecast_57_d$yhat <- as.data.frame(mean_arima)
forecast_57_d <- as.data.frame(forecast_57_d)
colnames(forecast_57_d) <- c("date", "y", "yhat")
forecast_57_d$date <- as.Date(forecast_57_d$date)

sy1 <- sum(filter(forecast_57_d, date < "2010-11-01")$y)
sy1hat <- sum(filter(forecast_57_d, date < "2010-11-01")$yhat)

sy2 <- sum(filter(forecast_57_d, date >= "2010-11-01")$y)
sy2hat <- sum(filter(forecast_57_d, date >= "2010-11-01")$yhat)

perr1 <- (sy1hat - sy1)/sy1*100
perr2 <- (sy2hat - sy2)/sy2*100

autoplot(train_total_day_57[["fc_arima"]]) +
  autolayer(train_total_day_57[["test"]]) 


train_total_day_4[["accuracy"]]

#----------------------------- Cross validation
far2 <- function(x, h){forecast::forecast(auto.arima(x), h=h)}

cv_ts_train_arima <- tsCV(ts_train, far2, h = 1)

rmse_cv_ts_train_arima <- sqrt(mean(cv_ts_train_arima^2, na.rm = T))

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
  prophet(trainset_prophet, yearly.seasonality = T, weekly.seasonality = T)

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

sy1hat_p <- sum(filter(testset_per, ds < "2010-11-01")$yhat)

sy2hat_p <- sum(filter(testset_per, ds >= "2010-11-01")$yhat)

sy1 <- sum(filter(testset_per, ds < "2010-11-01")$y)

sy2 <- sum(filter(testset_per, ds >= "2010-11-01")$y)

perr1_p <- (sy1hat_p - sy1)/sy1*100
perr2_p <- (sy2hat_p - sy2)/sy2*100

#---------------- MOdel Selection -----------------------------
# Selcted model is prophet with yearly and weekly seasonality
# MPAE on the monthly total energy consumption is
MPAE_monthly <- (abs(perr1_p) + abs(perr2_p))/2

#---------------- Training on the whole data -----------------

energy_day_prophet <- select(as.data.frame(energy_day), date, total_kwh)
colnames(energy_day_prophet) <- c("ds", "y")

#Creating the model
fit_day_total_prophet <- 
  prophet(energy_day_prophet, yearly.seasonality = T, weekly.seasonality = T)

#Future dataset to test the Prophet model
future <- make_future_dataframe(fit_day_total_prophet, 
                                periods = 35)
future_2 <- make_future_dataframe(fit_day_total_prophet, 
                                periods = 365)

#Creating the forecast
fc_day_total_prophet <- predict(fit_day_total_prophet, future)
fc_day_total_prophet_2 <- predict(fit_day_total_prophet, future_2)

#Plotting the forecast

dev.off()
plot(filter(fc_day_total_prophet, ds > "2010-11-27")$yhat)
ggplot(filter(fc_day_total_prophet_2, ds > "2010-11-27"), aes(x = ds, y = yhat)) +
  geom_line()


#Data frame for y and yhat
energy_day_total <- select(as.data.frame(energy_day), date, total_kwh)
names(energy_day_total$date) <- "ds"

horizon <- select(fc_day_total_prophet_2, date = ds, yhat_lower, yhat_upper, yhat)
horizon <- filter(horizon, date > "2010-11-27")
horizon$total_kwh <- 0
horizon <- horizon[,c(1, ncol(horizon), 2:(ncol(horizon)-1))]

energy_day_total$yhat_lower <- 0
energy_day_total$yhat_upper <- 0
energy_day_total$yhat <- 0

energy_day_total <- rbind(energy_day_total, horizon)

#---------------- Writing data for Tableau -----------------------------
write.csv(energy_day_total, "energy_day_total_2.csv", row.names = F)
