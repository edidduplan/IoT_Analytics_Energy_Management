# Loading libraries
library(stats)
library(forecast)

# Splitting the data
trainset <- filter(energy_hour_kwh, date <= "2010-09-30")
testset <- filter(energy_hour_kwh, date > "2010-09-30")

#----- Revise----------
# Creating TS objects on total_wh
ts_train <- ts(trainset$total_kwh, frequency = 365.25, start = 2007)
ts_test <- ts(testset$total_kwh, frequency = 365.25, start = c(2010, 274))

trainsize <- round(nrow(energy_hour)*.8)

train_ind <- seq(from = 1, to = trainsize, by = 1)
trainset <- energy_hour[train_ind,]
testset <- energy_hour[-train_ind,]

# Creating TS objects on total_wh
ts_train <- ts(trainset$total_wh, frequency = 8766, start = 2007)
ts_test <- ts(testset$total_wh, frequency = 8766, start = c(2010, 1081))

autoplot(ts_train)
autoplot(ts_test)
#----------------------------------------------
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

#====================== Prophet =========================

trainset_prophet <- as.data.frame(trainset)
testset_prophet <- as.data.frame(testset)
trainset_prophet <- select(trainset_prophet, date, total_kwh)
colnames(trainset_prophet) <- c("ds", "y")
testset_prophet <- select(testset_prophet, date, total_kwh)
colnames(testset_prophet) <- c("ds", "y")

#Creating the model
fit_hour_total_prophet <- 
  prophet(trainset_prophet, yearly.seasonality = T, weekly.seasonality = T)

#Future dataset to test the Prophet model
future <- make_future_dataframe(fit_hour_total_prophet, 
                                periods = nrow(testset_prophet))

#Creating the forecast
fc_hour_total_prophet <- predict(fit_hour_total_prophet, future)

#Plotting the forecast
plot(fit_hour_total_prophet, fc_hour_total_prophet)

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

#Creating the forecast
fc_day_total_prophet <- predict(fit_day_total_prophet, future)

#Plotting the forecast
plot(fit_day_total_prophet, fc_day_total_prophet)

#Data frame for y and yhat
energy_day
