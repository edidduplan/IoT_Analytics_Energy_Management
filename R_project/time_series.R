# Starting with Time series 

#------------------Loading libraries--------------------
library(fpp2)
library(aTSA)
library(tseries)
library(forecast)
library(urca)


#----------------- Timeseries total energy per month -----------------------
energy_month_2 <- filter(energy_month, !(year == 2010 & month == 11))
ts_total_energy_month <- ts(energy_month_2$total_kwh, frequency = 12, 
                            start = c(2007, 1))
autoplot(ts_total_energy_month) +
  ggtitle("Overall Energy consumption in kwh per month") +
  xlab("Year") +
  ylab("Energy consumption kwh/month")

#-------------------- TS total energy per day ----------------------------
ts_energy_day <- ts(energy_day, frequency = 365.25,
                          start = c(2007, 1))

# Plot energy per day for one week
autoplot(window(ts_energy_day[,12], 
                start = c(2008, 7), 
                end = c(2008, 14))) +
  ggtitle("Overall Energy consumption in kwh per day for one week") +
  xlab("1 week") +
  ylab("Energy consumption kwh/day")

# Plot energy per day for two weeks
autoplot(window(ts_energy_day[,12], 
                start = c(2008, 7), 
                end = c(2008, 20))) +
  ggtitle("Overall Energy consumption in kwh per day for two weeks") +
  xlab("2 weeks") +
  ylab("Energy consumption kwh/day")

# Plot energy per day for 2007 - 2011
autoplot(ts_energy_day[,12]) +
  ggtitle("Overall Energy consumption in kwh per day") +
  xlab("Year") +
  ylab("Energy consumption kwh/day")

#------------------ TS total energy per hour -----------------
energy_hour_07_08_09_10 <- energy_hour %>% filter(!year == 2006) %>% 
  select(date, everything())

ts_energy_hour <- ts(energy_hour$total_wh, frequency = 8766, start = 2007)

# Plot energy per hour one week
autoplot(window(ts_energy_hour, 
                start = c(2009, 101), 
                end = c(2009, 268))) +
  ggtitle("Overall Energy consumption in wh per hour for one week") +
  xlab("1 week") +
  ylab("Energy consumption wh/hour")

# PLot energy per hour one month (4 weeks = 672 h)
autoplot(window(ts_energy_hour, 
                start = c(2009, 701), 
                end = c(2009, 1372))) +
  ggtitle("Overall Energy consumption in wh per hour for 4 week") +
  xlab("4 week") +
  ylab("Energy consumption wh/hour")

# Decomposing
dec_ts_total_energy_month <- decompose(ts_total_energy_month)
stl_ts_total_energy_month <- stl(ts_total_energy_month, s.window = 5)


plot(dec_ts_total_energy_month)
autoplot(stl_ts_total_energy_month)

stl_ts_total_energy_month %>% forecast(method="naive") %>%
  autoplot() + ylab("Energy consumption kwh/month")

# Checking stationarity
acf(ts_total_energy_month, lag.max = 12)
adf.test(ts_total_energy_month)
# Differencing
diff_ts <- diff(ts_total_energy_month)
autoplot(diff_ts)

diff_diff_ts <- diff(diff_ts)
autoplot(diff_diff_ts)
acf(diff_ts)
adf.test(diff_ts)

acf(diff_diff_ts)
adf.test(diff_diff_ts)

#==================== Time series - granurality = hour =======================
#--------------- Creating ts object --------------------------------

msts_hour <- msts(
  energy_hour$total_wh, 
  seasonal.periods = c(8766, 168, 24), 
  start = c(2007))

#-------------------  Decomposing ------------------------------
#Decomposing 
dec_msts_hour <- decompose(msts_hour)
stl_msts_hour <- mstl(msts_hour)

dec_ts_hour <- decompose(ts_energy_hour)
stl_ts_hour <- mstl(ts_energy_hour)

autoplot(stl_msts_hour)

autoplot(stl_ts_hour)

# Checking stationarity

adf.test(ts_energy_hour)
# p-value < 0.01 => therefore you can reject the null-hypothesis. 
# The alternative hypothesis is that the ts is stationary

summary(ur.kpss(ts_energy_hour))
#With the KPSS test 1.9093 > 1% significance level of confidence value
#This means we can not reject the null hypothesis of ts being stationary


