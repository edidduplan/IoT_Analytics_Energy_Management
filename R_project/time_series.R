# Starting with Time series 

#------------------Loading libraries--------------------
library(fpp2)
library(aTSA)
library(tseries)

# Timeseries total energy per day 2007 - 2009
ts_total_energy <- ts(energy_day_07_09$total_kwh, 
                      frequency = 364.66, 
                      start = 2007)

plot.ts(ts_total_energy)

autoplot(ts_total_energy) +
  ggtitle("Overall Energy consumption in kwh per day") +
  xlab("Year") +
  ylab("Energy consumption kwh/d")

ggseasonplot(ts_total_energy, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("year") +
  ggtitle("Seasonal plot: ")

# Timeseries total energy per day 2006 - 2010
ts_total_energy <- ts(energy_day$total_kwh, frequency = 364.66, 
                      start = c(2006, 350))

plot.ts(ts_total_energy)

autoplot(ts_total_energy) +
  ggtitle("Overall Energy consumption in kwh per day") +
  xlab("Year") +
  ylab("Energy consumption kwh/day")

# Timeseries total energy per month without Nov 2010
energy_month_2 <- filter(energy_month, !(year == 2010 & month == 11))
ts_total_energy_month <- ts(energy_month_2$total_kwh, frequency = 12, 
                            start = c(2007, 1))
autoplot(ts_total_energy_month) +
  ggtitle("Overall Energy consumption in kwh per month") +
  xlab("Year") +
  ylab("Energy consumption kwh/month")

# TS total energy per day - one year
energy_day_2008 <- filter(energy_day, year == 2008)
ts_energy_day <- ts(energy_day_2008, frequency = 366,
                          start = c(2008, 1))

autoplot(window(ts_energy_day[,12], 
                start = c(2008, 7), 
                end = c(2008, 21))) +
  ggtitle("Overall Energy consumption in kwh per day") +
  xlab("Year") +
  ylab("Energy consumption kwh/day")

# TS total energy per hour
energy_hour_07_08_09_10 <- energy_hour %>% filter(!year == 2006) %>% 
  select(date, everything())

ts_energy_hour <- ts(energy_hour_07_08_09_10, frequency = 8766, start = 2007)

autoplot(window(ts_energy_hour[,ncol(ts_energy_hour)], 
                start = c(2009, 100), 
                end = c(2009, 197))) +
  ggtitle("Overall Energy consumption in kwh per hour") +
  xlab("Year") +
  ylab("Energy consumption kwh/hour")



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
