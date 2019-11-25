# Load libraries
library(readr)
library(rstudioapi)
library(chron)
library(DBI)
library(RMySQL)
library(lubridate)
library(tidyverse)
library(reshape)
library(data.table)
library(plotly)
library(pryr)
library(padr)
library(imputeTS)

# Set working directory
current_path <- getActiveDocumentContext()$path
current_path
dirname(current_path)
setwd(dirname(current_path))
getwd()

# Import dataset

# power_consumption <- read.csv("Data/household_power_consumption.txt", 
#                               sep = ";", header = T)

# Data preparation
dev.off()
str(power_consumption)

power_consumption$Date <- as.Date(power_consumption$Date, format = "%d/%m/%Y")

chron(times = power_consumption[1,2])

# Reading the source of the data in a database using MySQL
# Create a database connection

con <- 
  dbConnect(
    MySQL(), 
    user='deepAnalytics', password='Sqltask1234!', 
                 dbname='dataanalytics2018', 
                 host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

# Querying the database
y_2006 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1,
           Sub_metering_2, Sub_metering_3 FROM yr_2006")
y_2007 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1,
           Sub_metering_2, Sub_metering_3 FROM yr_2007")
y_2008 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1,
           Sub_metering_2, Sub_metering_3 FROM yr_2008")
y_2009 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1,
           Sub_metering_2, Sub_metering_3 FROM yr_2009")
y_2010 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1,
           Sub_metering_2, Sub_metering_3 FROM yr_2010")

str(y_2010)
y_2006$Date <- as.Date(y_2006$Date, format = "%Y-%m-%d")
y_2007$Date <- as.Date(y_2007$Date, format = "%Y-%m-%d")
y_2008$Date <- as.Date(y_2008$Date, format = "%Y-%m-%d")
y_2009$Date <- as.Date(y_2009$Date, format = "%Y-%m-%d")
y_2010$Date <- as.Date(y_2010$Date, format = "%Y-%m-%d")

tail(y_2006)
head(y_2006)

summary(y_2006)
summary(y_2007) #complete year
summary(y_2008) #complete year
summary(y_2009) #complete year
summary(y_2010)

#========================= Creating original data frame ========================

# Create complete_years
complete_years <- bind_rows(y_2006, y_2007, y_2008, y_2009, y_2010)

#========================= Pre-processing the data ====================

# Combine date and time in one column
complete_years <- cbind(complete_years, paste(complete_years$Date, 
                                              complete_years$Time), 
                        stringsAsFactors = F)
# Moving new column to the beginning
complete_years <- complete_years[,c(ncol(complete_years), 
                                    1:(ncol(complete_years) -1))]

# Change name of new column
colnames(complete_years)[1] <- "DateTime"

# Convert DateTime from POSIXlt to POSIXct 
complete_years$DateTime <- as.POSIXct(complete_years$DateTime, 
                                      "%Y-%m-%d %H:%M:%S")

attr(complete_years$DateTime, "tzone") <- "Europe/Paris"

#---------- Filling NAs ------------------------------------------------------

complete_years <- pad(complete_years, by = "DateTime", break_above = 3)

#========================== Feature engineering ===========================
# Create "date" variable
complete_years$date <- date(complete_years$DateTime)

# Create "year" variable with lubridate
complete_years$year <- year(complete_years$DateTime)

# Create "quarter" 
complete_years$season <- quarter(complete_years$DateTime)

# Create "month" variable 
complete_years$month <- month(complete_years$DateTime)

# Create "week"
complete_years$week <- week(complete_years$DateTime)

# Create "day_of_week"
complete_years$day_of_week <- wday(complete_years$DateTime)

# Create "day" variable
complete_years$day <- day(complete_years$DateTime)

# Create "hour" variable
complete_years$hour <- hour(complete_years$DateTime)

# Create "minute" variable
complete_years$minute <- minute(complete_years$DateTime)

# Create "is_weekend" variable
complete_years$is_weekend <- is.weekend(complete_years$date)

# Create "DateTime_hour" 
complete_years <- thicken(complete_years, by = "DateTime",
                          interval = "hour")

#========================= Select variables ==============================
complete_years <- select(complete_years, date, DateTime_hour, DateTime, 
                         year : is_weekend, Sub_metering_1 : Sub_metering_3, 
                         Global_active_power)

# Filtering only complete years
#complete_years <- filter(complete_years, DateTime >= "2007-01-01 00:00:00",
#                         DateTime < "2010-01-01 00:00:00")

#========================== Treating NAs =================================
# checking for duplicates

any(duplicated(complete_years$DateTime))

#------------------ Replacing NAs in GAP ----------------------------------
# Kalman smoothing with auto.arima

id_NA <- which(is.na(complete_years$Global_active_power))
complete_years$Global_active_power[id_NA]

complete_years[id_NA,] -> complete_years_na

complete_years_na %>% group_by(date) %>% summarise(count = n()) -> date_na_list

# KS with maxgap = 200
GAP_km_2 <- na_kalman(complete_years$Global_active_power, model = "auto.arima", 
                    maxgap = 200)

complete_years$GAP_km_2 <- GAP_km_2

# Date list of NAs after KS with maxgap = 200
id_NA <- which(is.na(complete_years$GAP_km_2))

complete_years[id_NA,] %>% group_by(date) %>% summarise(count = n()) -> 
  date_na_list_2

complete_years %>% filter(date == "2007-08-01" & is.na(Global_active_power)) %>% 
  select(DateTime, Global_active_power) %>% print()

# All the count(NAs) > 21 are consequtive (at least)

# Plotting the NAs predicted by KS 
complete_years %>% filter(date == "2009-06-15") %>% 
  select(Global_active_power, GAP_km_2) -> a

any(a$Global_active_power==0, na.rm = T)

a[is.na(a)] <- 0

plot(row.names(a), a$Global_active_power)
plot(row.names(a), a$GAP_km_2)

complete_years %>% filter(date == "2010-01-12") %>% 
  select(Global_active_power, GAP_km) -> b

any(b$Global_active_power==0, na.rm = T)

b[is.na(b)] <- 0
dev.off()

plot(row.names(b), b$Global_active_power)
plot(row.names(b), b$GAP_km)

# Plotting NAs
plotNA.distribution(complete_years_na_09$Global_active_power)

ggplot(date_na_list, aes(count)) + geom_histogram(binwidth = 10)
ggplot(date_na_list_2, aes(count)) + geom_histogram(binwidth = 10)

date_na_list_2$year <- year(date_na_list_2$date)

ggplot(date_na_list_2, aes(x=year)) + geom_histogram()

# Replacing count(NAs) > 200 with 10080th previus minute (1 week)

id_NA <- which(is.na(complete_years$GAP_km_2))

complete_years[id_NA, "GAP_km_2"] <- complete_years[id_NA - 10080, "GAP_km_2"] 

any(is.na(complete_years_test$GAP_km_2))

#---------------------- Replacing NAs in SM1---------------------------

complete_years$SM1_km_2 <- na_kalman(complete_years$Sub_metering_1, 
                                     model = "auto.arima", maxgap = 200)

id_NA <- which(is.na(complete_years$SM1_km_2))

complete_years[id_NA, "SM1_km_2"] <- complete_years[id_NA - 10080, "SM1_km_2"] 


# Plotting predicitons of KS in SM1

complete_years %>% filter(date == "2010-01-05") %>% 
  select(Sub_metering_1, SM1_km_2) -> a

plot(row.names(a), a$Sub_metering_1)
plot(row.names(a), a$SM1_km_2)

#-----------------Function to predict NAs in SM2 and SM3---------------

na_predict_ks <- function(col){
  ks_prediction <- na_kalman(col, model = "auto.arima", maxgap = 200)
  id_NA <- which(is.na(ks_prediction))
  seasonal_prediction <- ks_prediction
  seasonal_prediction[id_NA] <- seasonal_prediction[id_NA - 10080] 
  verify_na <- (any(is.na(seasonal_prediction)))
  return(list(ks_prediction, seasonal_prediction, verify_na))
}

SM2_ks_2 <- na_predict_ks(complete_years$Sub_metering_2)
SM3_ks_2 <- na_predict_ks(complete_years$Sub_metering_3)

#---------------- Plotting single day to evaluate replacement of NAs

plot(filter(complete_years, date == "2007-04-30")$DateTime, 
     filter(complete_years, date == "2007-04-30")$SM1_km_2)

#----------------- Original dataframe without NAs ----------------------
complete_years$SM2_km_2 <- SM2_ks_2[[2]]
complete_years$SM3_km_2 <- SM3_ks_2[[2]]

#===================== Feature engineering II ==============================

# Calculating energy consumption of the rest
complete_years <- complete_years %>% 
  mutate(SM4_km_2 = GAP_km_2*1000/60 - SM1_km_2 - SM2_km_2 - SM3_km_2)

# Adding total energy
complete_years <- complete_years %>% mutate(total = GAP_km_2*1000/60)

# Re-selecting, filtering out 2006 and changing variable names
complete_years <- complete_years %>% select(
  date:is_weekend, kitchen = SM1_km_2, laundry = SM2_km_2, HVAC = SM3_km_2, 
  rest = SM4_km_2, total, power = GAP_km_2) %>%
  filter(!(year == 2006))

complete_years <- filter(complete_years, !(year == 2006))

#------------------ Some trial plots ----------------------------------
# Plotting 2007, February, first per minute
ggplot(filter(complete_years, year == 2007, month == 2, day == 1), 
       aes(x = DateTime)) + 
  geom_point(aes(y = kitchen), color = "blue") +
  geom_point(aes(y = laundry), color = "red") + 
  geom_point(aes(y = HVAC), color = "orange")

ggplot(filter(complete_years, year == 2007, month == 2, day >= 1, day <= 7), 
       aes(x = hour)) + geom_point(aes(y = Sub_metering_1))

#========================= Data mungling ===================================

#------------------------ Grouping by: year, month, day, hour--------------
by_hour <- group_by(complete_years, year, month, week, date, DateTime_hour, 
                    day_of_week, day, hour, is_weekend)
by_month_hour <- group_by(complete_years, month, hour, is_weekend)
by_year <- group_by(complete_years, year)
by_day <- group_by(complete_years, year, month, week, date, day_of_week, day, 
                   is_weekend)
by_month <- group_by(complete_years, year, month)


#------------------------- Summirising per month per hour --------------------
power_month_hour <- summarise(by_month_hour, power_hour = mean(power),
                              median = median(power), sd = sd(power), 
                              min = min(power), max = max(power))

# This equation has little sense
energy_month_hour <- summarise(by_month_hour, kitchen_wh = sum(kitchen), 
                               laundry_wh = sum(laundry), HVAC_wh = sum(HVAC),
                               rest_wh = sum(rest))
#-------------------------- Summirising per hour -----------------------------
energy_hour <- summarise(by_hour, 
                         kitchen_wh = sum(kitchen), 
                         laundry_wh = sum(laundry), 
                         HVAC_wh = sum(HVAC), 
                         rest_wh = sum(rest),
                         total_wh = sum(total),
                         power_kw = mean(power))

#------------------------- Summarising per day  -----------------------------
energy_day <- summarise(by_day, kitchen_wh = sum(kitchen), 
                        laundry_wh = sum(laundry), 
                        HVAC_wh = sum(HVAC), 
                        rest_wh = sum(rest),
                        total_kwh = sum(total)/1000,
                        power_kw = mean(power))

energy_day_07_09 <- filter(energy_day, 
                           year == 2007 | year == 2008 | year == 2009)

#--------------------- Summarising per month --------------------------------
energy_month <- summarise(by_month, 
                          kitchen_kwh = sum(kitchen)/1000,
                          laundry_kwh = sum(laundry)/1000,
                          HVAC_kwh = sum(HVAC)/1000,
                          rest_kwh = sum(rest)/1000,
                          total_kwh = sum(total)/1000,
                          power_kw = mean(power))

#--------------------- Summirising per year -------------------------------
energy_year <- summarise(by_year, kwh = sum(power/60), 
                         kitchen_kwh = sum(kitchen/1000), 
                         laundry_kwh = sum(laundry/1000),
                         HVAC_kwh = sum(HVAC/1000), 
                         rest_kwh = sum(rest/1000))


#========================== Adding electricity tariff ======================

#-------------------------- Adding Som Energia ------------------------------
energy_hour$som_eur <- 0
energy_hour$som_eur[energy_hour$hour >= 13 & energy_hour$hour < 23] <- .161 
energy_hour$som_eur[!(energy_hour$hour >= 13 & energy_hour$hour < 23)] <- .082

energy_hour$som_eur_x10000 <- energy_hour$som_eur * 10000

#-------------------------- Adding Holaluz ----------------------------------
energy_hour$hluz_eur <- .136

energy_hour$hluz_eur_x10000 <- energy_hour$hluz_eur * 10000


#============================ Plots =========================================

# Plotting load profile 2007, Feb, 1st
ggplot(filter(complete_years, year == 2007, week == 5), 
       aes(x = DateTime, y = power)) + 
  geom_line() + geom_smooth()

# Plotting mean power consumption for weekday in winter month (Feb) vs hour
ggplot(filter(power_month_hour, month == 2, is_weekend == F), 
       aes(x = hour, y = power_hour)) + 
  geom_line() + geom_smooth() + 
  ggtitle("Mean power consumption for weekday in winter month (Feb) vs hour")

# Plotting mean power consumption for weekend in winter month (Feb) vs hour
ggplot(filter(power_month_hour, month == 2, is_weekend == T), 
       aes(x = hour, y = power_hour)) + 
  geom_line() + geom_smooth() +
  ggtitle("Mean power consumption for weekend in winter month (Feb) vs hour")

# Plotting mean power consumption for weekday in summer month (Aug) vs hour
ggplot(filter(power_month_hour, month == 8, is_weekend == F), 
       aes(x = hour, y = power_hour)) + 
  geom_line() + geom_smooth() +
  ggtitle("Mean power consumption for weekday in summer month (Aug) vs hour")

# Plotting mean power consumption for weekend in summer month (Aug) vs hour
ggplot(filter(power_month_hour, month == 8, is_weekend == T), 
       aes(x = hour, y = power_hour)) + 
  geom_line() + geom_smooth() +
  ggtitle("Mean power consumption for weekend in summer month (Aug) vs hour")

# Plotting 2007, February, first per hour
ggplot(filter(energy_hour, year == 2007, month == 2, day == 1), 
       aes(x = hour, y = kitchen_wh)) + 
  geom_bar(stat = "identity")

ggplot(filter(energy_hour, year == 2007, month == 2, day == 1), 
       aes(x = hour, y = laundry_wh)) + 
  #geom_point()
  geom_bar(stat = "identity")

ggplot(filter(energy_hour, year == 2007, month == 2, day == 1), 
       aes(x = hour, y = HVAC_wh)) + 
  geom_bar(stat = "identity")

# Trying to put all sub_meters together
ggplot(filter(energy_hour, year == 2007, month == 2, day == 1), 
       aes(x = hour)) + 
  geom_line(aes(y = kitchen_wh), color = "blue") +
  geom_line(aes(y = laundry_wh), color = "red") +  
  geom_line(aes(y = HVAC_wh), color = "orange") +
  geom_line(aes(y = kitchen_wh + laundry_wh + HVAC_wh + rest_wh),
            color = "black") +
  geom_line(aes(y = price_eur), color = "violet") +
  xlab("hour of the day") + ylab("Energy consumption in wh") +
  ggtitle("Energy consumption for each sub-meter and price curve")

dev.off()

# Looking at specific appliances
ggplot(filter(energy_hour, year == 2007, month == 2, day == 2), 
       aes(x = hour, y = laundry_wh)) + 
  geom_bar(stat = "identity") +
  ggtitle("Energy consumption of refrigerator")

ggplot(filter(energy_hour, year == 2007, month == 2, day == 3), 
       aes(x = hour, y = laundry_wh)) + 
  geom_bar(stat = "identity") +
  ggtitle("Energy consumption of washing machine and tumble drier")

summary(complete_years$power)

# Stacking bars
energy_hour_for_melt <- filter(energy_hour, year == 2007, month == 2, day == 1)
energy_hour_for_melt <- as.data.frame(energy_hour_for_melt)

energy_hour_for_melt <- select(energy_hour_for_melt, hour, kitchen_wh,
                               laundry_wh, HVAC_wh, rest_wh)

energy_hour_melt <- melt(energy_hour_for_melt, id.vars = "hour")

ggplot(energy_hour_melt, aes(x = hour, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_x_discrete(labels = c("0" = "0", "5" = ":", "10" = "0", "15" = "0", "20" = "10:00"))
  #scale_x_time(labels = date_format("%H"))
hms::h(1)

round(5, )
energy_hour_melt_test <- energy_hour_melt

energy_hour_melt_test$hour <- as.times(energy_hour_melt_test$hour)
class(energy_hour_melt_test$hour)

scale_x_tim

class(energy_hour_melt$hour)
 ?strptime

# Using plotly
# Winter day per hour
winter_day_hour <- filter(energy_hour, year == 2007, month == 2, day == 1)
plot_ly(winter_day_hour, 
        x = ~winter_day_hour$hour, 
        y = ~winter_day_hour$kitchen_wh,
        type = "bar", mode = "bar", 
        name = "kitchen") %>%
  add_trace(y = ~winter_day_hour$laundry_wh, name = "laundry") %>% 
  add_trace(y =  ~winter_day_hour$HVAC_wh, name = "HVAC & water heating") %>%
  add_trace(y = ~winter_day_hour$rest_wh, name = "rest") %>% 
  layout(title = "Energy consumption share on 01.02.2007", 
         xaxis = list(title = "Time of the day"),
         yaxis = list(title = "energy consumption in wh"))

# Same winter day per minute
winter_day_minute <- filter(complete_years, year == 2007, month == 2, day == 1)
plot_ly(winter_day_minute, 
        x = ~winter_day_minute$DateTime, 
        y = ~winter_day_minute$kitchen,
        type = "scatter", 
        mode = "lines", 
        name = "kitchen") %>%
  add_trace(y = ~winter_day_minute$laundry, name = "laundry") %>% 
  add_trace(y =  ~winter_day_minute$HVAC, name = "HVAC & water heating") %>%
  add_trace(y = ~winter_day_minute$rest, name = "rest") %>% 
  add_trace(y = )
  layout(title = "Energy consumption share on 01.02.2007", 
         xaxis = list(title = "Time of the day"),
         yaxis = list(title = "energy consumption in wh"))

# Same winter day every 10 min
winter_day_10min <- filter(complete_years, 
                           year == 2007 & month == 2 & 
                             day == 1 & 
                             (minute == 0 | minute == 10 | minute == 20 |  
                                minute == 30 | minute == 40 | minute == 50))

plot_ly(winter_day_10min, 
        x = ~winter_day_10min$DateTime, 
        y = ~winter_day_10min$kitchen,
        type = "scatter", 
        mode = "lines", 
        name = "kitchen") %>%
  add_trace(y = ~winter_day_10min$laundry, name = "laundry") %>% 
  add_trace(y =  ~winter_day_10min$HVAC, name = "HVAC & water heating") %>%
  add_trace(y = ~winter_day_10min$rest, name = "rest") %>% 
  layout(title = "Energy consumption share on 01.02.2007", 
         xaxis = list(title = "Time of the day"),
         yaxis = list(title = "energy consumption in wh"))

# Plotting one week
winter_week <- filter(energy_hour, year == 2007 & week == 5)
plot_ly(winter_week,
        x = ~winter_week$DateTime,
        y = ~winter_week$kitchen,
        type = "scatter",
        mode = "lines",
        name = "kitchen") %>%
  add_trace(y = ~winter_week$laundry,
            name = "laundry") %>%
  add_trace(y = ~winter_week$HVAC,
            name = "HVAC")

# Plotting one week energy per day
one_week <- filter(energy_day, year == 2008, month == 06)
plot_ly(one_week,
        x = ~one_week$date,
        y = ~one_week$laundry_wh,
        type = "bar",
        mode = "bar",
        name = "total energy")


# Plotting one year
one_year <- filter(energy_day_07_09, year == 2008)

# -------------------- Exploring summer 2008 ------------------------------------
summer_2008_day <- filter(energy_day, year == 2008, month == 08)

ggplot(summer_2008_day, aes(x = date)) +
  geom_line(aes(y = total_kwh), color = "black")
#------------------
summer_2008_hour <- filter(energy_hour, year == 2008, month == 07)

ggplot(summer_2008_hour, aes(x = date, y = total_wh)) +
  geom_point()
#-----------------------
summer_2008_minute <- filter(complete_years, year == 2008, month == 08)

ggplot(summer_2008_minute, aes(x = DateTime)) +
  geom_line(aes(y = kitchen + laundry + HVAC + rest), color = "black") +
  geom_line(aes(y = kitchen), color = "red")
#---------------------------
ggplot(filter(summer_2008_minute, day == 18, hour == 12), aes(x = DateTime)) +
  geom_line(aes(y = kitchen + laundry + HVAC + rest), color = "black") +
  geom_line(aes(y = kitchen), color = "red") +
  geom_line(aes(y = laundry), color = "blue") +
  geom_line(aes(y = HVAC), color = "orange") +
  geom_line(aes(y = rest), color = "violet")
