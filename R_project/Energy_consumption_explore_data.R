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

# Set working directory
current_path <- getActiveDocumentContext()$path
current_path
dirname(current_path)
setwd(dirname(current_path))
getwd()

# Import dataset

power_consumption <- read.csv("Data/household_power_consumption.txt", 
                              sep = ";", header = T)

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

dbListTables(con)[2]
dbListFields(con, "iris")

dbListFields(con, "yr_2006")

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

# Create complete_years
complete_years <- bind_rows(y_2006, y_2007, y_2008, y_2009, y_2010)

str(complete_years)
summary(complete_years)
head(complete_years)
tail(complete_years)

# Combine data and time in one column
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

# Create "date" variable
complete_years$date <- date(complete_years$DateTime)

# Create "year" variable with lubridate
complete_years$year <- year(complete_years$DateTime)

# Create "quarter" 
complete_years$season <- quarter(complete_years$DateTime)


levels(complete_years$season)

# Create "month" variable 
complete_years$month <- month(complete_years$DateTime)

# Create "week"
complete_years$week <- week(complete_years$DateTime)

# Create "day" variable
complete_years$day <- day(complete_years$DateTime)

# Create "hour" variable
complete_years$hour <- hour(complete_years$DateTime)

# Create "minute" variable
complete_years$minute <- minute(complete_years$DateTime)

# Create "is_weekend" variable
complete_years$is_weekend <- is.weekend(complete_years$date)

# Filtering only complete years
complete_years <- filter(complete_years, DateTime >= "2007-01-01 00:00:00",
                         DateTime < "2010-01-01 00:00:00")


# Select variables
complete_years <- select(complete_years, date, DateTime, year : minute, 
                         is_weekend, Sub_metering_1 : Sub_metering_3, 
                         Global_active_power)

# Calculating energy consumption of the rest
complete_years <- complete_years %>% mutate(Sub_metering_4 = 
                                              Global_active_power*1000/60 - 
                                              Sub_metering_1 - Sub_metering_2 - 
                                              Sub_metering_3)

# Changing variable names
complete_years <- complete_years %>% rename(kitchen = Sub_metering_1,
                                            laundry = Sub_metering_2,
                                            HVAC = Sub_metering_3,
                                            rest = Sub_metering_4,
                                            power = Global_active_power)

# and re-ordering columns
# complete_years <- complete_years[,c(1:ncol(complete_years), 11, 10)]

# Plotting 2007, February, first per minute
ggplot(filter(complete_years, year == 2007, month == 2, day == 1), 
       aes(x = DateTime)) + 
  geom_point(aes(y = kitchen), color = "blue") +
  geom_point(aes(y = laundry), color = "red") + 
  geom_point(aes(y = HVAC), color = "orange")

ggplot(filter(complete_years, year == 2007, month == 2, day >= 1, day <= 7), 
       aes(x = hour)) + geom_point(aes(y = Sub_metering_1))

# Plotting 2007, February, 1st - 7th
ggplot(filter(complete_years, year == 2007, month == 2, day == 1), 
       aes(x = DateTime)) + 
  geom_point(aes(y = Sub_metering_1), color = "blue") +
  geom_point(aes(y = Sub_metering_2), color = "red") + 
  geom_point(aes(y = Sub_metering_3), color = "orange") +
  geom_point(aes(y = Sub_metering_4), color = "violet") +
  guides(color = "legend") +
  theme(legend.position = "right")

ggplot(filter(complete_years, year == 2007, month == 2, day >= 1, day <= 7), 
       aes(x = hour)) + geom_point(aes(y = Sub_metering_1))

# Grouping by: year, month, day, hour
# ------------- Do not delete! -----------------------
by_hour <- group_by(complete_years, year, month, day, hour, is_weekend)
by_month_hour <- group_by(complete_years, month, hour, is_weekend)
by_year <- group_by(complete_years, year)
# -----------------------------------------------------

# Summirising per month per hour
power_month_hour <- summarise(by_month_hour, power_hour = mean(power),
                              median = median(power), sd = sd(power), 
                              min = min(power), max = max(power))

# This equation has little sense
energy_month_hour <- summarise(by_month_hour, kitchen_wh = sum(kitchen), 
                               laundry_wh = sum(laundry), HVAC_wh = sum(HVAC),
                               rest_wh = sum(rest))
# Summirising per hour
energy_hour <- summarise(by_hour, kitchen_wh = sum(kitchen), 
                              laundry_wh = sum(laundry), 
                              HVAC_wh = sum(HVAC), 
                              rest_wh = sum(rest))

# Adding electricity tariff
energy_hour$price_eur[energy_hour$hour >= 13 & energy_hour$hour < 23] <- .161 
energy_hour$price_eur[!(energy_hour$hour >= 13 & energy_hour$hour < 23)] <- .082

energy_hour$price_eur <- energy_hour$price_eur * 10000


# Summirising per year
energy_year <- summarise(by_year, kwh = sum(power/60), 
                         kitchen_kwh = sum(kitchen/1000), 
                         laundry_kwh = sum(laundry/1000),
                         HVAC_kwh = sum(HVAC/1000), 
                         rest_kwh = sum(rest/1000))

# Plotting per hour
ggplot(filter(consumption_hour, year == 2007, month == 2, day == 1), 
       aes(x = DateTime)) + 
  geom_point(aes(y = Sub_metering_1), color = "blue") +
  geom_point(aes(y = Sub_metering_2), color = "red") + 
  geom_point(aes(y = Sub_metering_3), color = "orange") +
  geom_point(aes(y = Sub_metering_4), color = "violet") +
  guides(color = "legend") +
  theme(legend.position = "right")

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
winter_day <- filter(energy_hour, year == 2007, month == 2, day == 1)
plot_ly(winter_day, x = ~winter_day$hour, y = ~winter_day$kitchen_wh,
        type = "bar", mode = "bar", name = "kitchen") %>%
  add_trace(y = ~winter_day$laundry_wh, name = "laundry") %>% 
  add_trace(y =  ~winter_day$HVAC_wh, name = "HVAC & water heating") %>%
  add_trace(y = ~winter_day$rest_wh, name = "rest") %>% 
  layout(title = "Energy consumption share on 01.02.2007", )
