library(tidyverse)
install.packages("nycflights13")
library(nycflights13)
?nycflights13
??nycflights13
str(flights)
head(flights)
# Filtering flights on January 1st
filter(flights, month == 1, day == 1)
# base R
flights[flights$year == 1 & flights$month == 1,]

# Arrange rows
arrange(flights, year, month, day)
arrange(flights, desc(dep_delay))

# base R
flights[order(flights$dep_delay, decreasing = T),]

# Select variables
select(flights, carrier, flight, dep_delay : arr_delay)
select(flights, carrier, flight, contains("delay"))

# base R
flights[,c(3,4,5:7)]

# Mutate to add new columns
flights %>% mutate(gain = arr_delay - dep_delay) %>% select(arr_delay, dep_delay,
                                                            gain)
# Transmutate keeps only transformed variable
transmute(flights, gain = arr_delay - dep_delay)

# Summarise (lets see what is it good for...)
summarise(flights, delay = mean(arr_delay, na.rm = T))

# base R
mean(flights$dep_delay, na.rm = T)

# Grouped operations
by_tailnum <- group_by(flights, tailnum)
delay <- summarise(by_tailnum, 
                   count = n(),
                   delay = mean(arr_delay, na.rm = T),
                   distance = mean(distance, na.rm = T))

# Plotting delay vs distance
ggplot(delay, aes(distance, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()
