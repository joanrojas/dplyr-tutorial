library(dplyr)
library(ggplot2)
library(ggvis)

flights <- tbl_df(read.csv("flights.csv", stringsAsFactors = FALSE))
flights$date <- as.Date(flights$date)

weather <- tbl_df(read.csv("weather.csv", stringsAsFactors = FALSE))
weather$date <- as.Date(weather$date)

planes <- tbl_df(read.csv("planes.csv", stringsAsFactors = FALSE))

airports <- tbl_df(read.csv("airports.csv", stringsAsFactors = FALSE))

#----FLIGHTS DATA SET--------------------

filter(flights, dest %in% c('OAK','SFO'))
filter(flights, hour >= 0 & hour <=5 )
filter(flights, dep_delay > 60)
filter(flights, arr_delay > 2*dep_delay)
help(select)

#Selecting delay variables
select(flights, dep_delay, arr_delay)
select(flights, ends_with('delay', ignore.case = TRUE))
select(flights, one_of('dep_delay', 'arr_delay'))
select(flights, contains('delay'))


#Arrange function
help(arrange)
arrange(flights, date, hour, minute)

arrange(flights, desc(dep_delay))
arrange(flights, dep_delay - arr_delay)
names(flights)
flights <- mutate(flights, speed = dist / (time / 60))
flights <- arrange(flights, desc(speed))
by_date <- group_by(flights, date)
filter(summarise(by_date, mean_distance = mean(dist), mean_dep_delay = mean(dep_delay, na.rm= TRUE), over_15 = mean(dep_delay, na.rm = TRUE)>15), over_15 == TRUE)

#EDA: QUESTIONS AND CHALLENGES


#Which destinations have the higher average delays? (departure)
flights %>% group_by(dest) %>% summarise(dep_d = mean(dep_delay, na.rm= TRUE)) %>% arrange(desc(dep_d))


#Which flights ( carrier+flight) happen every day? Where do they fly to?
flights %>% group_by(carrier, flight, dest) %>% summarise(n=n()) %>% filter(n == 365)


#
