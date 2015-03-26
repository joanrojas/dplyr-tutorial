library(dplyr)
library(ggplot2)
library(ggvis)
require(gridExtra)


flights <- tbl_df(read.csv("flights.csv", stringsAsFactors = FALSE))
flights$date <- as.Date(flights$date)

weather <- tbl_df(read.csv("weather.csv", stringsAsFactors = FALSE))
weather$date <- as.Date(weather$date)

planes_ds <- tbl_df(read.csv("planes.csv", stringsAsFactors = FALSE))

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
#Creating a new variable speed
flights <- mutate(flights, speed = dist / (time / 60))
#-----
flights <- arrange(flights, desc(speed))
by_date <- group_by(flights, date)
filter(summarise(by_date, mean_distance = mean(dist), mean_dep_delay = mean(dep_delay, na.rm= TRUE), over_15 = mean(dep_delay, na.rm = TRUE)>15), over_15 == TRUE)

#EDA: QUESTIONS AND CHALLENGES


#Which destinations have the higher average delays? (departure)
flights %>% group_by(dest) %>% summarise(dep_d = mean(dep_delay, na.rm= TRUE), n = n())


#Which flights ( carrier+flight number + destination) happen every day? Where do they fly to?
flights %>% group_by(carrier, flight, dest) %>% summarise(n=n()) %>% filter(n == 365)



flights %>% select(dist,time) %>% mutate(speedy = dist / (time/60))

flights %>% 
  group_by(dest) %>% 
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE)) 

#For each carrier, the percentage of fligts cancelled.
flights %>% group_by(carrier) %>% summarise( Percentage_cancelled = mean(cancelled))

#For each carrier, maximum and minimum departure delay VERY NICE!!
flights %>% group_by(carrier) %>% 
  summarise_each(funs(max(., na.rm = TRUE), min(., na.rm = TRUE)), matches("delay")) %>%
  arrange(desc(dep_delay_max))

flights %>% group_by(carrier,flight,dest) %>%  summarise(times = n()) %>% filter(carrier == "AA" , times == 365)



#For each day of the year, count the total number of flights. And the evolution
flights %>%
  group_by(date) %>%
  arrange(desc(date)) %>% 
  summarise(number_of_flights = as.numeric(n())) %>% 
  mutate(slag = number_of_flights - lag(number_of_flights)) %>%
  ggplot(aes(x = date, y = slag)) + geom_line()

#Which destinations have the highest average arr_delays and dep_delay?
flights %>% 
  group_by(carrier, dest) %>%
  summarise(average_arr_delay = mean(arr_delay, na.rm = TRUE), average_dep_delay = 
              mean(dep_delay, na.rm =TRUE), n = n(), tim = mean(time, na.rm = TRUE)) %>%
  filter(carrier %in% c("UA")) %>%
  ggplot(aes(x = average_arr_delay, y = dest, size = average_dep_delay)) + 
  geom_point()

#How do delays vary on the course of the day?
flights %>%
  filter(cancelled =="0") %>%
  group_by(hour,minute) %>%
  summarise(avg_delay = mean(dep_delay, na.rm = TRUE))
 
#Now combining dplyr and ggplot 

#1. What is the average dep and arr delay for each carrier?
#Plotting a scatterplot
flights %>% 
  group_by(carrier) %>%
  summarise_each(funs(mean(., na.rm = TRUE)), matches("delay")) %>%
  ggplot(aes(x= arr_delay, y= dep_delay, color = carrier)) + 
  geom_point()

#2. How do dep_delays evolve during the day?
flights %>%
  mutate(time_of_day = hour + minute/60) %>%
  group_by(time_of_day) %>%
  summarise(dep_delay = mean(dep_delay, na.rm= TRUE), n = n()) %>%
  arrange(desc(n)) %>%
  filter(n >50) %>%
  ggplot(aes(x = time_of_day , y = dep_delay, size = n)) + 
  geom_point() +
  scale_size_area() 

#3. Plot about the probability of departure delays of each carrier.
flights %>%
  group_by(carrier, dest) %>%
  mutate(is_delayed = dep_delay >= 0, is_arr_delayed = arr_delay >= 0  ) %>%
  summarise(me_delay = mean(is_delayed, na.rm = TRUE),
            me_arr_delay = mean(is_arr_delayed, na.rm = TRUE),me_canc = mean(cancelled, na.rm = TRUE)
            , n = n ()) %>%
  filter(n >2500) %>%
  ggplot(aes(x = carrier, y = me_delay, color = dest)) + geom_point() + scale_size_area()




#PART II: Hadley Wickham

planes <- flights %>%
  filter(!is.na(arr_delay)) %>%
  group_by(plane) %>% 
  filter(n() > 30) 

planes %>%
  mutate(z_delay = (arr_delay - mean(arr_delay))/sd(arr_delay)) %>%
  filter(z_delay > 5) 


#min_rank, row_number
planes %>% 
  filter(min_rank(desc(arr_delay)) <=2 )
#for every flight, picks less than 5 top ranking in delay

#lag function takes the last value
del<- flights %>%
  group_by(date) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE)) %>%
  mutate(slag = delay - lag(delay))

  grid.arrange(ggplot(del,aes(x = date, y = delay)) + geom_line(color = "steelblue") + 
               labs( y = "delay [min]") + ggtitle("Delay evolution") ,
               ggplot(del,aes(x = date, y = slag)) + geom_line(),
               ncol= 2)

location <- airports %>%
  select(dest = iata, name = airport, lat , long)
#IATA is the destination


delays <- flights %>% 
  group_by(dest) %>% 
  summarise(arr_delay = mean(arr_delay, na.rm= TRUE)) %>%
  arrange(desc(arr_delay)) %>%
  left_join(location, by = "dest")

hourly_delay <- flights %>%
  group_by(date,hour) %>%
  filter(!is.na(dep_delay)) %>%
  summarise( delay = mean(dep_delay), n = n()) %>%
  filter(n >10)

delay_weather <- left_join(weather, hourly_delay, by = c('date', 'hour'))

ggplot(delay_weather, aes(x = temp, y = delay)) + geom_point() 
ggplot(delay_weather, aes(x = wind_speed, y = delay)) + geom_point() 

ggplot(delay_weather, aes(y = delay, x = conditions)) + geom_boxplot() + 
 labs(x = "CONDITIONS") + theme(axis.text.x= element_text(angle = 90))

ggplot(delay_weather, aes(y = delay, x = events)) + geom_boxplot() + 
  labs(x = "CONDITIONS") + theme(axis.text.x= element_text(angle = 90))

#Are older planes more likely to be delayed??
View(planes_ds)

planes_ds %>%
  group_by(type) %>%
  tally()


plane_delay <- flights %>%
  group_by(plane) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE), n = n(), dist = mean(dist, na.rm = TRUE))

plane_delay <- left_join(plane_delay, planes_ds, by = "plane") 
View(plane_delay)

plane_delay %>%
  filter(n >50) %>%
  ggplot(aes(y = delay, x = year)) + geom_point() + geom_smooth() + xlim(1980,2015 )


