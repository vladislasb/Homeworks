#4.1 For each carrier what is the most common destination?
flights %>% 
  group_by(carrier, dest) %>% 
  count(dest) %>%
  group_by(carrier) %>%
  filter(rank(desc(n)) < 2)

#4.2 For each carrier what is the biggest delay?
flights %>% 
  group_by(carrier) %>% 
  summarise(carrier,delay = arr_delay - dep_delay) %>%
  group_by(carrier) %>%
  filter(rank(desc(delay)) < 2)


#4.3 Which are the three plane which have flown the most/least miles?
flights %>% arrange(distance) 
flights %>% arrange(desc(distance))


#4.4 What are the first/last flights for each day in February 2013?
filter(flights,year==2013,month==12) %>% arrange(dep_time) 
filter(flights,year==2013,month==12) %>% arrange(desc(dep_time))


#4.5 Which company flew the most miles in March 2013? Which flew the least?
filter(flights,month==3,year==2013) %>% arrange(desc(distance))
filter(flights,month==3,year==2013) %>% arrange(air_time)

#4.6 Which month had the most delays over 60 minutes?
flights %>% mutate(delay=arr_delay-dep_delay) %>% filter(delay > 60) %>% 
  group_by(month) %>% summarise(n=n()) %>%
  arrange(desc(n))


#4.7 What is the average time between two consecutive flights?

#4.8 Use the SDFunction function from exercise 2 to calculate the standard deviation
#of the flight delays for each month and for each destination