library(tidyverse)
library(nycflights13)
nycflights13::flights
view(flights)

#Problem 1 ----
  
outcomes <- c("win", "lose")

money <- 100
bet <- 1

counter <- 0

for(k in 1:1000){
 
  while(money > 0) {
    cointoss <- sample(outcomes, 1 , prob = c(04,06))
    
    
    if(cointoss == "win"){
      money <- money + bet
    }
    else{
      money <- money - bet
      bet <- min(money, bet * 2)
    }
    print(money)
  }
}










# Problem 5.2.4 ---- 

#Problem 5.2.4.1
delay2 <- filter(flights, arr_delay >= 120 )
destin <- filter(flights, dest == "IAH" | dest == "HOU" )
carri_er <- filter(flights, carrier %in% c("AA", "DL", "UA"))
summer <- filter(flights, month >= 7, month <= 9)
late_notlate <- filter(flights, arr_delay > 120, dep_delay <= 0)
made_up <- filter(flights, dep_delay >= 60, dep_delay - arr_delay > 30)
midnight <- filter(flights, dep_time <= 600 | dep_time == 2400)

#Problem 5.2.4.2 
summer2_0 <- filter(flights, between(month, 7, 9))

#Problem 5.2.4.3
misInfor <- filter(flights, is.na(dep_time))

#Problem 5.2.4.4 
NA ^ 0 # every number powered on 0 is 1 

# Problem 5.3.1 ---- 

#1 
arrangeNA <- arrange(flights, desc(is.na(dep_time)), dep_time)

#2 
topDelayed <- arrange(flights, desc(dep_delay))

#3 
fastest <- arrange(flights, desc(distance / air_time))

#4
farthest <- arrange(flights, desc(distance))

# Problem 5.4.1 ---- 

#1
select(flights, year, month, day)
select(flights, "year", "month", "day")
select(flights, 1, 2, 3)

#2
select(flights, year, month, year, year) #it does not include them 

#3 
choice <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, any_of(choice))

# Problem 5.5.2
#1
Times_change <- mutate(flights,
                        dep_time_mins = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
                        sched_dep_time_mins = (sched_dep_time %/% 100 * 60 +
                                                 sched_dep_time %% 100) %% 1440)

#4 
rankme <- tibble(
  x = c(10, 5, 1, 5, 5)
)