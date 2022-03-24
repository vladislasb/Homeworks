#Problem 1 ----

for (i in (1:10)) {
  print(i*3)
}


#Problem 2 ----

for (v in rnorm(10)) {
  if (v > 1) 
  print (v)
}

#Problem 3 ---- 


people <- c(rep("men", 6), rep("women", 8))
ResultV <- NULL

for (i in 1:10000){ 
  choose <- sample(people, size = 5, replace = FALSE)
  
  if (sum(choose=="men")==3){
    ResultV<-c(ResultV,1)
  } else{
    ResultV<-c(ResultV,0)
  }
  
}
sum(ResultV)/10000






#Problem 4 ----

profit = 0
strikePrice = 120


for(k in 1:1000){
  
  price <- 100
  
  for (x in 1:100){
    price <- price + rnorm(1, mean = 0, sd = 7)
  }
  
  if(price >= strikePrice){
    profit <- profit + (price - strikePrice)
  }
}
print(profit/1000)
       

