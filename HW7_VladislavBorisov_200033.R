#####Problem 1#####
library(tidyverse)
library(tidyquant)
library(lubridate)
library(RcppRoll)

# AMZN, KO, NVDA, AAPL, TSLA

spy <- getSymbols("SPY", src = "yahoo", from = "2011-01-01") %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  'colnames<-'("SPY")

market_return <- na.omit(Return.calculate(spy, method = "log"))

amzn <- getSymbols("AMZN", src = "yahoo", from = "2011-01-01") %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  'colnames<-'("AMZN")
amzn_return <- na.omit(Return.calculate(amzn, method = "log"))
amzn_alpha = CAPM.alpha(amzn, market_return)
amzn_beta = CAPM.beta(amzn, market_return)

ko <- getSymbols("KO", src = "yahoo", from = "2011-01-01") %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  'colnames<-'("KO")
ko_return <- na.omit(Return.calculate(ko, method = "log"))
ko_alpha = CAPM.alpha(ko, market_return)
ko_beta = CAPM.beta(ko, market_return)

nvda <- getSymbols("NVDA", src = "yahoo", from = "2011-01-01") %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  'colnames<-'("NVDA")
nvda_return <- na.omit(Return.calculate(nvda, method = "log"))
nvda_alpha = CAPM.alpha(nvda, market_return)
nvda_beta = CAPM.beta(nvda, market_return)

aapl <- getSymbols("AAPL", src = "yahoo", from = "2011-01-01") %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  'colnames<-'("AAPL")
aapl_return <- na.omit(Return.calculate(aapl, method = "log"))
aapl_alpha = CAPM.alpha(aapl, market_return)
aapl_beta = CAPM.beta(aapl, market_return)

tsla <- getSymbols("TSLA", src = "yahoo", from = "2011-01-01") %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  'colnames<-'("TSLA")
tsla_return <- na.omit(Return.calculate(tsla, method = "log"))
tsla_alpha = CAPM.alpha(tsla, market_return)
tsla_beta = CAPM.beta(tsla, market_return)

#Problem2
Ra <- c("AMZN", "GOOG", "FB","TSLA","NFLX") %>%
  tq_get(get  = "stock.prices",
         from = "2022-01-10", to = "2022-04-28") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               col_rename = "Ra")

Rb <- "SPY" %>%
  tq_get(get  = "stock.prices",
         from = "2022-01-10", to = "2022-04-28") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               col_rename = "Rb")

RaRb <- left_join(Ra, 
                  Rb,by = "date")


#weights of 1:"AMZN", 2:"GOOG", 3:"FB", 4:"TSLA", 5:"NFLX"

weights <- runif(5)
weights <- weights / sum(weights)

base <- tibble(w1=weights[1],w2=weights[2],w3=weights[3],w4=weights[4],w5=weights[5])

mean_return <- Ra %>% group_by(symbol) %>% summarise(mr=mean(Ra))

base <- base %>% 
  add_column(ER1=mean_return$mr[1],
             ER2=mean_return$mr[2],
             ER3=mean_return$mr[3],
             ER4=mean_return$mr[4],
             ER5=mean_return$mr[5]
  )


std <- Ra %>% group_by(symbol) %>% summarise(std=StdDev(Ra))

base <- base %>% add_column(STD1=std$std[1],
                            STD2=std$std[2],
                            STD3=std$std[3],
                            STD4=std$std[4],
                            STD5=std$std[5])



AMZN <- Ra %>% filter(symbol=="AMZN") 
GOOG <- Ra %>% filter(symbol=="GOOG")
FB <- Ra %>% filter(symbol=="FB")
TSLA <- Ra %>% filter(symbol=="TSLA")
NFLX <- Ra %>% filter(symbol=="NFLX")
prices <- tibble(AMZN=AMZN$Ra,
                 GOOG=GOOG$Ra,
                 FB=FB$Ra,
                 TSLA=TSLA$Ra,
                 NFLX=NFLX$Ra)

corr_matrix <- prices %>% correlate()

base <- base %>% add_column(cov12=corr_matrix$AMZN[2],
                            cov13=corr_matrix$AMZN[3],
                            cov14=corr_matrix$AMZN[4],
                            cov15=corr_matrix$AMZN[5],
                            cov23=corr_matrix$GOOG[3],
                            cov24=corr_matrix$GOOG[4],
                            cov25=corr_matrix$GOOG[5],
                            cov34=corr_matrix$FB[4],
                            cov35=corr_matrix$FB[5],
                            cov45=corr_matrix$TSLA[5])



base <- base %>% mutate(ERT=(w1*ER1+w2*ER2+w3*ER3+w4*ER4+w5*ER5)*252)
base <- base %>% mutate(STD= w1*w2*cov12 + w1*w3*cov13 + w1*w4*cov14 + w1*w5*cov14 + 
                          w2*w3*cov23 + w2*w4*cov24 + w2*w5*cov25 + 
                          w3*w4*cov34 + w3*w5*cov35 +
                          w4*w5*cov45)
base <- base %>% mutate(STD=2*STD)
base <- base %>% mutate(STD=sqrt(STD))
base <- base %>% mutate(STD=STD*sqrt(252))




base <- base %>% mutate(SR=(ERT-0.01)/STD)


Ra <- c("AMZN", "GOOG", "FB","TSLA","NFLX") %>%
  tq_get(get  = "stock.prices",
         from = "2018-01-10", to = "2022-04-28") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               col_rename = "Ra")
Ra <- na.omit(Ra)

n_portfolio <- 1000

SR_function <- function(Ra, n_portfolio){
  portfolios <- tibble(w1=0,w2=0,w3=0,w4=0,w5=0,SR=0)
  
  for (i in 1:n_portfolio){
    weights <- runif(5)
    weights <- weights / sum(weights)
    
    base <- tibble(w1=weights[1],w2=weights[2],w3=weights[3],w4=weights[4],w5=weights[5])
    
    mean_return <- Ra %>% group_by(symbol) %>% summarise(mr=mean(Ra))
    
    base <- base %>% 
      add_column(ER1=mean_return$mr[1],
                 ER2=mean_return$mr[2],
                 ER3=mean_return$mr[3],
                 ER4=mean_return$mr[4],
                 ER5=mean_return$mr[5]
      )
    
    std <- Ra %>% group_by(symbol) %>% summarise(std=StdDev(Ra))
    
    base <- base %>% add_column(STD1=std$std[1],
                                STD2=std$std[2],
                                STD3=std$std[3],
                                STD4=std$std[4],
                                STD5=std$std[5])
    
    AMZN <- Ra %>% filter(symbol=="AMZN") 
    GOOG <- Ra %>% filter(symbol=="GOOG")
    FB <- Ra %>% filter(symbol=="FB")
    TSLA <- Ra %>% filter(symbol=="TSLA")
    NFLX <- Ra %>% filter(symbol=="NFLX")
    prices <- tibble(AMZN=AMZN$Ra,
                     GOOG=GOOG$Ra,
                     FB=FB$Ra,
                     TSLA=TSLA$Ra,
                     NFLX=NFLX$Ra)
    
    corr_matrix <- prices %>% correlate()
    
    base <- base %>% add_column(cov12=corr_matrix$AMZN[2],
                                cov13=corr_matrix$AMZN[3],
                                cov14=corr_matrix$AMZN[4],
                                cov15=corr_matrix$AMZN[5],
                                cov23=corr_matrix$GOOG[3],
                                cov24=corr_matrix$GOOG[4],
                                cov25=corr_matrix$GOOG[5],
                                cov34=corr_matrix$FB[4],
                                cov35=corr_matrix$FB[5],
                                cov45=corr_matrix$TSLA[5])
    
    base <- base %>% mutate(ERT=(w1*ER1+w2*ER2+w3*ER3+w4*ER4+w5*ER5)*252)
    base <- base %>% mutate(STD= w1*w2*cov12 + w1*w3*cov13 + w1*w4*cov14 + w1*w5*cov14 + 
                              w2*w3*cov23 + w2*w4*cov24 + w2*w5*cov25 + 
                              w3*w4*cov34 + w3*w5*cov35 +
                              w4*w5*cov45)
    base <- base %>% mutate(STD=2*STD)
    base <- base %>% mutate(STD=sqrt(STD))
    base <- base %>% mutate(STD=STD*sqrt(252))
    
    base <- base %>% mutate(SR=(ERT-0.01)/STD)
    temp <- base %>% select("w1", "w2", "w3", "w4", "w5", "SR")
    portfolios <- full_join(portfolios,temp)
  }
  return(portfolios)
}

all_portfolios<-SR_function(Ra, n_portfolio)

lg <- n_portfolio+1
all_portfolios<-all_portfolios%>% filter(row_number() %in% 2:lg)

print("The best portfolios is:")
all_portfolios %>% arrange(desc(SR)) %>% filter(row_number()==1)