
# task 3 
library("wbstats")
library(httr)

pops2020 <- wb_data("SP.POP.TOTL", country = "all", start_date = 2020, end_date = 2020)

pops2021 <- wb_data("SP.POP.TOTL", country = "all", start_date = 2021, end_date = 2021)

# sum for 2021 
sum(as.numeric(pops2021$SP.POP.TOTL), na.rm = T)

# sum for 2020
sum(as.numeric(pops2020$SP.POP.TOTL), na.rm = T)