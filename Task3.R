
# task 3 
library("wbstats")
library(httr)
library(stringr)
library(tidyr)

# pops2020 <- wb_data("SP.POP.TOTL", country = "all", start_date = 2020, end_date = 2020)

pops2021 <- as.data.frame(wb_data("SP.POP.TOTL", country = "all", start_date = 2021, end_date = 2021))

##regex attempt -- help plz 
pops2021 %>% str_extract(iso2c, [[:digit:]]) 

pops2021 %>% filter(pops2021, grepl("[0-9]") == T)

### without numbers 

pops2021.2 <- filter(pops2021, !grepl("[0-9]", iso2c))


drop_na(pops2021.2$iso2c)

# sum for 2021 
sum(as.numeric(pops2021$SP.POP.TOTL), na.rm = T)

# sum for 2020
sum(as.numeric(pops2020$SP.POP.TOTL), na.rm = T)

