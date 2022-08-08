#Task 3 – Combine data to calculate the ratio of all types of COVID-19 cases 
#(confirmed, recovered, death/mortality-rate) to the population 
#(mortality rate = # of deaths in population per unit of time) 
                                                                                                                                              
# task 3 
library("wbstats")
library(httr)
library(stringr)
library(tidyr)

pops2021 <- as.data.frame(wb_data("SP.POP.TOTL", country = "all", start_date = 2021, end_date = 2021))

###removing iso2c columns including numbers and NA-values 
pops2021_2 <- filter(pops2021, !grepl("[0-9]", iso2c)) 
pops2021_2 <- pops2021_2[!is.na(pops2021_2$iso2c),]
pops2021_2 <- pops2021_2 %>% drop_na(iso2c)

pops2021_2 <- filter(pops2021_2, !grepl("countries", country)) 
pops2021_2 <- filter(pops2021_2, !grepl("only", country)) 
pops2021_2 <- filter(pops2021_2, !grepl("income", country)) 
pops2021_2 <- filter(pops2021_2, !grepl("total", country)) 
pops2021_2 <- filter(pops2021_2, !grepl("blend", country)) 
pops2021_2 <- filter(pops2021_2, !grepl("Union", country))  

View(pops2021_2)

# sum of population for 2021 
sum(as.numeric(pops2021$SP.POP.TOTL), na.rm = T)


