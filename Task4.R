#Task 3 – Combine data to calculate the ratio of all types of COVID-19 cases 
#(confirmed, recovered, death/mortality-rate) to the population 
#(mortality rate = # of deaths in population per unit of time) 

# task 3 
library("wbstats")
library(httr)
library(stringr)
library(tidyr)
library(jsonlite)
library(readxl)

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
pops2021_2 <- filter(pops2021_2, !grepl("&", country))
pops2021_2 <- filter(pops2021_2, !grepl("area", country))  
pops2021_2 <- filter(pops2021_2, !grepl("members", country))  
pops2021_2 <- filter(pops2021_2, !grepl("North America", country))  
pops2021_2 <- filter(pops2021_2, !grepl("Sub", country))  
pops2021_2 <- filter(pops2021_2, !grepl("Union", country))  


# sum of population for 2021 
world_pop <- sum(as.numeric(pops2021$SP.POP.TOTL), na.rm = T)


#copying code from Task 1, afterwards import in RDS object to obtain confirmed, deaths and recovered 
library(httr)

res <- VERB("GET", url = "https://covid19-stats-api.herokuapp.com/api/v1/cases?")

cat(content(res, 'text'))

totalconfirmed_cases <- content(res)$confirmed
totalconfirmed_deaths <- content(res)$deaths
totalconfirmed_recovered <- content(res)$recovered

world_pop/totalconfirmed_cases

world_pop/totalconfirmed_deaths

world_pop/totalconfirmed_recovered

#### Task 4 ######

# Adding excel file of gini rates (from world bank)
#Gini index < 0.2 represents perfect income equality, 0.2–0.3 relative equality, 
#0.3–0.4 adequate equality, 0.4–0.5 big income gap, and above 0.5 represents severe income gap.
gini <- read_excel("gini.xls", skip = 3)
View(gini)   

na_count <-sapply(gini, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count) # Lets use the year 2018 for this (most data points)

gini_clean <- gini %>% select(`Country Name`, `Country Code`, `2018`) %>% filter(!is.na(`2018`))
View(gini_clean)

# Making gini categories 
gini_categories <- gini_clean %>%
  mutate(gini_equaltiy = case_when(
    `2018` < 20 ~ "Perfect",
    `2018` <= 30 ~ "Relative",
    `2018` <= 40 ~ "Adequate",
    `2018` <= 50 ~ "Big gap",
    `2018` > 50 ~ "Severe gap"
  ))
View(gini_categories)

#############

# Adding population density 
pop.density <- read_excel("density.xls", skip = 3)
View(pop.density)

pop.density_clean <- pop.density %>%
  select(`Country Name`, `Country Code`, `2021`) %>%
  filter(!is.na(`2021`))
View(pop.density_clean)

# Making populaiton density categories
# <= 100 - extremely  low
# 101 - 250 - low
# 251 = 500 - moderate
# 501 - 1000 - high
# > 1000 - very high

pop.density_categories <- pop.density_clean %>%
  mutate(pop.density_categories = case_when(
    `2021` <= 100 ~ "Extremely low",
    `2021` <= 250 ~ "Low",
    `2021` <= 500 ~ "Moderate",
    `2021` <= 1000 ~ "High",
    `2021` > 1000 ~ "Very high"
  ))
View(pop.density_categories)

###MERGING COVID and COUNTRY-CATEGORIES by Country

############################
#Confirmed Cases by Country
############################
library(httr)
res <- VERB("GET", url = "https://covid19-stats-api.herokuapp.com/api/v1/cases/country/confirmed")
country_list <- list()
count_list <- list()

#removing 1 row of list 
unlisted_res <- unlist(content(res),recursive=FALSE)

#adding countries to separate list 
for (i in 1:length(unlisted_res)) {
  if (i %% 2 == 0) {
    country_list <- c(country_list,unlisted_res[i])
  }
}

#adding counts to separate list 
for (i in 1:length(unlisted_res)) {
  if (i %% 2 != 0) {
    count_list <- c(count_list,unlisted_res[i])
  }
}

confirmed_cases_country <- do.call(rbind, Map(cbind, count_list, country_list))
confirmed_cases_country <- as.data.frame(confirmed_cases_country)
confirmed_cases_country %>% mutate(Country = V2) %>% mutate(Number = V1)  %>% select(-V2) %>% select(-V1)

library(plyr)
df_covid_gini <- left_join(confirmed_cases_country,gini_categories,by=c("Country" = "Country Name"))
View(df_covid_gini)


df_covid_pop_density <- left_join(confirmed_cases_country,pop.density_categories,by=c("Country" = "Country Name"))
View(df_covid_pop_density)


