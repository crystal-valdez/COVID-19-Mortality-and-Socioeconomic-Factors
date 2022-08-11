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
library(dplyr)
library(plyr)


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


### FIGURING OUT WHATS THE BEST VARIABLE FOR CONFIRMED CASES AND MORTALITY

############################
#Confirmed Cases in each country
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
confirmed_cases_country$Country <- confirmed_cases_country$V2
confirmed_cases_country$Number <- confirmed_cases_country$V1
confirmed_cases_country <- confirmed_cases_country %>% select(-V2) %>% select(-V1)
View(confirmed_cases_country)

#replacing 30 countries names for matching - second column is what you are changing it to 
confirmed_cases_country$Country[confirmed_cases_country$Country == 'US'] <- 'United States'
confirmed_cases_country$Country[confirmed_cases_country$Country == 'Korea, South'] <- 'Korea, Rep.'
confirmed_cases_country$Country[confirmed_cases_country$Country == 'Korea, North'] <- "People's Rep."
confirmed_cases_country$Country[confirmed_cases_country$Country == 'Russia'] <- "Russian Federation"
confirmed_cases_country$Country[confirmed_cases_country$Country == 'Turkey'] <- "Turkiye"
confirmed_cases_country$Country[confirmed_cases_country$Country == 'Iran'] <- "Islamic Rep."
confirmed_cases_country$Country[confirmed_cases_country$Country == 'Czechia'] <- "Czech Republic"
confirmed_cases_country$Country[confirmed_cases_country$Country == 'Slovakia'] <- "Slovak Republic" 
confirmed_cases_country$Country[confirmed_cases_country$Country == 'Burma'] <- "Myanmar"
confirmed_cases_country$Country[confirmed_cases_country$Country ==  'Venezuela'] <-"Venezuela, RB"
confirmed_cases_country$Country[confirmed_cases_country$Country == 'Egypt'] <- "Egypt, Arab Rep."
confirmed_cases_country$Country[confirmed_cases_country$Country == 'Kyrgyzstan'] <- "Kyrgyz Republic"
confirmed_cases_country$Country[confirmed_cases_country$Country == 'Laos'] <- "Lao PDR"

confirmed_cases_country$Country[confirmed_cases_country$Country == 'Congo (Kinshasa)'] <- "Congo, Dem. Rep."
confirmed_cases_country$Country[confirmed_cases_country$Country == 'Syria'] <- "Syrian Arab Republic"
confirmed_cases_country$Country[confirmed_cases_country$Country == 'Bahamas'] <- "Bahamas, The"
confirmed_cases_country$Country[confirmed_cases_country$Country == 'Congo (Brazzaville)'] <- "Congo, Rep."
confirmed_cases_country$Country[confirmed_cases_country$Country ==  'Brunei'] <-"Brunei Darussalam"
confirmed_cases_country$Country[confirmed_cases_country$Country == 'Saint Lucia'] <- "St. Lucia"
confirmed_cases_country$Country[confirmed_cases_country$Country == 'Gambia'] <- "Gambia, The"
confirmed_cases_country$Country[confirmed_cases_country$Country == 'Yemen'] <- "Yemen, Rep."
confirmed_cases_country$Country[confirmed_cases_country$Country == 'Saint Vincent and the Grenadines'] <- "St. Vincent and the Grenadines"
confirmed_cases_country$Country[confirmed_cases_country$Country ==  'Saint Kitts and Nevis'] <-"St. Kitts and Nevis"
confirmed_cases_country$Country[confirmed_cases_country$Country == 'Micronesia'] <- "Micronesia, Fed. Sts."

############################
#Mortality in each country
############################
library(httr)
res <- VERB("GET", url = "https://covid19-stats-api.herokuapp.com/api/v1/cases/country/deaths")
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

death_cases_country <- do.call(rbind, Map(cbind, count_list, country_list))
death_cases_country <- as.data.frame(death_cases_country)
death_cases_country$Country <- death_cases_country$V2
death_cases_country$Number <- death_cases_country$V1
death_cases_country <- death_cases_country %>% select(-V2) %>% select(-V1)
View(death_cases_country)

#replacing 30 countries names for matching - second column is what you are changing it to 
death_cases_country$Country[death_cases_country$Country == 'US'] <- 'United States'
death_cases_country$Country[death_cases_country$Country =='Korea, South'] <- 'Korea, Rep.'
death_cases_country$Country[death_cases_country$Country == 'Korea, North'] <- "People's Rep."
death_cases_country$Country[death_cases_country$Country == 'Russia'] <- "Russian Federation"
death_cases_country$Country[death_cases_country$Country == 'Turkey'] <- "Turkiye"
death_cases_country$Country[death_cases_country$Country == 'Iran'] <- "Islamic Rep."
death_cases_country$Country[death_cases_country$Country == 'Czechia'] <- "Czech Republic"
death_cases_country$Country[death_cases_country$Country == 'Slovakia'] <- "Slovak Republic" 
death_cases_country$Country[death_cases_country$Country == 'Burma'] <- "Myanmar"
death_cases_country$Country[death_cases_country$Country ==  'Venezuela'] <-"Venezuela, RB"
death_cases_country$Country[death_cases_country$Country == 'Egypt'] <- "Egypt, Arab Rep."
death_cases_country$Country[death_cases_country$Country == 'Kyrgyzstan'] <- "Kyrgyz Republic"
death_cases_country$Country[death_cases_country$Country == 'Laos'] <- "Lao PDR"

death_cases_country$Country[death_cases_country$Country == 'Congo (Kinshasa)'] <- "Congo, Dem. Rep."
death_cases_country$Country[death_cases_country$Country == 'Syria'] <- "Syrian Arab Republic"
death_cases_country$Country[death_cases_country$Country == 'Bahamas'] <- "Bahamas, The"
death_cases_country$Country[death_cases_country$Country == 'Congo (Brazzaville)'] <- "Congo, Rep."
death_cases_country$Country[death_cases_country$Country == 'Brunei'] <-"Brunei Darussalam"
death_cases_country$Country[death_cases_country$Country == 'Saint Lucia'] <- "St. Lucia"
death_cases_country$Country[death_cases_country$Country == 'Gambia'] <- "Gambia, The"
death_cases_country$Country[death_cases_country$Country == 'Yemen'] <- "Yemen, Rep."
death_cases_country$Country[death_cases_country$Country == 'Saint Vincent and the Grenadines'] <- "St. Vincent and the Grenadines"
death_cases_country$Country[death_cases_country$Country ==  'Saint Kitts and Nevis'] <-"St. Kitts and Nevis"
death_cases_country$Country[death_cases_country$Country == 'Micronesia'] <- "Micronesia, Fed. Sts."

#decide to what to do with taiwan


###########
# GINI VARIABLE
###########

# Making categories (Use it if Seb says we need categories to divide data up)

###Gini index < 0.2 represents perfect income equality, 0.2–0.3 relative equality, 
####0.3–0.4 adequate equality, 0.4–0.5 big income gap, and above 0.5 represents severe income gap.

gini <- read_excel("gini.xls", skip = 3)
View(gini)

na_count <-sapply(gini, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count) # USing 2018 as it has the msot info

#impute horizontally based on previous-years' values 
gini <- gini %>% relocate(`Country Name`, .after = last_col())
gini <- gini %>% relocate(`Country Code`, .after = last_col())
gini <- gini %>% relocate(`Indicator Name`, .after = last_col())
gini <- gini %>% relocate(`Indicator Code`, .after = last_col())

gini <- gini %>% 
  t() %>%   # Transpose 
  na.locf(na.rm=F) %>% # fill columns with previous non NA value
  t() 
gini <- as.data.frame(gini)

gini_clean <- gini %>% select(`Country Name`, `Country Code`, `2021`) %>%
  filter(!is.na(`2021`))



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
# Confirmed cases plots
#############

#creating df with both cases and countries called cases_countries
#creating df with both deaths and countries called deaths_countries
cases_countries <- left_join(confirmed_cases_country,pops2021_2, by=c("Country"="country"))
deaths_countries <- left_join(death_cases_country,pops2021_2, by=c("Country"="country"))

############# GINI #############
df_covid_gini <- left_join(cases_countries,gini_clean,by=c("Country" = "Country Name"))

df_covid_gini <- as.data.frame(df_covid_gini)
df_covid_gini$Number <- as.numeric(df_covid_gini$Number)
df_covid_gini$SP.POP.TOTL <- as.numeric(df_covid_gini$SP.POP.TOTL)

df_covid_gini$cases_per_capita <- as.numeric(df_covid_gini$Number)/as.numeric(df_covid_gini$SP.POP.TOTL)
df_covid_gini$cases_per_capita <- as.numeric(df_covid_gini$cases_per_capita)
df_covid_gini$`2018` <- as.numeric(df_covid_gini$`2018`) #remember to rename to another  metric-name 


plot(x=log(df_covid_gini$`2018`), y=log(df_covid_gini$cases_per_capita), type="plot") 


# Mortality Plots
df_covid_gini_mort <- left_join(deaths_countries,gini_clean,by=c("Country" = "Country Name"))

df_covid_gini_mort <- as.data.frame(df_covid_gini_mort)
df_covid_gini_mort$SP.POP.TOTL <- as.numeric(df_covid_gini_mort$SP.POP.TOTL)
df_covid_gini_mort$Number <- as.numeric(df_covid_gini_mort$Number)

df_covid_gini_mort$deaths_per_capita <- as.numeric(df_covid_gini_mort$Number)/as.numeric(df_covid_gini_mort$SP.POP.TOTL)
df_covid_gini_mort$`2018` <- as.numeric(df_covid_gini_mort$`2018`)
df_covid_gini_mort$deaths_per_capita <- as.numeric(df_covid_gini_mort$deaths_per_capita)

plot(x=log(df_covid_gini_mort$`2018`), y=log(df_covid_gini_mort$deaths_per_capita), type="plot") 

###########
# POPULATION DENSITY VARIABLE
###########

# Making categories (Use it if Seb says we need categories to divide data up)
# Adding population density 
pop.density <- read_excel("density.xls", skip = 3)
View(pop.density)

#impute horizontally based on previous-years' values 
pop.density <- pop.density %>% relocate(`Country Name`, .after = last_col())
pop.density <- pop.density %>% relocate(`Country Code`, .after = last_col())
pop.density <- pop.density %>% relocate(`Indicator Name`, .after = last_col())
pop.density <- pop.density %>% relocate(`Indicator Code`, .after = last_col())

pop.density <- pop.density %>% 
  t() %>%   # Transpose 
  na.locf(na.rm=F) %>% # fill columns with previous non NA value
  t() 
pop.density <- as.data.frame(pop.density)

pop.density_clean <- pop.density %>% select(`Country Name`, `Country Code`, `2021`) %>%
  filter(!is.na(`2021`))

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

#Confirmed cases plots
df_covid_pop_density <- left_join(cases_countries,pop.density_clean,by=c("Country" = "Country Name"))
View(df_covid_pop_density)
View(pop.density_clean)

df_covid_pop_density$SP.POP.TOTL <- as.numeric(df_covid_pop_density$SP.POP.TOTL)
df_covid_pop_density$Number <- as.numeric(df_covid_pop_density$Number)
df_covid_pop_density$cases_per_capita <- as.numeric(df_covid_pop_density$Number)/as.numeric(df_covid_pop_density$SP.POP.TOTL)
df_covid_pop_density$`2021` <- as.numeric(df_covid_pop_density$`2021`)

df_covid_pop_density <- as.data.frame(df_covid_pop_density)


# #summarized_data_pop <- df_covid_pop_density %>% group_by(pop.density_categories) %>% dplyr::summarise(median_number <- median(Number))
# #levels(summarized_data_pop$pop.density_categories)
# #summarized_data_pop$pop.density_categories <- ordered(summarized_data_pop$pop.density_categories, levels = c("Extremely low",
#                                                                                         "Low",

plot(x=log(df_covid_pop_density$`2021`), y=log(df_covid_pop_density$cases_per_capita), type="plot") 


#Mortality plots
df_covid_pop_density.mort <- left_join(deaths_countries,pop.density_clean,by=c("Country" = "Country Name"))
View(df_covid_pop_density.mort)

df_covid_pop_density.mort <- as.data.frame(df_covid_pop_density.mort)
df_covid_pop_density.mort$SP.POP.TOTL <- as.numeric(df_covid_pop_density.mort$SP.POP.TOTL)
df_covid_pop_density.mort$Number <- as.numeric(df_covid_pop_density.mort$Number)
df_covid_pop_density.mort$`2021` <- as.numeric(df_covid_pop_density.mort$`2021`)
df_covid_pop_density.mort$deaths_per_capita <- as.numeric(df_covid_pop_density.mort$Number)/as.numeric(df_covid_pop_density.mort$SP.POP.TOTL)

df_covid_pop_density$cases_per_capita <- as.numeric(df_covid_pop_density$Number)/as.numeric(df_covid_pop_density$SP.POP.TOTL)
df_covid_pop_density$`2021` <- as.numeric(df_covid_pop_density$`2021`)

df_covid_pop_density <- as.data.frame(df_covid_pop_density)


# summarized_data_pop_mort <- df_covid_pop_density.mort %>% 
#   group_by(pop.density_categories) %>% 
#   dplyr::summarise(median_number <- median(Number))
# 
# levels(summarized_data_pop_mort$pop.density_categories)
# summarized_data_pop_mort$pop.density_categories <- ordered(summarized_data_pop_mort$pop.density_categories, levels = c("Extremely low",
#                                                                                                              "Low",
#                                                                                                              "Moderate",
#                                                                                                              "High",
#                                                                                                              "Very high",
#                                                                                                             "NA"))


plot(x=log(df_covid_pop_density.mort$`2021`), y=log(df_covid_pop_density.mort$deaths_per_capita), type="plot") 

####################################
# GDP PER CAP  
####################################

# Confirmed cases 
GDP_cap <- read_excel("GDP.capita.xls", skip = 3) ###UPLOAD THIS FILE
View(GDP_cap)

#impute horizontally based on previous-years' values 
GDP_cap <- GDP_cap %>% relocate(`Country Name`, .after = last_col())
GDP_cap <- GDP_cap %>% relocate(`Country Code`, .after = last_col())
GDP_cap <- GDP_cap %>% relocate(`Indicator Name`, .after = last_col())
GDP_cap <- GDP_cap %>% relocate(`Indicator Code`, .after = last_col())

GDP_cap <- GDP_cap %>% 
  t() %>%   # Transpose 
  na.locf(na.rm=F) %>% # fill columns with previous non NA value
  t() 
GDP_cap <- as.data.frame(GDP_cap)

GDP_cap.clean <- GDP_cap %>% select(`Country Name`, `Country Code`, `2021`) %>%
  filter(!is.na(`2021`))


# Side by side df of GDP and confirmed cases
df_GDP_conf <- left_join(cases_countries,GDP_cap.clean,by=c("Country" = "Country Name"))
View(df_GDP_conf)

df_GDP_conf$SP.POP.TOTL <- as.numeric(df_GDP_conf$SP.POP.TOTL)
df_GDP_conf$cases_per_capita <- as.numeric(df_GDP_conf$Number)/as.numeric(df_GDP_conf$SP.POP.TOTL)
df_GDP_conf$cases_per_capita <- as.numeric(df_GDP_conf$cases_per_capita)
df_GDP_conf$Number <- as.numeric(df_GDP_conf$Number)
df_GDP_conf$`2021` <- as.numeric(df_GDP_conf$`2021`)
df_GDP_conf <- as.data.frame(df_GDP_conf)


plot(x=log(df_GDP_conf$`2021`), y=log(df_GDP_conf$cases_per_capita), type="plot") 



#Mortality plots ###UPLOAD THIS FILE
df_GDP_mort <- left_join(deaths_countries,GDP_cap.clean,by=c("Country" = "Country Name"))
View(df_GDP_mort)
df_GDP_mort$Number <- as.numeric(df_GDP_mort$Number)
df_GDP_mort$`2021` <- as.numeric(df_GDP_mort$`2021`)
df_GDP_mort$deaths_per_capita <- as.numeric(df_GDP_mort$Number)/as.numeric(df_GDP_mort$SP.POP.TOTL)
df_GDP_mort$deaths_per_capita <- as.numeric(df_GDP_mort$deaths_per_capita)
df_GDP_mort <- as.data.frame(df_GDP_mort)



plot(x=log(df_GDP_mort$`2021`), y=log(df_GDP_mort$deaths_per_capita), type="plot") 


####################################
# Healthcare expenditure (% of GDP) 
####################################

# Confirmed cases   ###UPLOAD THIS FILE
Health_exp <- read_excel("Health.exp.xls", skip = 3)

#impute horizontally based on previous-years' values 
Health_exp <- Health_exp %>% relocate(`Country Name`, .after = last_col())
Health_exp <- Health_exp %>% relocate(`Country Code`, .after = last_col())
Health_exp <- Health_exp %>% relocate(`Indicator Name`, .after = last_col())
Health_exp <- Health_exp %>% relocate(`Indicator Code`, .after = last_col())

Health_exp <- Health_exp %>% 
  t() %>%   # Transpose 
  na.locf(na.rm=F) %>% # fill columns with previous non NA value
  t() 
Health_exp <- as.data.frame(Health_exp)

Health_exp <- Health_exp %>% select(`Country Name`, `Country Code`, `2021`) %>%
  filter(!is.na(`2021`))


# Side by side df of GDP and confirmed cases ###UPLOAD THIS FILE
df_health_conf <- left_join(cases_countries,Health_exp,by=c("Country" = "Country Name"))
View(df_health_conf)
df_health_conf <- as.data.frame(df_health_conf)
df_health_conf$Number <- as.numeric(df_health_conf$Number)
df_health_conf$`2021` <- as.numeric(df_health_conf$`2021`)
df_health_conf$cases_per_capita <- as.numeric(df_health_conf$Number)/as.numeric(df_health_conf$SP.POP.TOTL)
df_health_conf$cases_per_capita <- as.numeric(df_health_conf$cases_per_capita)
df_GDP_conf$Number <- as.numeric(df_GDP_conf$Number)
df_GDP_conf$`2021` <- as.numeric(df_GDP_conf$`2021`)
df_GDP_conf <- as.data.frame(df_GDP_conf)

plot(x=log(df_health_conf$`2021`), y=log(df_health_conf$cases_per_capita), type="plot") 

# Mortality plots
df_health_mort <- left_join(deaths_countries,Health_exp,by=c("Country" = "Country Name"))
View(df_health_mort)
df_health_mort <- as.data.frame(df_health_mort)
df_health_mort$Number <- as.numeric(df_health_mort$Number)
df_health_mort$`2021` <- as.numeric(df_health_mort$`2021`)
df_health_mort$deaths_per_capita <- as.numeric(df_health_mort$Number)/as.numeric(df_health_mort$SP.POP.TOTL)
df_health_mort$deaths_per_capita <- as.numeric(df_health_mort$deaths_per_capita)

plot(x=log(df_health_mort$`2021`), y=log(df_health_mort$deaths_per_capita), type="plot") 


##REGION


