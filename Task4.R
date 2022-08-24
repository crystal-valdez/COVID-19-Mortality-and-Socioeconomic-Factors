#Task 3 – Combine data to calculate the ratio of all types of COVID-19 cases 
#(confirmed, recovered, death/mortality-rate) to the population 
#(mortality rate = # of deaths in population per unit of time) 

#Importing necessary libraries
library("wbstats")
library(httr)
library(stringr)
library(tidyr)
library(jsonlite)
library(readxl)
library(dplyr)
install.packages("plyr")
library(plyr)
library(zoo)


#Loading in world-bank data library and labelling as pops2021 object
pops2021 <- as.data.frame(wb_data("SP.POP.TOTL", country = "all", start_date = 2021, end_date = 2021))

#Filtering out or removing values with digits in iso2c column, along with NA values
pops2021_2 <- filter(pops2021, !grepl("[0-9]", iso2c)) 
pops2021_2 <- pops2021_2[!is.na(pops2021_2$iso2c),]
pops2021_2 <- pops2021_2 %>% drop_na(iso2c)

#Removing columns from country column in pops2021_2 that have specific key words as follows 
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

#Taking sum of population for individual countries to retrieve global world population, set to world_pop object
world_pop <- sum(as.numeric(pops2021$SP.POP.TOTL), na.rm = T)

#Copying code from Task 1, afterwards import to RDS object to obtain confirmed, deaths and recovered 

#Obtaining cases from covid-world-data API and setting to res object
res <- VERB("GET", url = "https://covid19-stats-api.herokuapp.com/api/v1/cases?")

#cat(content(res, 'text'))

#creating objects for global confirmed cases, deaths and recovered ccases
totalconfirmed_cases <- content(res)$confirmed
totalconfirmed_deaths <- content(res)$deaths
totalconfirmed_recovered <- content(res)$recovered

#retrieving ratios for total confirmed cases, deaths and recovered 
ratio_confirmed_cases <- world_pop/totalconfirmed_cases
ratio_deaths <- world_pop/totalconfirmed_deaths
ratio_recovered <- world_pop/totalconfirmed_recovered

#### Task 4 ######


### FIGURING OUT WHATS THE BEST VARIABLE FOR CONFIRMED CASES AND MORTALITY

############################
#Country-Level Confirmed Cases
############################
#retrieving total amount of confirmed_cases in confirmed_cases_res object
confirmed_cases_res <- VERB("GET", url = "https://covid19-stats-api.herokuapp.com/api/v1/cases/country/confirmed")

#creating empty list for country and count
country_list <- list()
count_list <- list()

#Removing 1 row of list using unlist function and assigning to unlisted_res object
unlisted_res <- unlist(content(confirmed_cases_res),recursive=FALSE)

#Adding countries to country list by taking every 2nd entry from unlisted_res to country_list object
for (i in 1:length(unlisted_res)) {
  if (i %% 2 == 0) {
    country_list <- c(country_list,unlisted_res[i])
  }
}
#Adding counts for cases in count_list object
for (i in 1:length(unlisted_res)) {
  if (i %% 2 != 0) {
    count_list <- c(count_list,unlisted_res[i])
  }
}

#Merging both country and case count-information in the same dataframe 
confirmed_cases_country <- do.call(rbind, Map(cbind, count_list, country_list))
confirmed_cases_country <- as.data.frame(confirmed_cases_country)

#Changing name of column of V2 to Country, and V1 to Number, and removing the original named columns 
confirmed_cases_country$Country <- confirmed_cases_country$V2
confirmed_cases_country$Number <- confirmed_cases_country$V1
confirmed_cases_country <- confirmed_cases_country %>% select(-V2) %>% select(-V1)

#Due to dissimilarity in country-names between world-covid API and global country API, 
#30 countries names' were changed 
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

#Retrieving total amount of confirmed_cases in res_deaths object
res_deaths <- VERB("GET", url = "https://covid19-stats-api.herokuapp.com/api/v1/cases/country/deaths")

#creating empty list for country and count
country_list_deaths_new <- list()
count_list_deaths_new <- list()

#Removing 1 row of list using unlist function and assigning to unlisted_res_deaths object
unlisted_res_deaths <- unlist(content(res_deaths),recursive=FALSE)

#Adding countries to country_list_deaths_new list by taking every 2nd entry from unlisted_res_deaths to country_list_deaths_new object
for (i in 1:length(unlisted_res_deaths)) {
  if (i %% 2 == 0) {
    country_list_deaths_new <- c(country_list_deaths_new,unlisted_res_deaths[i])
  }
}

#Adding counts for deaths in count_list_deaths_new object
for (i in 1:length(unlisted_res)) {
  if (i %% 2 != 0) {
    count_list_deaths_new <- c(count_list_deaths_new,unlisted_res[i])
  }
}

#Merging both country and case count-information in the same death_cases_country dataframe 
death_cases_country <- do.call(rbind, Map(cbind, count_list_deaths_new, country_list_deaths_new))
death_cases_country <- as.data.frame(death_cases_country)

#Changing name of column of V2 to Country, and V1 to Number, and removing the original named columns 
death_cases_country$Country <- death_cases_country$V2
death_cases_country$Number <- death_cases_country$V1
death_cases_country <- death_cases_country %>% select(-V2) %>% select(-V1)


#Due to dissimilarity in country-names between world-covid API and global country API, 
#30 countries names' were changed 
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

#############
# Confirmed and death cases plots
#############

#Creating df with both cases and countries called cases_countries
#Creating df with both deaths and countries called deaths_countries
cases_countries <- left_join(confirmed_cases_country,pops2021_2, by=c("Country"="country"))
deaths_countries <- left_join(death_cases_country,pops2021_2, by=c("Country"="country"))

#############
# Creating categories for income (Low, Medium, High) and merging with Covid-data 
#############

# Attaching data with income and region info of countries
income_region <- read.csv("income_world_data_country.csv")

# Selecting Country, Income.group, Region information for income_Region df 
income_region <- income_region %>%
  select(Country, Income.group, Region)

# Attaching income and region info to confirmed cases data through left-join, assigning to case_countries2 object
case_countries2 <- left_join(income_region, cases_countries, by = "Country")

# Establishing factors - Income level and region levels
case_countries2$Income.group <- as.factor(case_countries2$Income.group)
case_countries2$Income.group <- ordered(case_countries2$Income.group, levels = c("Low income", 
            "Lower middle income", 
            "Upper middle income",
            "High income",
            ""))
case_countries2$Region <- as.factor(case_countries2$Region)

# Attaching income and region info to death cases data
death_countries2 <- left_join(income_region, deaths_countries, by = "Country")

#Converting to factors - establishing income level and region levels 
death_countries2$Income.group <- as.factor(case_countries2$Income.group)
death_countries2$Income.group <- ordered(death_countries2$Income.group, levels = c("Low income", 
                                                                                 "Lower middle income", 
                                                                                 "Upper middle income",
                                                                                 "High income",
                                                                                 ""))                                         
death_countries2$Region <- as.factor(death_countries2$Region)


#Income level Boxplots 
#############

# Creating boxplot for number of cases abnd income, setting outliers as exceeding 3500000 cases 
case_countries2$Number <- as.numeric(case_countries2$Number)
case_countries2$cases_per_capita <- as.numeric(case_countries2$Number)/as.numeric(case_countries2$SP.POP.TOTL)
boxplot(case_countries2$Number ~ case_countries2$Income.group, ylim=c(0,3500000))

# Creating boxplot for number of cases per capita and income 
boxplot(case_countries2$cases_per_capita ~ case_countries2$Income.group)

# Creating boxplot for number of deaths and income  
death_countries2$Number <- as.numeric(death_countries2$Number)
death_countries2$cases_per_capita <- as.numeric(death_countries2$Number)/as.numeric(death_countries2$SP.POP.TOTL)
boxplot(death_countries2$Number ~ death_countries2$Income.group, ylim=c(0,55000))

# Creating boxplot for number of deaths per capita and income  
boxplot(death_countries2$cases_per_capita ~ death_countries2$Income.group)

# Performing ANOVA and Tukey-Test for income level 
one.way_income <- aov(death_countries2$cases_per_capita ~ death_countries2$Income.group, data = death_countries2)
summary(one.way_income)
tukey.test_income <- TukeyHSD(one.way_income)
tukey.test_income

#Region level Boxplots 
#############

# Creating boxplot for number of cases based on regions and setting outlier limits
case_countries2$Number <- as.numeric(case_countries2$Number)
case_countries2$cases_per_capita <- as.numeric(case_countries2$Number)/as.numeric(case_countries2$SP.POP.TOTL)
boxplot(case_countries2$Number ~ case_countries2$Region, ylim=c(0,5000000))

# Creating boxplot for number of cases per capita based on regions 
boxplot(case_countries2$cases_per_capita ~ case_countries2$Region)

# Creating boxplot for number of deaths based on regions 
death_countries2$Number <- as.numeric(death_countries2$Number)
death_countries2$cases_per_capita <- as.numeric(death_countries2$Number)/as.numeric(death_countries2$SP.POP.TOTL)

# Creating boxplot for number of deaths based on regions and setting outlier-limits
boxplot(death_countries2$Number ~ death_countries2$Region, ylim=c(0,80000))

# Creating boxplot for number of deaths per capita based on regions and setting outlier-limits
boxplot(death_countries2$cases_per_capita ~ case_countries2$Region)

#Gini level (indicator of inequality) Boxplots 
#############

#reading in data providing gini-values for each country
gini <- read_excel("gini.xls", skip = 3)

# Counting number of NA's to find year with least-amount of NA's to serve as representative score
# Using 2018 as this year has the greatest amount of non-NA GINI-values  
na_count <-sapply(gini, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count) 

#Moving Country Name, Country Code, Indicator Name and Indicator columns to be the last-columns 
gini <- gini %>%
  relocate(`Country Name`, .after = last_col()) %>%
  relocate(`Country Code`, .after = last_col()) %>%
  relocate(`Indicator Name`, .after = last_col()) %>%
  relocate(`Indicator Code`, .after = last_col())

#Imputing to fill in 2018 year information for countries with NA's horizontally based on previous-years' values 
gini <- gini %>% 
  # Transposing columns
  t() %>%  
  # Filling in columns with previous non-NA values
  na.locf(na.rm=F) %>% 
  # Transposing columns back 
  t() 
gini <- as.data.frame(gini)

#Filtering out columns except for Country Name, Country Code and 2018  
gini_clean <- gini %>% select(`Country Name`, `Country Code`, `2018`) %>%
  filter(!is.na(`2018`))

#Constructed Gini index categories 
#< 0.2 represents perfect income equality
#0.2–0.3 relative equality, 
#0.3–0.4 adequate equality
#0.4–0.5 big income gap
#above 0.5 represents severe income gap.

# Creating column called gini_equaltiy which encodes the categories for gini-values 
gini_categories <- gini_clean %>%
  mutate(gini_equality = case_when(
    `2018` < 20 ~ "Perfect",
    `2018` <= 30 ~ "Relative",
    `2018` <= 40 ~ "Adequate",
    `2018` <= 50 ~ "Big gap",
    `2018` > 50 ~ "Severe gap"
  ))

# Attaching data on gini-levels associated with countries to confirmed cases dataset, called df_covid_gini object
# Setting df_covid_gini as dataframe and setting type of relevant column-categories 
df_covid_gini <- left_join(case_countries2,gini_categories,by=c("Country" = "Country Name"))
df_covid_gini <- as.data.frame(df_covid_gini)
df_covid_gini$Number <- as.numeric(df_covid_gini$Number)
df_covid_gini$SP.POP.TOTL <- as.numeric(df_covid_gini$SP.POP.TOTL)
df_covid_gini$gini_equality <- as.factor(df_covid_gini$gini_equality)
df_covid_gini$cases_per_capita <- as.numeric(df_covid_gini$Number)/as.numeric(df_covid_gini$SP.POP.TOTL)
df_covid_gini$cases_per_capita <- as.numeric(df_covid_gini$cases_per_capita)
df_covid_gini$`2018` <- as.numeric(df_covid_gini$`2018`) #remember to rename to another  metric-name 

#Ordering categories in df_covid_gini from low to high-gaps 
df_covid_gini$gini_equality <- ordered(df_covid_gini$gini_equality, levels = c("Relative", 
                                                                                   "Adequate", 
                                                                                   "Big gap",
                                                                                   "Severe gap"))                                         


# Attaching data on gini-levels associated with countries to confirmed cases dataset, called df_covid_gini_mort object
# Setting df_covid_gini as dataframe and setting type of relevant column-categories 
df_covid_gini_mort <- left_join(death_countries2,gini_categories,by=c("Country" = "Country Name"))
df_covid_gini_mort <- as.data.frame(df_covid_gini_mort)
df_covid_gini_mort$SP.POP.TOTL <- as.numeric(df_covid_gini_mort$SP.POP.TOTL)
df_covid_gini_mort$Number <- as.numeric(df_covid_gini_mort$Number)
df_covid_gini_mort$gini_equality <- as.factor(df_covid_gini_mort$gini_equality)
df_covid_gini_mort$deaths_per_capita <- as.numeric(df_covid_gini_mort$Number)/as.numeric(df_covid_gini_mort$SP.POP.TOTL)
df_covid_gini_mort$`2018` <- as.numeric(df_covid_gini_mort$`2018`)
df_covid_gini_mort$deaths_per_capita <- as.numeric(df_covid_gini_mort$deaths_per_capita)

#Ordering categories in df_covid_gini from low to high-gaps 
df_covid_gini_mort$gini_equality <- ordered(df_covid_gini_mort$gini_equality, levels = c("Relative", 
                                                                               "Adequate", 
                                                                               "Big gap",
                                                                               "Severe gap"))                                         


# Creating boxplot for number of cases based on gini-level and setting outlier limits
boxplot(df_covid_gini$Number ~ df_covid_gini$gini_equality, ylim=c(0,15000000))

# Creating boxplot for number of cases per capita based on gini-level and setting outlier limits
boxplot(df_covid_gini$cases_per_capita ~ df_covid_gini$gini_equality)

# Creating boxplot for number of deaths on gini-level and setting outlier limits
boxplot(df_covid_gini_mort$Number ~ df_covid_gini_mort$gini_equality, ylim=c(0,1000000))

# Creating boxplot for number of deaths per capita on gini-level and setting outlier limits
boxplot(df_covid_gini_mort$cases_per_capita ~ df_covid_gini_mort$gini_equality)

# Performing ANOVA and Kruskal-Test for gini-level information level
one.way <- aov(df_covid_gini_mort$deaths_per_capita ~ df_covid_gini_mort$gini_equality, data = df_covid_gini_mort)
summary(one.way)
kruskal.test(df_covid_gini_mort$deaths_per_capita ~ df_covid_gini_mort$gini_equality, data = df_covid_gini_mort)


#Population Density Boxplots 
#############

# Reading in data providing population density-values for each country
pop.density <- read_excel("density.xls", skip = 3)

#Moving Country Name, Country Code, Indicator Name and Indicator columns to be the last-columns 
pop.density <- pop.density %>%
  relocate(`Country Name`, .after = last_col()) %>%
  relocate(`Country Code`, .after = last_col()) %>% 
  relocate(`Indicator Name`, .after = last_col()) %>%
  relocate(`Indicator Code`, .after = last_col())

pop.density <- pop.density %>% 
  # Transpose columns 
  t() %>%   
  # Fill columns with previous non NA value
  na.locf(na.rm=F) %>% 
  # Transpose columns back 
  t() 
pop.density <- as.data.frame(pop.density)

#Filtering out columns except for Country Name, Country Code and 2018  
pop.density_clean <- pop.density %>% select(`Country Name`, `Country Code`, `2021`) %>%
  filter(!is.na(`2021`))

# Creating column called pop.density_categories which encodes the categories for population density-values 
pop.density_categories <- pop.density_clean %>%
  mutate(pop.density_categories = case_when(
    `2021` <= 100 ~ "Extremely low",
    `2021` <= 250 ~ "Low",
    `2021` <= 500 ~ "Moderate",
    `2021` > 500 ~ "High",
  ))

# Joining pop.density dataset to confirmed cases datasets, called the df_covid_pop_density object
# Setting df_covid_gini as dataframe and setting type of relevant column-categories 
df_covid_pop_density <- left_join(cases_countries,pop.density_categories,by=c("Country" = "Country Name"))
df_covid_pop_density$SP.POP.TOTL <- as.numeric(df_covid_pop_density$SP.POP.TOTL)
df_covid_pop_density$Number <- as.numeric(df_covid_pop_density$Number)
df_covid_pop_density$cases_per_capita <- as.numeric(df_covid_pop_density$Number)/as.numeric(df_covid_pop_density$SP.POP.TOTL)
df_covid_pop_density$`2021` <- as.numeric(df_covid_pop_density$`2021`)
df_covid_pop_density$pop.density_categories <- as.factor(df_covid_pop_density$pop.density_categories)
df_covid_pop_density <- as.data.frame(df_covid_pop_density)

#Ordering categories in df_covid_pop_density from low to high pop density
df_covid_pop_density$pop.density_categories <- ordered(df_covid_pop_density$pop.density_categories, levels = c("Extremely low", 
                                                                                         "Low", 
                                                                                         "Moderate",
                                                                                         "High"))                                                                                  


# Joining pop.density dataset to deaths datasets, called the df_covid_pop_density.mort object
# Setting df_covid_pop_density.mort as dataframe and setting type of relevant column-categories 
df_covid_pop_density.mort <- left_join(deaths_countries,pop.density_categories,by=c("Country" = "Country Name"))
df_covid_pop_density.mort <- as.data.frame(df_covid_pop_density.mort)
df_covid_pop_density.mort$SP.POP.TOTL <- as.numeric(df_covid_pop_density.mort$SP.POP.TOTL)
df_covid_pop_density.mort$Number <- as.numeric(df_covid_pop_density.mort$Number)
df_covid_pop_density.mort$`2021` <- as.numeric(df_covid_pop_density.mort$`2021`)
df_covid_pop_density.mort$deaths_per_capita <- as.numeric(df_covid_pop_density.mort$Number)/as.numeric(df_covid_pop_density.mort$SP.POP.TOTL)
df_covid_pop_density.mort$`2021` <- as.numeric(df_covid_pop_density.mort$`2021`)
df_covid_pop_density.mort$pop.density_categories <- as.factor(df_covid_pop_density.mort$pop.density_categories)
df_covid_pop_density.mort <- as.data.frame(df_covid_pop_density.mort)

#Ordering categories in df_covid_pop_density.mort from low to high pop density
df_covid_pop_density.mort$pop.density_categories <- ordered(df_covid_pop_density$pop.density_categories, levels = c("Extremely low", 
                                                                                             "Low", 
                                                                                             "Moderate",
                                                                                             "High"))

# Creating boxplot for number of cases based on pop.density_categories, and setting outlier limits 
boxplot(df_covid_pop_density$Number ~ df_covid_pop_density$pop.density_categories, ylim=c(0,5000000))

# Creating boxplot for number of cases per capita based on pop.density_categories, and setting outlier limits 
boxplot(df_covid_pop_density$cases_per_capita ~ df_covid_pop_density$pop.density_categories)


# Creating boxplot for number of deaths based on pop.density_categories, and setting outlier limits 
boxplot(df_covid_pop_density.mort$Number ~ df_covid_pop_density.mort$pop.density_categories, ylim=c(0,50000))

# Creating boxplot for number of deaths per capita based on pop.density_categories, and setting outlier limits 
boxplot(df_covid_pop_density.mort$deaths_per_capita ~ df_covid_pop_density.mort$pop.density_categories)

# Performing ANOVA and Kruskal-Test for population denisty-level information level
one.way_pop_density_mort <- aov(df_covid_pop_density.mort$deaths_per_capita ~ df_covid_pop_density.mort$pop.density_categories, data = df_covid_pop_density.mort)
summary(one.way_pop_density_mort)
kruskal.test(df_covid_pop_density.mort$deaths_per_capita ~ df_covid_pop_density.mort$pop.density_categories, data = df_covid_pop_density.mort)


# 
# ####################################
# # GDP PER CAP  - not using and not updated 
# ####################################
# 
# # Confirmed cases 
# GDP_cap <- read_excel("GDP.capita.xls", skip = 3) ###UPLOAD THIS FILE
# 
# 
# #impute horizontally based on previous-years' values 
# GDP_cap <- GDP_cap %>% relocate(`Country Name`, .after = last_col())
# GDP_cap <- GDP_cap %>% relocate(`Country Code`, .after = last_col())
# GDP_cap <- GDP_cap %>% relocate(`Indicator Name`, .after = last_col())
# GDP_cap <- GDP_cap %>% relocate(`Indicator Code`, .after = last_col())
# 
# GDP_cap <- GDP_cap %>% 
#   t() %>%   # Transpose 
#   na.locf(na.rm=F) %>% # fill columns with previous non NA value
#   t() 
# GDP_cap <- as.data.frame(GDP_cap)
# 
# GDP_cap.clean <- GDP_cap %>% select(`Country Name`, `Country Code`, `2021`) %>%
#   filter(!is.na(`2021`))
# 
# 
# # Side by side df of GDP and confirmed cases
# df_GDP_conf <- left_join(cases_countries,GDP_cap.clean,by=c("Country" = "Country Name"))
# 
# 
# df_GDP_conf$SP.POP.TOTL <- as.numeric(df_GDP_conf$SP.POP.TOTL)
# df_GDP_conf$cases_per_capita <- as.numeric(df_GDP_conf$Number)/as.numeric(df_GDP_conf$SP.POP.TOTL)
# df_GDP_conf$cases_per_capita <- as.numeric(df_GDP_conf$cases_per_capita)
# df_GDP_conf$Number <- as.numeric(df_GDP_conf$Number)
# df_GDP_conf$`2021` <- as.numeric(df_GDP_conf$`2021`)
# df_GDP_conf <- as.data.frame(df_GDP_conf)
# 
# 
# plot(x=log(df_GDP_conf$`2021`), y=log(df_GDP_conf$cases_per_capita), type="plot") 
# 
# 
# 
# #Mortality plots ###UPLOAD THIS FILE
# df_GDP_mort <- left_join(deaths_countries,GDP_cap.clean,by=c("Country" = "Country Name"))
# 
# df_GDP_mort$Number <- as.numeric(df_GDP_mort$Number)
# df_GDP_mort$`2021` <- as.numeric(df_GDP_mort$`2021`)
# df_GDP_mort$deaths_per_capita <- as.numeric(df_GDP_mort$Number)/as.numeric(df_GDP_mort$SP.POP.TOTL)
# df_GDP_mort$deaths_per_capita <- as.numeric(df_GDP_mort$deaths_per_capita)
# df_GDP_mort <- as.data.frame(df_GDP_mort)
# 
# 
# 
# plot(x=log(df_GDP_mort$`2021`), y=log(df_GDP_mort$deaths_per_capita), type="plot") 
# 



# Healthcare expenditure (% of GDP) 
####################################

# Reading in data connecting healthcare expenditure values to countries 
Health_exp <- read_excel("Health.exp.xls", skip = 3)

#Moving Country Name, Country Code, Indicator Name and Indicator columns to be the last-columns 
Health_exp <- Health_exp %>%
  relocate(`Country Name`, .after = last_col())  %>%
  relocate(`Country Code`, .after = last_col()) %>%
  relocate(`Indicator Name`, .after = last_col()) %>%
  relocate(`Indicator Code`, .after = last_col())

Health_exp <- Health_exp %>% 
  # Transposing columns 
  t() %>%   
  # Filling columns with previous non NA value
  na.locf(na.rm=F) %>% 
  # Transposing columns back 
  t() 
Health_exp <- as.data.frame(Health_exp)

#Filtering out columns except for Country Name, Country Code and 2018  
Health_exp <- Health_exp %>% select(`Country Name`, `Country Code`, `2021`) %>%
  filter(!is.na(`2021`))

# Joining health expenditure dataset to confirmed cases datasets, called the df_health_conf object
# Setting df_health_conf as dataframe and setting type of relevant column-categories 
df_health_conf <- left_join(cases_countries,Health_exp,by=c("Country" = "Country Name"))
df_health_conf <- as.data.frame(df_health_conf)
df_health_conf$Number <- as.numeric(df_health_conf$Number)
df_health_conf$`2021` <- as.numeric(df_health_conf$`2021`)
df_health_conf$cases_per_capita <- as.numeric(df_health_conf$Number)/as.numeric(df_health_conf$SP.POP.TOTL)
df_health_conf$cases_per_capita <- as.numeric(df_health_conf$cases_per_capita)
df_health_conf$Number <- as.numeric(df_health_conf$Number)
df_health_conf$`2021` <- as.numeric(df_health_conf$`2021`)
df_health_conf <- as.data.frame(df_health_conf)


#Creating quartiles with healthcare expenditure, to construct categoriesbased on Year 2021 data 
quantile_cases = df_health_conf$`2021`    
quantile(quantile_cases, na.rm=T)

# Creating column called df_health_conf_categories which encodes the categories for healthcare expenditure values 
health_categories <- df_health_conf %>%
  mutate(df_health_conf_categories = case_when(
    `2021` <= 1.525117 ~ "Extremely low",
    `2021` <= 4.346481 ~ "Low",
    `2021` <= 6.073806 ~ "Moderate",
    `2021` <= 8.027737 ~ "High",
    `2021` > 8.027737 ~ "Very high"
  ))
#Ordering categories in health_categories dataset from low to high health expenditure
health_categories$df_health_conf_categories <- ordered(health_categories$df_health_conf_categories, levels = c("Extremely low", 
                                                                                                               "Low", 
                                                                                                               "Moderate",
                                                                                                               "High",
                                                                                                               "Very high"))   

# Joining health expenditure dataset to deaths datasets, called the df_health_mort object
# Setting df_health_conf as dataframe and setting type of relevant column-categories 
df_health_mort <- left_join(deaths_countries,Health_exp,by=c("Country" = "Country Name"))
df_health_mort <- as.data.frame(df_health_mort)
df_health_mort$Number <- as.numeric(df_health_mort$Number)
df_health_mort$`2021` <- as.numeric(df_health_mort$`2021`)
df_health_mort$deaths_per_capita <- as.numeric(df_health_mort$Number)/as.numeric(df_health_mort$SP.POP.TOTL)
df_health_mort$deaths_per_capita <- as.numeric(df_health_mort$deaths_per_capita)

#Creating quartiles for health expenditure based on Year 2021 data, used for creating categories 
quantile_cases_morts_health = df_health_mort$`2021`    
quantile(quantile_cases_morts_health, na.rm=T)

# Creating column called df_health_mort which encodes the categories for healthcare expenditure values 
health_categories_death <- df_health_mort %>%
  mutate(df_health_conf_categories = case_when(
    `2021` <= 1.525117 ~ "Extremely low",
    `2021` <= 4.346481 ~ "Low",
    `2021` <= 6.073806 ~ "Moderate",
    `2021` <= 8.027737 ~ "High",
    `2021` > 8.027737 ~ "Very high"))
#Ordering categories in health_categories_death dataset from low to high health expenditure
health_categories_death$df_health_conf_categories <- ordered(health_categories_death$df_health_conf_categories, levels = c("Extremely low", 
                                                                                                                           "Low", 
                                                                                                                           "Moderate",
                                                                                                                           "High",
                                                                                                                           "Very high"))  
health_categories_death$df_health_conf_categories <- as.factor(health_categories_death$df_health_conf_categories)

# Creating boxplot for number of cases based on pop.density_categories, and setting outlier limits 
boxplot(health_categories$Number ~ health_categories$df_health_conf_categories)

# Creating boxplot for number of cases per capita based on health_categories
plot(x=health_categories$df_health_conf_categories, y=health_categories$cases_per_capita, type="plot") 
# Saving this file as RDS object, will be used in future dashboards 
saveRDS(health_categories, file = "df_health_expenditure_cases.RDS")

# Creating boxplot for number of deaths based on health expenditure categories
boxplot(health_categories$Number ~ health_categories$df_health_conf_categories)

# Creating boxplot for number of deaths per capita based on health expenditure categories
plot(x=health_categories_death$df_health_conf_categories, y=health_categories_death$deaths_per_capita, type="plot") 
# Saving this file as RDS object, will be used in future dashboards 
saveRDS(health_categories_death, file = "df_health_expenditure_deaths.RDS")

#Doing ANOVA and Post-Hoc Tukey Test
one.way_health <- aov(health_categories_death$deaths_per_capita ~ health_categories_death$df_health_conf_categories, data = health_categories_death)
summary(one.way_health)
#3 categories are shown to be significant 
tukey.test_healthexp <- TukeyHSD(one.way_health)
tukey.test_healthexp


# Literacy (education for female above 15 yrs)  
################################################

# Reading in data covering data on unemployed females in each countries
lit <- read_excel("female_unemployed.xls", 
                  skip = 3)

#Moving Country Name, Country Code, Indicator Name and Indicator columns to be the last-columns 
lit <- lit %>% relocate(`Country Name`, .after = last_col()) %>%
  relocate(`Country Code`, .after = last_col()) %>%
  relocate(`Indicator Name`, .after = last_col()) %>%
  relocate(`Indicator Code`, .after = last_col())

#impute horizontally based on previous-years' values 
lit <- lit %>%
  # Transposing columns
  t() %>%   
  #Filling columns with previous non NA value
  na.locf(na.rm=F) %>%
  # Transposing columns back 
  t() 
lit <- as.data.frame(lit)

#Filtering out columns except for Country Name, Country Code and 2021  
lit <- lit %>% select(`Country Name`, `Country Code`, `2021`) %>%
  filter(!is.na(`2021`))

# Joining literacy education dataset to cases datasets, called  df_lit_conf object
# Setting df_lit_conf as dataframe and setting type of relevant column-categories 
df_lit_conf <- left_join(cases_countries,lit,by=c("Country" = "Country Name"))
df_lit_conf <- as.data.frame(df_lit_conf)
df_lit_conf$Number <- as.numeric(df_lit_conf$Number)
df_lit_conf$`2021` <- as.numeric(df_lit_conf$`2021`)
df_lit_conf$cases_per_capita <- as.numeric(df_lit_conf$Number)/as.numeric(df_lit_conf$SP.POP.TOTL)
df_lit_conf$cases_per_capita <- as.numeric(df_lit_conf$cases_per_capita)
df_lit_conf$Number <- as.numeric(df_lit_conf$Number)
df_lit_conf$`2021` <- as.numeric(df_lit_conf$`2021`)
df_lit_conf <- as.data.frame(df_lit_conf)

#Creating quantiles for df_lit_conf's literacy information for Year 2021 
quantile_literacy = df_lit_conf$`2021`    
quantile(quantile_literacy, na.rm=T)

#Creating new column for categories corresponding to literacy data called literacy_categories_new
literacy_categories_df <- df_lit_conf %>%
  mutate(literacy_categories_new = case_when(
    `2021` <= 0.5200 ~ "Extremely low",
    `2021` <= 3.8950 ~ "Low",
    `2021` <= 6.5400 ~ "Moderate",
    `2021` <= 11.8775 ~ "High",
    `2021` > 11.8775 ~ "Very high"))
literacy_categories_df$literacy_categories_new <- ordered(literacy_categories_df$literacy_categories_new, levels = c("Extremely low", 
                                                                                                                     "Low", 
                                                                                                                     "Moderate",
                                                                                                                     "High",
                                                                                                                     "Very high"))   
literacy_categories_df$literacy_categories_new <- as.factor(literacy_categories_df$literacy_categories_new)


# Joining literacy education dataset to deaths datasets, called  df_lit_conf_deaths object
# Setting df_lit_conf_deaths as dataframe and setting type of relevant column-categories 
df_lit_conf_deaths <- left_join(deaths_countries,lit,by=c("Country" = "Country Name"))
df_lit_conf_deaths <- as.data.frame(df_lit_conf_deaths)
df_lit_conf_deaths$Number <- as.numeric(df_lit_conf_deaths$Number)
df_lit_conf_deaths$`2021` <- as.numeric(df_lit_conf_deaths$`2021`)
df_lit_conf_deaths$deaths_per_capita <- as.numeric(df_lit_conf_deaths$Number)/as.numeric(df_lit_conf_deaths$SP.POP.TOTL)
df_lit_conf_deaths$deaths_per_capita <- as.numeric(df_lit_conf_deaths$deaths_per_capita)
df_lit_conf_deaths$Number <- as.numeric(df_lit_conf_deaths$Number)
df_lit_conf_deaths$`2021` <- as.numeric(df_lit_conf_deaths$`2021`)
df_lit_conf_deaths <- as.data.frame(df_lit_conf_deaths)

#Creating quantiles for quantile_literacy_deaths for Year 2021 
quantile_literacy_deaths = df_lit_conf_deaths$`2021`    
quantile(quantile_literacy_deaths, na.rm=T)

#Creating new column for categories corresponding to literacy data called literacy_categories_new_deaths
literacy_categories_df_deaths <- df_lit_conf_deaths %>%
  mutate(literacy_categories_new_deaths = case_when(
    `2021` <= 0.5200 ~ "Extremely low",
    `2021` <= 3.8950 ~ "Low",
    `2021` <= 6.5400 ~ "Moderate",
    `2021` <= 11.8775 ~ "High",
    `2021` > 11.8775 ~ "Very high"))
literacy_categories_df_deaths$literacy_categories_new_deaths <- ordered(literacy_categories_df_deaths$literacy_categories_new_deaths, levels = c("Extremely low", 
                                                                                                                                                 "Low", 
                                                                                                                                                 "Moderate",
                                                                                                                                                 "High",
                                                                                                                                                 "Very high"))
literacy_categories_df_deaths$literacy_categories_new_deaths <- as.factor(literacy_categories_df_deaths$literacy_categories_new_deaths)




# Creating boxplot for number of cases based on literacy education
plot(x=literacy_categories_df$literacy_categories_new, y=literacy_categories_df$Number, type="plot") 

# Creating boxplot for number of cases per capita based on literacy education
plot(x=literacy_categories_df$literacy_categories_new, y=literacy_categories_df$cases_per_capita, type="plot") 

# Creating boxplot for number of deaths per capita based on literacy education
plot(x=literacy_categories_df_deaths$literacy_categories_new_deaths, y=literacy_categories_df_deaths$Number, type="plot") 

# Creating boxplot for number of deaths per capita based on literacy education
plot(x=literacy_categories_df_deaths$literacy_categories_new_deaths, y=literacy_categories_df_deaths$deaths_per_capita, type="plot") 


##Doing ANOVA and Post-Hoc Tukey Test
one.way_literacy <- aov(literacy_categories_df_deaths$deaths_per_capita ~ literacy_categories_df_deaths$literacy_categories_new_deaths, data = literacy_categories_df_deaths)
summary(one.way_literacy)




