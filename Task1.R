#Global Counts 
library(httr)

res <- VERB("GET", url = "https://covid19-stats-api.herokuapp.com/api/v1/cases?")

cat(content(res, 'text'))

totalconfirmed_cases <- content(res)$confirmed
totalconfirmed_deaths <- content(res)$deaths
totalconfirmed_recovered <- content(res)$recovered


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

############################
#Confirmed Deaths by Country
############################
library(httr)
res_deaths <- VERB("GET", url = "https://covid19-stats-api.herokuapp.com/api/v1/cases/country/deaths")
country_list_deaths <- list()
count_list_deaths <- list()

#removing 1 row of list 
unlisted_res_deaths <- unlist(content(res_deaths),recursive=FALSE)

#adding countries to separate list 
for (i in 1:length(unlisted_res_deaths)) {
  if (i %% 2 == 0) {
    country_list_deaths <- c(country_list_deaths,unlisted_res_deaths[i])
  }
}

#adding counts to separate list 
for (i in 1:length(unlisted_res_deaths)) {
  if (i %% 2 != 0) {
    count_list_deaths <- c(count_list_deaths,unlisted_res_deaths[i])
  }
}

confirmed_deaths_country <- do.call(rbind, Map(cbind, count_list_deaths, country_list_deaths))
confirmed_deaths_country <- as.data.frame(confirmed_deaths_country)
confirmed_deaths_country %>% mutate(Country = V2) %>% mutate(Number = V1) %>% select(-V2) %>% select(-V1)




############################
#Confirmed Recovered by Country
############################
library(httr)
res_recovered <- VERB("GET", url = "https://covid19-stats-api.herokuapp.com/api/v1/cases/country/recovered")
country_list_recovered <- list()
count_list_recovered <- list()

#removing 1 row of list 
unlisted_res_recovered <- unlist(content(res_recovered),recursive=FALSE)

#adding countries to separate list 
for (i in 1:length(unlisted_res_recovered)) {
  if (i %% 2 == 0) {
    country_list_recovered <- c(country_list_recovered,unlisted_res_recovered[i])
  }
}

#adding counts to separate list 
for (i in 1:length(unlisted_res_recovered)) {
  if (i %% 2 != 0) {
    count_list_recovered <- c(count_list_recovered,unlisted_res_recovered[i])
  }
}
confirmed_recovered_country <- do.call(rbind, Map(cbind, count_list_recovered, country_list_recovered))
confirmed_recovered_country <- as.data.frame(confirmed_recovered_country)
confirmed_recovered_country %>% mutate(Country = V2) %>% mutate(Number = V1) %>% select(-V2) %>% select(-V1)


