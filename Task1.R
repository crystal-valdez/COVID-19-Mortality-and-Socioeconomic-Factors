#Global Counts 
library(httr)
library(dplyr)
library(tidyverse)

res <- VERB("GET", url = "https://covid19-stats-api.herokuapp.com/api/v1/cases?")

cat(content(res, 'text'))


totalconfirmed_cases <- content(res)$confirmed
totalconfirmed_deaths <- content(res)$deaths
totalconfirmed_recovered <- content(res)$recovered


#Confirmed countries 
library(httr)

res <- VERB("GET", url = "https://covid19-stats-api.herokuapp.com/api/v1/cases/country/confirmed")
content(res)
cat(content(res, 'text'))
n <- content(res)
n <- as.data.frame(n)

n.1 <- n %>% 
  rownames_to_column() %>% 
  gather(variable, value, -rowname) %>% 
  spread(rowname, value)
View(n.1)

n.2 <- split(n.1,cumsum(1:nrow(n.1)%in%202))
View(n.2)
n.2.1 <- n.2$`0`
n.2.2 <- n.2$`1`

n.2.2$Counts <- n.2.1$`1`
names(n.2.2)[2] <- "Country"
df_confirmed <- n.2.2[2:3]

#deaths countries 
library(httr)

res <- VERB("GET", url = "https://covid19-stats-api.herokuapp.com/api/v1/cases/country/deaths")
content(res)
cat(content(res, 'text'))
d <- content(res)
d <- as.data.frame(d)

d.1 <- d %>% 
  rownames_to_column() %>% 
  gather(variable, value, -rowname) %>% 
  spread(rowname, value)
View(d.1)

d.2 <- split(n.1,cumsum(1:nrow(n.1)%in%202))
View(d.2)
d.2.1 <- d.2$`0`
d.2.2 <- d.2$`1`

d.2.2$Counts <- n.2.1$`1`
names(d.2.2)[2] <- "Country"
df_deaths <- d.2.2[2:3]

#recovered countries 

res <- VERB("GET", url = "https://covid19-stats-api.herokuapp.com/api/v1/cases/country/recovered")
content(res)
cat(content(res, 'text'))
r <- content(res)
r <- as.data.frame(r)

r.1 <- r %>% 
  rownames_to_column() %>% 
  gather(variable, value, -rowname) %>% 
  spread(rowname, value)
View(r.1)

r.2 <- split(r.1,cumsum(1:nrow(r.1)%in%202))
View(r.2)
r.2.1 <- r.2$`0`
r.2.2 <- r.2$`1`

r.2.2$Counts <- r.2.1$`1`
names(r.2.2)[2] <- "Country"
df_recovered <- r.2.2[2:3]


