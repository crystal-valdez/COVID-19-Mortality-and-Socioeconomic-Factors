library(httr)

res <- VERB("GET", url = "https://covid19-stats-api.herokuapp.com/api/v1/cases?")

cat(content(res, 'text'))


