
library(httr)
library(jsonlite)

url <- "https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=biomedical&format=json&pageSize=100"

response <- GET(url)

status_code(response)

raw_text <- content(response, "text", encoding = "UTF-8")
data <- fromJSON(raw_text)

names(data)
str(data, max.level = 2)
