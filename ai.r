#Made with RStudio. R vs 3.2

library(jsonlite)
library(httr)


this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

base.url <- "http://www.earthempires.com/api"
user.name <- "username"
api.key <- "apikey"

default.params <- paste("?", "username=", user.name, "&ai_key=", api.key, "&server=ai", sep="")

fixjson <- function(json)
{
  json <- sub("(\\w*):", "\"\\1\":", x=json) #Adds quotes around first word
  json <- paste("{", json, "}", sep="") # Adds {} around json 
  return(json)
}

doPOST <- function(function.url="info", body)
{
  url <- paste(base.url, "/", function.url, default.params, sep="")
  print(url)
  req <- GET(url=url)
  json <- content(req, "text")
  
  json <- fixjson(json)
  
  return(json)
}

doPOST()

getInfo <- function()
{
  
  
}

res <- doPOST()

api_function=server&api_payload='{"username":"salted","ai_key":"49ee125ad5e9a3b81dfb771ac0d3d2fb","server":"ai"}'

req <- curl_fetch_memory("http://www.earthempires.com/api/server?username=salted&ai_key=49ee125ad5e9a3b81dfb771ac0d3d2fb&server=ai")
req <- POST()
parse_headers(req$headers)
cat(rawToChar(req$content))

