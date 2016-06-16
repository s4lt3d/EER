#Made with RStudio. R vs 3.2
library(jsonlite)
library(httr)
library(randomNames)

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

base.url <- "http://www.earthempires.com/api"
user.name <- "salted"
api.key <- ""

#default.params <- paste("?", "username=", user.name, "&ai_key=", api.key, "&server=ai", sep="")
default.params <- list()
default.params$username <- user.name
default.params$ai_key <- api.key
default.params$server <- "ai"

fixjson <- function(json)
{
  json <- sub("(\\w*):", "\"\\1\":", x=json) #Adds quotes around first word
  json <- paste("{", json, "}", sep="") # Adds {} around json 
  return(json)
}

doPOST <- function(params)
{
  
  
  url <- paste(base.url, "?api_payload=", p, sep="")
  print(url)
  
  p <- toJSON(params, auto_unbox=TRUE)
  
  req <- POST(url=url)
  json <- content(req, "text")
  print(json)
  json <- fixjson(json)
  return(json)
}

getInfo <- function()
{
  params <- default.params
  params$api_function <- "info"
  res <- doPOST(params)
  return(fromJSON(res))
}

getServer <- function()
{
  params <- default.params
  params$api_function <- "server"
  res <- doPOST(params)
  return(fromJSON(res))
}

createCountry <- function()
{
  params <- default.params
  params$api_function <- "create"
  params$cname <- paste(sample(state.division, 1), randomNames(n=1,which.names="first"))
  res <- doPOST(params)
  return(fromJSON(res))
}

res <- POST(url="http://www.earthempires.com/api/info", body='api_function=info&api_payload={"username":"salted","ai_key":"49ee125ad5e9a3b81dfb771ac0d3d2fb","server":"ai"}', encode="json", verbose())
content(res, "text")


#info <- getInfo()
#server <- getServer()



#country <- createCountry()
