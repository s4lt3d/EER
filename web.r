base.url <- "http://www.earthempires.com/api"
user.name <- "salted"
api.key <- "49ee125ad5e9a3b81dfb771ac0d3d2fb"

timeout(5) # set timeout for web requests

#default.params <- paste("?", "username=", user.name, "&ai_key=", api.key, "&server=ai", sep="")
default.params <- list()
default.params$username <- user.name
default.params$ai_key <- api.key
default.params$server <- "ai"

fixjson <- function(json)
{
  
  if(grepl(':', x=json) == FALSE)
  {
    json <- sub("(\\w*)", "\"ERROR\":\"\\1\"", x=json)
  } else {
    json <- sub("(\\w*):", "\"\\1\":", x=json) #Adds quotes around first word
  }
  
  json <- paste("{", json, "}", sep="") # Adds {} around json 
  return(json)
}

doPOST <- function(params)
{
  post.url <- base.url 
  p <- toJSON(params, auto_unbox=TRUE)
  req <- ""
  #print(p)
  tryCatch( # POST can timeout so catch this. 
    req <- POST(url=post.url, body=list(api_payload=p), encode="form"),
    error=function(X){ req <-'{"response":"ERROR"}'}
  )
  json <- content(req, "text", encoding = "UTF-8")
  #print(json)
  json <- fixjson(json)
  return(json)
}

getInfo <- function()
{
  params <- default.params
  params$api_function <- "info"
  res <- doPOST(params)
  return(tbl_dt(fromJSON(res)$USER_INFO))
}

getServer <- function()
{
  params <- default.params
  params$api_function <- "server"
  res <- doPOST(params)
  res <- fromJSON(res)$SERVER
  return(tbl_dt(res))
}

createCountry <- function()
{
  params <- default.params
  params$api_function <- "create"
  params$cname <- paste("R_bot", sample(state.division, 1), randomNames(n=1,which.names="first"))
  res <- doPOST(params)
  return(tbl_dt(fromJSON(res)$CREATE))
}

countryInfo <- function(cnum)
{
  params <- default.params
  params$api_function <- "main"
  params$cnum <- cnum
  res <- doPOST(params)
  return(tbl_dt(fromJSON(res)$MAIN))
}

# m_ advisor
# t_ tech total points
# ps_ planned strike with delay
# pt_ percentage resulting from tech 
advisor <- function(cnum)
{
  params <- default.params
  params$api_function <- "advisor"
  params$cnum <- cnum
  res <- doPOST(params)
  return(tbl_dt(fromJSON(res)$ADVISOR))
}

simpleAdvisor <- function(cnum)
{
  return(select(advisor(26), networth, turns_stored, pop, land, money, food, oil, taxrate, build_cost, income, taxes, expenses))
}

privateMarketInfo <- function(cnum)
{
  params <- default.params
  params$api_function <- "pm_info"
  params$cnum <- cnum
  res <- doPOST(params)
  json <- fromJSON(res)$PM_INFO
  json$buy_price$m_spy <- 0
  json$available$m_spy <- 0
  upm <- lapply(json, ldply)
  m2 <- merge(upm$available, upm$sell_price, by='.id')
  m3 <- merge(m2, upm$buy_price, by='.id')
  colnames(m3) <- c('type', 'available', 'sell_price', 'buy_price')
  print("PM")
  return(tbl_dt(m3))
}

pm <-privateMarketInfo(26)

#!
# M D T F H(Theocracy) C I(Dictatorship) R 
government <- function(cnum, govt)
{
  params <- default.params
  params$api_function <- "govt"
  params$cnum <- cnum
  params$govt <- govt
  res <- doPOST(params)
  print("G")
  return(tbl_dt(fromJSON(res)$GOVT))
}

cashTurn <- function(cnum)
{
  params <- default.params
  params$api_function <- "cash"
  params$cnum <- cnum
  params$turns <- 1
  res <- doPOST(params)
  print("C")
  return(tbl_dt(fromJSON(res)$CASH))
}

explore <- function(cnum)
{
  params <- default.params
  params$api_function <- "explore"
  params$cnum <- cnum
  params$turns <- 1
  res <- doPOST(params)
  print("E")
  return(tbl_dt(fromJSON(res)$EXPLORE))
}

build <- function(cnum, ent=0, res=0, indy=0, mb=0, lab=0, farm=0, rig=0, cs=0)
{
  params <- default.params
  params$api_function <- "build"
  params$cnum <- cnum
  params$build <- list(farm=farm, ent=ent, res=res, indy=indy, mb=mb, lab=lab, farm=farm, rig=rig, cs=cs)
  res <- doPOST(params)
  print("B")
  return(tbl_dt(fromJSON(res)$BUILD))
}

privateMarketBuy <- function(cnum, m_tr=0, m_j=0, m_tu=0, m_ta=0, m_bu=0, m_oil=0)
{
  params <- default.params
  params$api_function <- "pm"
  params$cnum <- cnum
  params$buy <- list( m_tr=m_tr, m_j=m_j, m_tu=m_tu, m_ta=m_ta, m_bu=m_bu, m_oil=m_oil)
  res <- doPOST(params)
  print("PM")
  return(tbl_dt(fromJSON(res)$PM))
}

privateMarketSell <- function(cnum,  m_spy=0, m_tr=0, m_j=0, m_tu=0, m_ta=0, m_bu=0, m_oil=0)
{
  params <- default.params
  params$api_function <- "pm"
  params$cnum <- cnum
  params$sell <- list( m_spy=m_spy, m_tr=m_tr, m_j=m_j, m_tu=m_tu, m_ta=m_ta, m_bu=m_bu, m_oil=m_oil)
  res <- doPOST(params)
  return(tbl_dt(fromJSON(res)$PM))
}

tech <- function(cnum, mil=0, med=0, bus=0, res=0, agri=0, war=0, ms=0, weap=0, indy=0, spy=0, sdi=0)
{
  params <- default.params
  params$api_function <- "tech"
  params$cnum <- cnum
  params$tech <- list(mil=mil, med=med, bus=bus, res=res, agri=agri, war=war, ms=ms, weap=weap, indy=indy, spy=spy, sdi=sdi)
  res <- doPOST(params)
  print("T")
  return(tbl_dt(fromJSON(res)$TECH))
}

publicMarketInfo <- function(cnum)
{
  params <- default.params
  params$api_function <- "market"
  params$cnum <- cnum
  res <- doPOST(params)
  pm <- fromJSON(res)$MARKET
  pm <- setNames(data.frame(cbind(pm$buy_price, pm$so_price, pm$available)), c('buy_price', 'so_price', 'available'))
  pm$type <- rownames(pm)
  return(tbl_df(pm))
}


publicMarketGoods <- function(cnum)
{
  params <- default.params
  params$api_function <- "onmarket"
  params$cnum <- cnum
  res <- doPOST(params)
  return(tbl_dt(fromJSON(res)$ONMARKET))
}

publicMarketBuy <- function(cnum, m_tr=list(price=0, quantity=0), m_j=list(price=0, quantity=0), m_tu=list(price=0, quantity=0), m_ta=list(price=0, quantity=0), m_bu=list(price=0, quantity=0), m_oil=list(price=0, quantity=0), mil=list(price=0, quantity=0), med=list(price=0, quantity=0), bus=list(price=0, quantity=0), res=list(price=0, quantity=0), agri=list(price=0, quantity=0), war=list(price=0, quantity=0), ms=list(price=0, quantity=0), weap=list(price=0, quantity=0), indy=list(price=0, quantity=0), spy=list(price=0, quantity=0), sdi=list(price=0, quantity=0))
{    
  params <- default.params
  params$api_function <- "buy"
  params$cnum <- cnum
  params$price <- list(m_tr=120)
  params$quantity <- list(m_tr=1)
  res <- doPOST(params)
  return((fromJSON(res)$BUY))
}



publicMarketSell <- function(cnum)
{
  params <- default.params
  params$api_function <- "sell"
  params$cnum <- cnum
  params$price <- list(m_bu=500)
  params$quantity <- list(m_bu=5000)
  res <- doPOST(params)
  return((fromJSON(res)$SELL))
}

