# This is the low level comms for talking to the server. 
# See game_functions.r for game development

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
  m3 <- bind_cols(m3, data.frame(rep("private", length(m3$.id))))
  colnames(m3) <- c('type', 'available', 'sell_price', 'buy_price', 'market')
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
  return(tbl_dt(fromJSON(res)$GOVT))
}

cashTurn <- function(cnum)
{
  params <- default.params
  params$api_function <- "cash"
  params$cnum <- cnum
  params$turns <- 1
  res <- doPOST(params)
  return(tbl_dt(fromJSON(res)$CASH))
}

exploreTurn <- function(cnum)
{
  params <- default.params
  params$api_function <- "explore"
  params$cnum <- cnum
  params$turns <- 1
  res <- doPOST(params)
  return(tbl_dt(fromJSON(res)$EXPLORE))
}

buildTurn <- function(cnum, ent=0, res=0, indy=0, mb=0, lab=0, farm=0, rig=0, cs=0)
{
  params <- default.params
  params$api_function <- "build"
  params$cnum <- cnum
  params$build <- list(ent=ent, res=res, indy=indy, mb=mb, lab=lab, farm=farm, rig=rig, cs=cs)
  res <- doPOST(params)
  return(tbl_dt(fromJSON(res)$BUILD))
}

privateMarketBuy <- function(cnum, m_tr=0, m_j=0, m_tu=0, m_ta=0, m_bu=0, m_oil=0)
{
  params <- default.params
  params$api_function <- "pm"
  params$cnum <- cnum
  params$buy <- list( m_tr=m_tr, m_j=m_j, m_tu=m_tu, m_ta=m_ta, m_bu=m_bu, m_oil=m_oil)
  res <- doPOST(params)
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
  return(tbl_dt(fromJSON(res)$TECH))
}

publicMarketInfo <- function(cnum)
{
  params <- default.params
  params$api_function <- "market"
  params$cnum <- cnum
  res <- doPOST(params)
  pm <- fromJSON(res)$MARKET
  
  market.table <- tbl_dt(list())
  
  pm$available[pm$available == "NULL"] <- 0
  pm$buy_price[pm$buy_price == "NULL"] <- 0
  pm$so_price[pm$so_price == "NULL"] <- 0
  
  for(i in 1:length(pm$available))
  {
    name <- colnames(data.frame(pm$available[i]))
    available <- as.numeric(pm$available[i])
    buy_price <- as.numeric(pm$buy_price[i])
    sell_price <- buy_price * 0.95 # sell 5% below market price
    tbl.row <- tbl_dt(t(c(name, available, sell_price, buy_price, "public")))
    market.table <- bind_rows(market.table, tbl.row)
  }
  colnames(market.table) <- c("type", "available","sell_price", "buy_price", "market")
  market.table <- mutate(market.table, sell_price, sell_price = as.numeric(sell_price))
  market.table <- mutate(market.table, buy_price, buy_price = as.numeric(buy_price))
  market.table <- mutate(market.table, available, available = as.numeric(available))
  
  return(market.table)
}


publicMarketGoods <- function(cnum)
{
  params <- default.params
  params$api_function <- "onmarket"
  params$cnum <- cnum
  res <- doPOST(params)
  res <- fromJSON(res)$ONMARKET
  if(length(res$goods) == 0){return(NULL)}
  
  return(tbl_df(res$goods))
}

publicMarketBuy <- function(cnum, m_tr=list(price=0, quantity=0), m_j=list(price=0, quantity=0), m_tu=list(price=0, quantity=0), m_ta=list(price=0, quantity=0), m_bu=list(price=0, quantity=0), m_oil=list(price=0, quantity=0), mil=list(price=0, quantity=0), med=list(price=0, quantity=0), bus=list(price=0, quantity=0), res=list(price=0, quantity=0), agri=list(price=0, quantity=0), war=list(price=0, quantity=0), ms=list(price=0, quantity=0), weap=list(price=0, quantity=0), indy=list(price=0, quantity=0), spy=list(price=0, quantity=0), sdi=list(price=0, quantity=0))
{    
  params <- default.params
  params$api_function <- "buy"
  params$cnum <- cnum
  params$price <- list(m_tr=m_tr$price,
                       m_j=m_j$price, 
                       m_tu=m_tu$price, 
                       m_ta=m_ta$price, 
                       m_bu=m_bu$price, 
                       m_oil=m_oil$price, 
                       mil=mil$price, 
                       med=med$price, 
                       bus=bus$price, 
                       res=res$price, 
                       agri=agri$price, 
                       war=war$price, 
                       ms=ms$price, 
                       weap=weap$price, 
                       indy=indy$price, 
                       spy=spy$price, 
                       sdi=sdi$price)
  
  params$quantity <- list(m_tr=m_tr$quantity,
                          m_j=m_j$quantity, 
                          m_tu=m_tu$quantity, 
                          m_ta=m_ta$quantity, 
                          m_bu=m_bu$quantity, 
                          m_oil=m_oil$quantity, 
                          mil=mil$quantity, 
                          med=med$quantity, 
                          bus=bus$quantity, 
                          res=res$quantity, 
                          agri=agri$quantity, 
                          war=war$quantity, 
                          ms=ms$quantity, 
                          weap=weap$quantity, 
                          indy=indy$quantity, 
                          spy=spy$quantity, 
                          sdi=sdi$quantity)
  res <- doPOST(params)
  return((fromJSON(res)$BUY))
}

publicMarketSell <- function(cnum, m_tr=list(price=0, quantity=0), m_j=list(price=0, quantity=0), m_tu=list(price=0, quantity=0), m_ta=list(price=0, quantity=0), m_bu=list(price=0, quantity=0), m_oil=list(price=0, quantity=0), mil=list(price=0, quantity=0), med=list(price=0, quantity=0), bus=list(price=0, quantity=0), res=list(price=0, quantity=0), agri=list(price=0, quantity=0), war=list(price=0, quantity=0), ms=list(price=0, quantity=0), weap=list(price=0, quantity=0), indy=list(price=0, quantity=0), spy=list(price=0, quantity=0), sdi=list(price=0, quantity=0))
{
  params <- default.params
  params$api_function <- "sell"
  params$cnum <- cnum
  params$price <- list(m_tr=m_tr$price,
                       m_j=m_j$price, 
                       m_tu=m_tu$price, 
                       m_ta=m_ta$price, 
                       m_bu=m_bu$price, 
                       m_oil=m_oil$price, 
                       mil=mil$price, 
                       med=med$price, 
                       bus=bus$price, 
                       res=res$price, 
                       agri=agri$price, 
                       war=war$price, 
                       ms=ms$price, 
                       weap=weap$price, 
                       indy=indy$price, 
                       spy=spy$price, 
                       sdi=sdi$price)
  
  params$quantity <- list(m_tr=m_tr$quantity,
                          m_j=m_j$quantity, 
                          m_tu=m_tu$quantity, 
                          m_ta=m_ta$quantity, 
                          m_bu=m_bu$quantity, 
                          m_oil=m_oil$quantity, 
                          mil=mil$quantity, 
                          med=med$quantity, 
                          bus=bus$quantity, 
                          res=res$quantity, 
                          agri=agri$quantity, 
                          war=war$quantity, 
                          ms=ms$quantity, 
                          weap=weap$quantity, 
                          indy=indy$quantity, 
                          spy=spy$quantity, 
                          sdi=sdi$quantity)
  res <- doPOST(params)
  return((fromJSON(res)$SELL))
}

marketInfo <- function(cnum)
{
  return(bind_rows(privateMarketInfo(cnum), publicMarketInfo(cnum)))
}