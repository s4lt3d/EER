# Easy to use functions for every day development

# advisor.history, cnum is a global variable

# build.Residences()
# build.Enterprise.Zones()
# build.Industrial.Complexes()
# build.Construction.Sites()
# build.Oil.Rigs()
# build.Farms()
# build.Research.Labs()
# build.Military.Bases()
# 
# tech.Spy()
# tech.SDI()
# tech.Weapons()
# tech.Industrial()
# tech.Military()
# tech.Medical()
# tech.Business()
# tech.Residential()
# tech.Agriculture()
# tech.Warfare()
# tech.Military.Strategy()
# 
# get.advisor(cnum)
# government.Monarchy()
# government.Demoncracy()
# government.Fascism()
# government.Tyranny()
# government.Dictatorship()
# government.Communism()
# government.Theocracy()
# government.Republic()

# buy.Tanks(5000)
# buy.Turrets(5000)
# buy.Jets(5000)
# buy.Troops(5000)
# buy.Bushels(5000)
# buy.Military.Tech(5000)
# buy.Medical.Tech(5000)
# buy.Business.Tech(5000)
# buy.Residential.Tech(5000)
# buy.Agriculture.Tech(5000)
# buy.Warfare.Tech(5000)
# buy.Military.Strategy.Tech(5000)
# buy.Weapons.Tech(5000)
# buy.Industrial.Tech(5000)
# buy.Spy.Tech(5000)
# buy.SDI.Tech(5000)
#
#
# sell.Bushels(5000)
# sell.Spies(5000)
# sell.Troops(5000)
# sell.Jets(5000)
# sell.Turrents(5000)
# sell.Tanks(5000)
# sell.Oil(5000)
# sell.Military.Tech(5000)
# sell.Medical.Tech(5000)
# sell.Business.Tech(5000)
# sell.Residential.Tech(5000)
# sell.Agricultural.Tech(5000)
# sell.Warfare.Tech(5000)
# sell.Military.Strategy.Tech(5000)
# sell.Weapons.Tech(5000)
# sell.Industrial.Tech(5000)
# sell.Spy.Tech(5000)
# sell.SDI.Tech(5000)

setCountry <- function(countryNum)
{
  cnum <<- countryNum
}

max.Buildings <- function()
{
  buildings.per.turn <- select(filter(advisor.current, cnum==cnum), bpt)$bpt
  return(buildings.per.turn)
}

max.Tech <- function()
{
  tech.per.turn <- select(filter(advisor.current, cnum==cnum), tpt)$tpt
  return(tech.per.turn)
}

government.will.change <- function(gov = "M")
{
  current.gov <- select(filter(advisor.current, cnum==cnum), govt)$govt
  return(!(current.gov == gov))
}

cash <- function()
{
  response <- cashTurn(cnum)
  return(!is.null(response$turns))
}

explore <- function()
{
  response = exploreTurn(cnum)
  return(!is.null(response$turns))
}

build.Residences <- function()
{
  mb <- max.Buildings()
  response <- buildTurn(cnum, res=mb)
  return(!is.null(response$turns))
}

build.Enterprise.Zones <- function()
{
  maxb <- max.Buildings()
  response <- buildTurn(cnum, ent=maxb)
  return(!is.null(response$turns))
}

build.Industrial.Complexes <- function()
{
  mb <- max.Buildings()
  response <- buildTurn(cnum, indy=mb)
  return(!is.null(response$turns))
}

build.Military.Bases <- function()
{
  mb <- max.Buildings()
  response <- buildTurn(cnum, mb=mb)
  return(!is.null(response$turns))
}

build.Research.Labs <- function()
{
  mb <- max.Buildings()
  response <- buildTurn(cnum, lab=mb)
  return(!is.null(response$turns))
}

build.Farms <- function()
{
  mb <- max.Buildings()
  response <- buildTurn(cnum, farm=mb)
  return(!is.null(response$turns))
}

build.Oil.Rigs <- function()
{
  mb <- max.Buildings()
  response <- buildTurn(cnum, rig=mb)
  return(!is.null(response$turns))
}

build.Construction.Sites <- function()
{
  # can build one per turn
  response <- buildTurn(cnum, cs=1)
  return(!is.null(response$turns))
}


#tech <- function(cnum, mil=0, med=0, bus=0, res=0, agri=0, war=0, ms=0, weap=0, indy=0, spy=0, sdi=0)

tech.Spy <- function()
{
  mt <- max.Tech()
  response <- tech(cnum, spy=mt)
  return(!is.null(response$turns))
}

tech.SDI <- function()
{
  mt <- max.Tech()
  response <- tech(cnum, sdi=mt)
  return(!is.null(response$turns))
}

tech.Weapons <- function()
{
  mt <- max.Tech()
  response <- tech(cnum, weap=mt)
  return(!is.null(response$turns))
}

tech.Industrial <- function()
{
  mt <- max.Tech()
  response <- tech(cnum, indy=mt)
  return(!is.null(response$turns))
}
 
tech.Military <- function()
{
  mt <- max.Tech()
  response <- tech(cnum, mil=mt)
  return(!is.null(response$turns))
}

tech.Medical <- function()
{
  mt <- max.Tech()
  response <- tech(cnum, med=mt)
  return(!is.null(response$turns))
}

tech.Business <- function()
{
  mt <- max.Tech()
  response <- tech(cnum, bus=mt)
  return(!is.null(response$turns))
}

tech.Residential <- function()
{
  mt <- max.Tech()
  response <- tech(cnum, res=mt)
  return(!is.null(response$turns))
}

tech.Agriculture <- function()
{
  mt <- max.Tech()
  response <- tech(cnum, agri=mt)
  return(!is.null(response$turns))
}

tech.Warfare <- function()
{
  mt <- max.Tech()
  response <- tech(cnum, war=mt)
  return(!is.null(response$turns))
}

tech.Military.Strategy <- function()
{
  mt <- max.Tech()
  response <- tech(cnum, ms=mt)
  return(!is.null(response$turns))
}

government.Monarchy <- function()
{
  if(government.will.change("M") == FALSE)
  {
    return(TRUE) # act like a turn was successful, no need to change
  }
  response <- government(cnum, "M")
  return(!is.null(response$govt))
}


government.Demoncracy <- function()
{
  if(government.will.change("D") == FALSE)
  {
    return(TRUE) # act like a turn was successful, no need to change
  }
  response <- government(cnum, "D")
  return(!is.null(response$govt))
}

government.Fascism <- function()
{
  if(government.will.change("F") == FALSE)
  {
    return(TRUE) # act like a turn was successful, no need to change
  }
  response <- government(cnum, "F")
  return(!is.null(response$govt))
}

government.Tyranny <- function()
{
  if(government.will.change("T") == FALSE)
  {
    return(TRUE) # act like a turn was successful, no need to change
  }
  response <- government(cnum, "T")
  return(!is.null(response$govt))
}

government.Dictatorship <- function()
{
  if(government.will.change("I") == FALSE)
  {
    return(TRUE) # act like a turn was successful, no need to change
  }
  response <- government(cnum, "I")
  return(!is.null(response$govt))
}

government.Communism <- function()
{
  if(government.will.change("C") == FALSE)
  {
    return(TRUE) # act like a turn was successful, no need to change
  }
  response <- government(cnum, "C")
  return(!is.null(response$govt))
}

government.Theocracy <- function()
{
  if(government.will.change("H") == FALSE)
  {
    return(TRUE) # act like a turn was successful, no need to change
  }
  response <- government(cnum, "H")
  return(!is.null(response$govt))
}

government.Republic <- function()
{
  if(government.will.change("R") == FALSE)
  {
    return(TRUE) # act like a turn was successful, no need to change
  }
  response <- government(cnum, "R")
  return(!is.null(response$govt))
}

buy.Bushels <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "m_bu", available >= qty), buy_price)
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  buy_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  if(market == "public")
  {
    response <- publicMarketBuy(cnum, m_bu=list(price=buy_price, quantity=qty))
    return(!is.null(response$bought))
  } else {
    response <- privateMarketBuy(cnum, m_bu=qty)
    return(!is.null(response$cost))
  }
}

buy.Troops <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "m_tr", available >= qty), buy_price)
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  buy_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  if(market == "public")
  {
    response <- publicMarketBuy(cnum, m_tr=list(price=buy_price, quantity=qty))
    return(!is.null(response$bought))
  } else {
    response <- privateMarketBuy(cnum, m_tr=qty)
    return(!is.null(response$cost))
  }
}

buy.Jets <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "m_j", available >= qty), buy_price)
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  buy_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  if(market == "public")
  {
    response <- publicMarketBuy(cnum, m_j=list(price=buy_price, quantity=qty))
    return(!is.null(response$bought))
  } else {
    response <- privateMarketBuy(cnum, m_j=qty)
    return(!is.null(response$cost))
  }
}

buy.Turrets <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "m_tu", available >= qty), buy_price)
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  buy_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  if(market == "public")
  {
    response <- publicMarketBuy(cnum, m_tu=list(price=buy_price, quantity=qty))
    return(!is.null(response$bought))
  } else {
    response <- privateMarketBuy(cnum, m_tu=qty)
    return(!is.null(response$cost))
  }
}

buy.Tanks <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "m_ta", available >= qty), buy_price)
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  buy_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  if(market == "public")
  {
    response <- publicMarketBuy(cnum, m_ta=list(price=buy_price, quantity=qty))
    return(!is.null(response$bought))
  } else {
    response <- privateMarketBuy(cnum, m_ta=qty)
    return(!is.null(response$cost))
  }
}

buy.Oil <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "m_oil", available >= qty), buy_price)
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  buy_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketBuy(cnum, m_oil=list(price=buy_price, quantity=qty))
  return(!is.null(response$bought))
}

buy.Military.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "mil", available >= qty), buy_price)
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  buy_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketBuy(cnum, mil=list(price=buy_price, quantity=qty))
  return(!is.null(response$bought))
}

buy.Medical.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "med", available >= qty), buy_price)
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  buy_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketBuy(cnum, med=list(price=buy_price, quantity=qty))
  return(!is.null(response$bought))
}

buy.Business.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "bus", available >= qty), buy_price)
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  buy_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketBuy(cnum, bus=list(price=buy_price, quantity=qty))
  return(!is.null(response$bought))
}

buy.Residential.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "res", available >= qty), buy_price)
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  buy_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketBuy(cnum, res=list(price=buy_price, quantity=qty))
  return(!is.null(response$bought))
}

buy.Agriculture.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "agri", available >= qty), buy_price)
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  buy_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketBuy(cnum, agri=list(price=buy_price, quantity=qty))
  return(!is.null(response$bought))
}

buy.Warfare.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "war", available >= qty), buy_price)
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  buy_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketBuy(cnum, war=list(price=buy_price, quantity=qty))
  return(!is.null(response$bought))
}

buy.Military.Strategy.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "ms", available >= qty), buy_price)
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  buy_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketBuy(cnum, ms=list(price=buy_price, quantity=qty))
  return(!is.null(response$bought))
}

buy.Weapons.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "weap", available >= qty), buy_price)
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  buy_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketBuy(cnum, weap=list(price=buy_price, quantity=qty))
  return(!is.null(response$bought))
}

buy.Industrial.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "indy", available >= qty), buy_price)
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  buy_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketBuy(cnum, indy=list(price=buy_price, quantity=qty))
  return(!is.null(response$bought))
}

buy.Spy.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "spy", available >= qty), buy_price)
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  buy_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketBuy(cnum, spy=list(price=buy_price, quantity=qty))
  return(!is.null(response$bought))
}

buy.SDI.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "sdi", available >= qty), buy_price)
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  buy_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketBuy(cnum, sdi=list(price=buy_price, quantity=qty))
  return(!is.null(response$bought))
}

sell.Bushels <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "m_bu"), desc(sell_price))
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  sell_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  if(market == "public")
  {
    response <- publicMarketSell(cnum, m_bu=list(price=sell_price, quantity=qty))
    return(!is.null(response$turns))
  } else {
    response <- privateMarketSell(cnum, m_bu=qty)
    return(!is.null(response$goods))
  }
}

sell.Spies <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "m_spy"), desc(sell_price))
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  sell_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  if(market == "public")
  {
    response <- publicMarketSell(cnum, m_spy=list(price=sell_price, quantity=qty))
    return(!is.null(response$turns))
  } else {
    response <- privateMarketSell(cnum, m_spy=qty)
    return(!is.null(response$goods))
  }
}

sell.Troops <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "m_tr"), desc(sell_price))
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  sell_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  if(market == "public")
  {
    response <- publicMarketSell(cnum, m_tr=list(price=sell_price, quantity=qty))
    return(!is.null(response$turns))
  } else {
    response <- privateMarketSell(cnum, m_tr=qty)
    return(!is.null(response$goods))
  }
}

sell.Jets <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "m_j"), desc(sell_price))
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  sell_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  if(market == "public")
  {
    response <- publicMarketSell(cnum, m_j=list(price=sell_price, quantity=qty))
    return(!is.null(response$turns))
  } else {
    response <- privateMarketSell(cnum, m_j=qty)
    return(!is.null(response$goods))
  }
}

sell.Turrents <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "m_tu"), desc(sell_price))
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  sell_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  if(market == "public")
  {
    response <- publicMarketSell(cnum, m_tu=list(price=sell_price, quantity=qty))
    return(!is.null(response$turns))
  } else {
    response <- privateMarketSell(cnum, m_tu=qty)
    return(!is.null(response$goods))
  }
}

sell.Tanks <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "m_ta"), desc(sell_price))
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  sell_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  if(market == "public")
  {
    response <- publicMarketSell(cnum, m_ta=list(price=sell_price, quantity=qty))
    return(!is.null(response$turns))
  } else {
    response <- privateMarketSell(cnum, m_ta=qty)
    return(!is.null(response$goods))
  }
}

sell.Oil <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "m_oil"), desc(sell_price))
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  sell_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  if(market == "public")
  {
    response <- publicMarketSell(cnum, m_oil=list(price=sell_price, quantity=qty))
    return(!is.null(response$turns))
  } else {
    response <- privateMarketSell(cnum, m_oil=qty)
    return(!is.null(response$goods))
  }
}

sell.Military.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "mil"), desc(sell_price))
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  sell_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market

  response <- publicMarketSell(cnum, m_oil=list(price=sell_price, quantity=qty))
  return(!is.null(response$turns))
}

sell.Medical.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "med"), desc(sell_price))
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  sell_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketSell(cnum, med=list(price=sell_price, quantity=qty))
  return(!is.null(response$turns))
}

sell.Business.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "bus"), desc(sell_price))
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  sell_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketSell(cnum, bus=list(price=sell_price, quantity=qty))
  return(!is.null(response$turns))
}

sell.Residential.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "res"), desc(sell_price))
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  sell_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketSell(cnum, res=list(price=sell_price, quantity=qty))
  return(!is.null(response$turns))
}

sell.Agricultural.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "agri"), desc(sell_price))
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  sell_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketSell(cnum, agri=list(price=sell_price, quantity=qty))
  return(!is.null(response$turns))
}

sell.Warfare.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "war"), desc(sell_price))
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  sell_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketSell(cnum, war=list(price=sell_price, quantity=qty))
  return(!is.null(response$turns))
}

sell.Military.Strategy.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "ms"), desc(sell_price))
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  sell_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketSell(cnum, ms=list(price=sell_price, quantity=qty))
  return(!is.null(response$turns))
}

sell.Weapons.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "weap"), desc(sell_price))
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  sell_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketSell(cnum, weap=list(price=sell_price, quantity=qty))
  return(!is.null(response$turns))
}

sell.Industrial.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "indy"), desc(sell_price))
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  sell_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketSell(cnum, indy=list(price=sell_price, quantity=qty))
  return(!is.null(response$turns))
}

sell.Spy.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "spy"), desc(sell_price))
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  sell_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketSell(cnum, spy=list(price=sell_price, quantity=qty))
  return(!is.null(response$turns))
}

sell.SDI.Tech <- function(qty)
{
  mi <- marketInfo(cnum)
  mi <- arrange(filter(mi, type== "sdi"), desc(sell_price))
  if(length(mi$buy_price) == 0)
  {
    return(FALSE)
  }
  
  sell_price <- head(mi, n=1)$buy_price
  market <- head(mi, n=1)$market
  
  response <- publicMarketSell(cnum, sdi=list(price=sell_price, quantity=qty))
  return(!is.null(response$turns))
}



