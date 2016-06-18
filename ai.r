#Made with RStudio. R vs 3.2
library(dplyr)
library(jsonlite)
library(httr)
library(randomNames)
library(plyr)

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source('web.r') # contains all the code for getting data in and out of the server


getInfo()
getServer()
#createCountry()
countryInfo(26)
advisor(26)
simpleAdvisor(26)
privateMarketInfo(26)
#government(26, "T")
cashTurn(26)
explore(26)
build(26, farm=5)
publicMarketInfo(26)
privateMarketBuy(26, m_tr=100)
privateMarketSell(26, m_tr=100)
tech(26, res=1)
publicMarketInfo(26)
publicMarketGoods(26)
publicMarketBuy(26, m_tr=list(price=120, quantity=1))
publicMarketSell(26)

#country <- createCountry()
