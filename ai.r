#Made with RStudio. R vs 3.2
list.of.packages <- c("dplyr", "jsonlite", "httr", "randomNames", "plyr", "tidyr", "stats")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library("dplyr") 
library("jsonlite") 
library("httr") 
library("randomNames") 
library("plyr")
library("tidyr")
library("stats")

options(warn=-2)

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source('web.r') # contains all the code for getting data in and out of the server
source('stat.r')
source('advisor.R')
source('game_functions.r')

getInfo()
createCountry()
server <- getServer()

repeat
{
  for(cnum in server$cnum_list[[1]])
  {
    repeat
    {
      get.advisor(cnum)
      government.Theocracy()
      print(paste("Playing", cnum, "Turn", advisor.current$turns_played))
      
      decisions <- decisionTable(cnum)
      
      switch(as.character(decisions$type[1]), 
         b_cs={
           build.Construction.Sites()
           print('construction')
         },
         explore={
           explore()
           print('explore')
         },
         pop={
          build.Residences()
          print('pop')
        },
        food={
          buy.Bushels(advisor.current$foodcon * 50)
          build.Farms()
          print('food')
        },
        income={
          tech.Business()
          print('income')
        },
        money={
          cash()
          print('money')
        },
        taxes={
          build.Enterprise.Zones()
          print('taxes')
        },
        {
          tech.Residential()
          tech.Business()
          build.Residences()
          build.Research.Labs()
          print('none')
        }
      )
      if(advisor.current$turns <= 0){
        break
      }
      
    }
   # plot.advisor(cnum, 100) # Show the last 100 interesting things
  }
  print("sleeping for 120 seconds")
  Sys.sleep(120*4)
  dev.off()
}


#simpleAdvisor(cnum)
#pm <- privateMarketInfo(cnum)
#government(26, "T")


#

#publicMarketInfo(cnum)
#privateMarketBuy(cnum, m_tr=100)
#privateMarketSell(cnum, m_tr=100)
#tech(cnum, agri=advisor.current$tpt)
#publicMarketInfo(cnum)
#publicMarketGoods(cnum)
#publicMarketBuy(cnum, m_tr=list(price=120, quantity=1))
#publicMarketSell(cnum)

#country <- createCountry()

