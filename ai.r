#Made with RStudio. R vs 3.2
list.of.packages <- c("dplyr", "jsonlite", "httr", "randomNames", "plyr", "tidyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library("dplyr") 
library("jsonlite") 
library("httr") 
library("randomNames") 
library("plyr")
library("tidyr")

options(warn=-2)

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source('web.r') # contains all the code for getting data in and out of the server
source('stat.r')

getInfo()
createCountry()
server <- getServer()

while(TRUE)
{
  for(cnum in server$cnum_list[[1]])
  {
    #cnum <- server$cnum_list[[1]][length(server$cnum_list[[1]])]
    
    countryInfo(cnum)
    
    advisor.current <- advisor(cnum)
    
    while(advisor.current$turns > 0)
    {
      print(paste("Playing", cnum, "Turn", advisor.current$turns_played))
      
      advisor.current <- advisor(cnum)
      if(!exists("advisor.history"))
      {
        advisor.history <- advisor.current
      }
      advisor.history <- bind_rows(advisor.history, advisor.current)
    
      if(advisor.current$money < 60000)
      {
        cashTurn(cnum)
        next
      }
      
      if(advisor.current$empty < advisor.current$bpt)
      {
        explore(cnum)
        print("explore")
      }
      
      
      
      if(advisor.current$b_cs < 100)
      {
        build(cnum, cs=1)
        next
      }
      
      if(advisor.current$food < 1000)
      {
        privateMarketBuy(cnum, m_bu=1000)
        build(cnum, farm=advisor.current$bpt)
        tech(cnum, agri=advisor.current$tpt)
      }
      
      
      if(length(filter(advisor.history, cnum==cnum)$pop) > 20)
      {
        decision.table <- decisionTable(advisor.history, cnum)
        
        switch(decision.table$type[1],
          foodnet={
            tech(cnum, agri=advisor.current$tpt)
            print("foodnet")
          },
          taxes={
            build(cnum, res=advisor.current$bpt)
            print("foodnet")
          },
          income={
            build(cnum, res=advisor.current$bpt)
            print("income")
          },
          money={
            tech(cnum, res=advisor.current$tpt)
            print("money")
          },
          pop={
            build(cnum, res=advisor.current$bpt)
            print("pop")
          },
          pci={ 
            explore(cnum)
            print("pci")
          })
      } else {
        build(cnum, cs=1)
        explore(cnum)
        cashTurn(cnum)  
      }
      
    }
    
  }
  print("sleeping for 60 seconds")
  Sys.sleep(60)
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

