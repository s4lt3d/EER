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
source('advisor.R')

getInfo()
createCountry()
server <- getServer()

repeat
{
  for(cnum in server$cnum_list[[1]])
  {
    advisor.current <<- get.advisor(cnum)
    
    plot.advisor(cnum, 100) # Show the last 100 interesting things
    
    while(advisor.current$turns > 0)
    {
      print(paste("Playing", cnum, "Turn", advisor.current$turns_played))
      
      advisor.current <<- advisor(cnum)
      
      if(advisor.current$money < 60000)
      {
        cashTurn(cnum)
        next
      }
      
      if(advisor.current$empty < advisor.current$bpt)
      {
        explore(cnum)
        advisor.current <<- advisor(cnum)
        print("explore")
      }
      
      if(advisor.current$b_cs < 100)
      {
        build(cnum, cs=1)
        advisor.current <<- advisor(cnum)
        next
      }
      
      if(advisor.current$food < 1000)
      {
        privateMarketBuy(cnum, m_bu=1000)
        advisor.current <<- advisor(cnum)
        build(cnum, farm=advisor.current$bpt)
        advisor.current <<- advisor(cnum)
        tech(cnum, agri=advisor.current$tpt)
        advisor.current <<- advisor(cnum)
      }
      
      
      if(length(filter(advisor.history, cnum==cnum)$pop) > 20)
      {
        decision.table <- decisionTable(cnum)
        
        switch(decision.table$type[1],
          food={
            tech(cnum, agri=advisor.current$tpt)
            print("food")
          },
          taxes={
            build(cnum, res=advisor.current$bpt)
            print("food")
          },
          income={
            build(cnum, res=advisor.current$bpt)
            print("income")
          },
          money={
            
            if(advisor.current$food > 5000 )
            {
              privateMarketSell(cnum, m_bu=(advisor.current$food-5000))
              advisor.current <<- advisor(cnum)
            }
            build(cnum, farm=advisor.current$bpt)
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
        advisor.current <<- advisor(cnum)
        explore(cnum)
        advisor.current <<- advisor(cnum)
        cashTurn(cnum)  
        advisor.current <<- advisor(cnum)
      }
      advisor.current <<- advisor(cnum)
    }
    
  }
  print("sleeping for 120 seconds")
  Sys.sleep(120)
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

