#Made with RStudio. R vs 3.2
print('2016-06-13 GREEN')
list.of.packages <- c("dplyr", "jsonlite", "httr", "randomNames", "tidyr", "stats", "RSQLite", "sqldf")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')
library("dplyr") 
library("jsonlite") 
library("httr") 
library("randomNames") 
library("tidyr")
library("stats")
library("RSQLite")
options(warn=-2)

#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)

source('web.r') # contains all the code for getting data in and out of the server
source('stat.r')
source('advisor.R')
source('game_functions.r')

server = getServer()

if(!exists("advisor.history"))
{
  if(file.exists("EE_History.csv"))
  {
    advisor.history <<- tbl_df(read.table(file="EE_History.csv", header = TRUE, sep=","))
    server <- getServer()
    advisor.history <- filter(advisor.history, round_num == server$round_num)
  }
}

# get only the current round for the bots

getInfo()

#createCountry()

repeat
{
  server <- getServer()


  for(cnum in server$cnum_list[[1]][1:5]) # play only the first 5 countries
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
          if(buy.Bushels(advisor.current$foodcon * 50) == FALSE)
          {
            cash()
          }
          
          print('food')
        },
        farm={
          build.Farms()
          tech.Agriculture() # make farms better
          buy.Bushels(advisor.current$foodcon * 50)
          print('farms')
        },
        income={
          cash()
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
        end.of.game={
          #sell of all but about 10 turns worth of food
          if(advisor.current$food > advisor.current$foodnet * 20)
          {
            sell.Bushels(advisor.current$food - advisor.current$foodnet * 20)
          }
          get.advisor(cnum) # make sure we are up to date for this
          buy.Tanks() # buy all tanks we have money for
          get.advisor(cnum) # make sure we are up to date for this
          buy.Troops() # buy all troops we have money for
          get.advisor(cnum) # make sure we are up to date for this
          buy.Turrets() # buy all turrets we have money for
          get.advisor(cnum) # make sure we are up to date for this
          buy.Jets() # buy all jets we have money for
          print('end of game')
          cash()
        },
        {
          tech.Residential()
          tech.Business()
          build.Residences()
         # build.Research.Labs()
          print('none')
        }
      )
      if(advisor.current$turns <= 0){
        break
      }
    }
    #plot.advisor(35, 2500) # Show the last 100 interesting things
  }
  print("sleeping for 8 minutes")
  Sys.sleep(120)
}