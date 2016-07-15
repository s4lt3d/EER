#Made with RStudio. R vs 3.3
print('2016-07-13 REDISH')
list.of.packages <- c("plyr", "dplyr", "jsonlite", "httr", "randomNames", "tidyr", "stats", "RSQLite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')
library("plyr")
library("dplyr") 
library("jsonlite") 
library("httr") 
library("randomNames") 
library("tidyr")
library("stats")
library("RSQLite")
#library("RSQLite")
options(warn=-2)

source('web.r') # contains all the code for getting data in and out of the server
source('stat.r')
source('advisor.R')
source('game_functions.r')

server <<- getServer()

if(!exists("sqlite.con")){
  sqlite.con <<- dbConnect(RSQLite::SQLite(), dbname="advisor.sqlite")
}

tables <- dbListTables(sqlite.con)

advisor.history <- dbReadTable(conn= sqlite.con, name="advisor_history")
advisor.history <- tbl_df(advisor.history)

# get only the current round for the bots
getInfo()

<<<<<<< Updated upstream
=======
for(i in 1:35) createCountry()

>>>>>>> Stashed changes
repeat
{
  for(i in 1:25) createCountry()
  
  server <- getServer()
  bots <- server$cnum_list[[1]][1:25]
  
  for(cnum in sample(bots, length(bots))) # play only the first 5 countries
  {
    if(is.na(cnum)) break
    repeat
    {
      get.advisor(cnum)
      government.Theocracy()
      print(paste("Playing", cnum, "Turn", advisor.current$turns_played))
      
      decisions <- decisionTable(cnum)
      #print(decisions)
      for(i in 1:length(decisions$type)) 
      {
        switch(as.character(decisions$type[i]), 
           b_cs={
             if(build.Construction.Sites()){
               print('construction')
               break
             }
             
           },
           explore={
             if(explore(advisor.current$turns)) {
               print('explore')
               break
             }
           },
           pop={
            if(build.Residences()) {
              print('pop')
              break
            }
          },
          food={
            if(buy.Bushels(advisor.current$foodcon * 60))
            {
              print('food')
              break
            }
          },
          farm={
            
            if(build.Farms()) {
              tech.Agriculture() # make farms better
              buy.Bushels(advisor.current$foodcon * 60)
              print('farms')
              break
            }
          },
          income={
            if(cash()) {
              tech.Business()
              print('income')
              break
            }
          },
          money={
            if(cash()) {
              print('money')
              break
            }
          },
          taxes={ 
            if(build.Enterprise.Zones()) {
              print('taxes')
              break
            }
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
          },
          {
            if(build.Residences()) {
              tech.Residential()
              tech.Business()
              if(sample(1:10, 1) > 5) {
                build.Research.Labs()
                
              }
              print('none / residences')
              break
            }
            
          }
        ) # end switch
      }   # end for
      
      if(advisor.current$turns <= 0){
        break
      }
    }
    plot.advisor(cnum, 2500) # Show the last 100 interesting things
  }
  
  if((advisor.current$reset_end - advisor.current$time) / 3600 > 5)
  {
    print("sleeping for up to two hours")
    Sys.sleep(sample(600:14400, 1))  
  } else { # sleep less near end of game
    print("sleeping for 10 minutes")
    Sys.sleep(600)  
  }
}