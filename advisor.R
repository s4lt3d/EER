plot.advisor <- function(cnum, tail.n=20)
{
  old.par <- par(mfrow=c(2, 4))
  if(exists("advisor.history") == FALSE)
  {
    return(0)
  }
  
  advisor.cnum <- distinct(filter(advisor.history, cnum== cnum))
  
  tail.n <- min(tail.n, length(advisor.cnum$pop))
  if(tail.n < 10)
  {
    return(0)
  }
  
  getSlopeDebug(tail(select(advisor.cnum, turns_played, networth),n=tail.n), "networth")
  getSlopeDebug(tail(select(advisor.cnum, turns_played, money),n=tail.n), "money")
  getSlopeDebug(tail(select(advisor.cnum, turns_played, income),n=tail.n), "income")
  getSlopeDebug(tail(select(advisor.cnum, turns_played, taxes),n=tail.n), "taxes")
  getSlopeDebug(tail(select(advisor.cnum, turns_played, food),n=tail.n), "food")
  getSlopeDebug(tail(select(advisor.cnum, turns_played, foodnet),n=tail.n), "foodnet")
  getSlopeDebug(tail(select(advisor.cnum, turns_played, pop),n=tail.n), "pop")
  getSlopeDebug(tail(select(advisor.cnum, turns_played, land),n=tail.n), "land")
  par(old.par)
}

get.advisor <- function(cnum)
{
  cnum <<- cnum
  ac <- advisor(cnum)
  server <- getServer()
  advisor.current <<- tbl_df(cbind(ac, distinct(select(server, -cnum_list)), setNames(tbl_dt(as.numeric(as.POSIXct(Sys.time()))), c('Time'))))
  
  #write.table(advisor.current, file="EE_History.csv", sep=",")
  if(!exists("advisor.history"))
  {
    advisor.history <<- advisor.current
      
  }
  advisor.history <<- bind_rows(advisor.history, advisor.current)
  
  return(advisor.current)
}



networth.estimator <- function(Troops = 0, Jets = 0, Turrents=0, Tanks=0, Spies=0, Tech=0, Land=0, Buildings=0, Money=0, Food=0, Missles=0, Pop=0, Oil=0)
{
  networth <- (Troops * 0.5) + (Jets * 0.6) + (Turrents * 0.6) + (Tanks * 2) + (Spies * 1) + (Tech * 2) + (Land * 45) + (Buildings * 35) + (Money / 20000) + (Food/1000) + (Missles * 2500) + (Pop/6) + (Oil/100)
  return(networth)
}
