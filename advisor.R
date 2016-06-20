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
