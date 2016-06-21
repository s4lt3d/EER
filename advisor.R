plot.advisor <- function(cnum, tail.n=20)
{
  old.par <- par(mfrow=c(3, 4))
  if(exists("advisor.history") == FALSE)
  {
    return(0)
  }
  country_num <- cnum
  advisor.cnum <- arrange(distinct(filter(advisor.history, cnum== country_num, round_num==server$round_num)), turns_played)
  
  advisor.cnum <- advisor.cnum %>% filter(cnum==country_num, round_num==server$round_num) %>% distinct(.) %>% do(tail(., n=tail.n))
  
  tail.n <- min(tail.n, length(advisor.cnum$pop))
  if(tail.n < 10)
  {
    return(0)
  }
  
  getSlopeDebug(distinct(select(advisor.cnum, turns_played, networth)), "networth")
  getSlopeDebug(tail(select(advisor.cnum, turns_played, money),n=tail.n), "money")
  getSlopeDebug(tail(select(advisor.cnum, turns_played, income),n=tail.n), "income")
  getSlopeDebug(tail(select(advisor.cnum, turns_played, taxes),n=tail.n), "taxes")
  getSlopeDebug(tail(select(advisor.cnum, turns_played, food),n=tail.n), "food")
  getSlopeDebug(tail(select(advisor.cnum, turns_played, foodnet),n=tail.n), "foodnet")
  getSlopeDebug(tail(select(advisor.cnum, turns_played, pop),n=tail.n), "pop")
  getSlopeDebug(tail(select(advisor.cnum, turns_played, land),n=tail.n), "land")
  getSlopeDebug(tail(select(advisor.cnum, turns_played, b_farm),n=tail.n), "farm")
  getSlopeDebug(tail(select(advisor.cnum, turns_played, b_res),n=tail.n), "res")
  getSlopeDebug(tail(select(advisor.cnum, turns_played, t_res),n=tail.n), "t_res")
  getSlopeDebug(tail(select(advisor.cnum, turns_played, b_lab),n=tail.n), "lab")
  par(old.par)
}

get.advisor <- function(cnum)
{
  cnum <<- cnum
  ac <- advisor(cnum)
  ac <- mutate(ac, taxrate=as.integer(taxrate), ps_tr=as.integer(ps_tr),
               ps_j=as.integer(ps_j), ps_ta=as.integer(ps_ta), cnum=as.integer(cnum),
               protection=as.integer(protection)) # bug
  server <- getServer()
  advisor.current <<- tbl_df(cbind(ac, distinct(select(server, -cnum_list)), setNames(tbl_dt(as.numeric(as.POSIXct(Sys.time()))), c('local.time'))))
  
  advisor.current <- mutate(advisor.current, countries_allowed=as.integer(countries_allowed))
  write.table(advisor.current, file="EE_History.csv", sep=",", append = TRUE)
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
