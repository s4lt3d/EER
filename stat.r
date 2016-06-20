normalit<-function(m){
  (m - min(m))/(max(m)-min(m))
}

getSlope <- function(table.df)
{
  if(length(table.df[[1]]) == 1){
    return(10000000)
  }
  
  table.df[[2]][is.nan( table.df[[2]])] <- 0
  lin.model <- lm(table.df[[2]]~table.df[[1]])
  if(is.na(as.numeric(coef(lin.model)[2])))
  {
    return(10000000)
  }
  return(as.numeric(coef(lin.model)[2]))
}

getSlopeDebug <- function(table.df, title)
{
  lin.model <- lm(table.df[[2]]~table.df[[1]])
  plot(table.df[[2]]~table.df[[1]], main=title)
  if(!is.na(as.numeric(coef(lin.model)[2])))
  {  
    abline(lin.model) 
  }
  return(coef(lin.model)[2])
}

decisionTable <- function(cnum=26)
{
  turns.money <- tail(distinct(select(filter(advisor.history, cnum==cnum), turns_played, money)), n=20)
  turns.money <- mutate(turns.money, money = normalit(money))
  money.slope <- getSlope(turns.money)

  turns.income <- tail(distinct(select(filter(advisor.history, cnum==cnum), turns_played, income)), n=20)
  turns.income <- mutate(turns.income, income = normalit(income))
  income.slope <- getSlope(turns.income)

  turns.taxes <- tail(distinct(select(filter(advisor.history, cnum==cnum), turns_played, taxes)), n=20)
  turns.taxes <- mutate(turns.taxes, taxes = normalit(taxes))
  taxes.slope <- getSlope(turns.taxes)

  turns.pop <- tail(distinct(select(filter(advisor.history, cnum==cnum), turns_played, pop)), n=20)
  turns.pop <- mutate(turns.pop, pop = normalit(pop))
  pop.slope <- getSlope(turns.pop)

  turns.pci <- tail(distinct(select(filter(advisor.history, cnum==cnum), turns_played, pci)), n=20)
  turns.pci <- mutate(turns.pci, pci = normalit(pci))
  pci.slope <- getSlope(turns.pci)

  turns.food <- tail(distinct(select(filter(advisor.history, cnum==cnum), turns_played, food)), n=20)
  turns.food <- mutate(turns.food, food = normalit(food))
  food.slope <- getSlope(turns.food)
  
  advisor.cnum <- tail(select(filter(advisor.history, cnum==cnum)), n=1)

  if(select(advisor.current, money) < 1000)
  {
    income.slope <- -10
  }
  
  if(select(advisor.current, food) < select(advisor.current, foodnet) * 2)
  {
    food.slope <- -9
  }

  building.needed <- 0
  
  if(select(advisor.current, empty) < select(advisor.current, bpt) * 4)
  {
    building.needed <- -8
  }
  
  cs.needed <- 0
  
  if(select(advisor.current, b_cs) < 80)
  {
    cs.needed <- -5
  }
  
  decision.table <- cbind('money', money.slope)
  decision.table <- rbind(decision.table, cbind('explore', building.needed))
  decision.table <- rbind(decision.table, cbind('b_cs', cs.needed))
  decision.table <- rbind(decision.table, cbind('income', income.slope))
  decision.table <- rbind(decision.table, cbind('taxes', taxes.slope))
  decision.table <- rbind(decision.table, cbind('pop', pop.slope))
  decision.table <- rbind(decision.table, cbind('pci', pci.slope))
  decision.table <- rbind(decision.table, cbind('food', food.slope))
  decision.table <- rbind(decision.table, cbind('none', -0.5))
  decision.table <- data.frame(decision.table)
  colnames(decision.table) <- c('type','weight')
  decision.table <- tbl_dt(decision.table)
  decision.table <- mutate(decision.table, weight, weight=as.numeric(levels(weight))[weight])
  decision.table <- decision.table %>% group_by(type) %>% arrange(weight, desc(weight))
  
  return(decision.table)
}
