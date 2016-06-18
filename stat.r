
getSlope <- function(table.df)
{
  lin.model <- lm(table.df[[2]]~table.df[[1]])
  return(as.numeric(coef(lin.model)[2]))
}

getSlopeDebug <- function(table.df, title)
{
  lin.model <- lm(table.df[[2]]~table.df[[1]])
  plot(table.df[[2]]~table.df[[1]], main=title)
  abline(lin.model)
  return(coef(lin.model)[2])
}

decisionTable <- function(history.table, cnum)
{
  turns.money <- tail(distinct(select(filter(history.table, cnum==cnum), turns_played, money)), n=20)
  money.slope <- getSlope(turns.money)
  #getSlopeDebug(turns.money, "money")
  
  turns.income <- tail(distinct(select(filter(history.table, cnum==cnum), turns_played, income)), n=20)
  income.slope <- getSlope(turns.income)
  #getSlopeDebug(turns.income, "income")
  
  turns.taxes <- tail(distinct(select(filter(history.table, cnum==cnum), turns_played, taxes)), n=20)
  taxes.slope <- getSlope(turns.taxes)
  #getSlopeDebug(turns.taxes, "taxes")
  
  turns.pop <- tail(distinct(select(filter(history.table, cnum==cnum), turns_played, pop)), n=20)
  pop.slope <- getSlope(turns.pop)
  #getSlopeDebug(turns.pop, "pop")
  
  turns.pci <- tail(distinct(select(filter(history.table, cnum==cnum), turns_played, pci)), n=20)
  pci.slope <- getSlope(turns.pci)
  #getSlopeDebug(turns.pci, "pci")
  
  turns.foodnet <- tail(distinct(select(filter(history.table, cnum==cnum), turns_played, foodnet)), n=20)
  foodnet.slope <- getSlope(turns.foodnet)
  #getSlopeDebug(turns.foodnet, "foodnet")
  
  decision.table <- cbind('money', money.slope)
  decision.table <- rbind(decision.table, cbind('income', income.slope))
  decision.table <- rbind(decision.table, cbind('taxes', taxes.slope))
  decision.table <- rbind(decision.table, cbind('pop', pop.slope))
  decision.table <- rbind(decision.table, cbind('pci', pci.slope))
  decision.table <- rbind(decision.table, cbind('foodnet', foodnet.slope))
  decision.table <- data.frame(decision.table)
  colnames(decision.table) <- c('type','slope')
  decision.table <- tbl_dt(decision.table)
  decision.table <- decision.table %>% group_by(type) %>% arrange(slope, desc(slope))
  
  return(decision.table)
}
