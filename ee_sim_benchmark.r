#source('ee_sim.R')


teststate <- Initialize.State(9999)
loops <- 500

timeStateFunc <- function(FUN)
{
  system.time(
    for(i in 1:loops)
    {
      FUN(teststate)
    }
  )
}

timeStateFunc(Update.State)

timeStateFunc(Calc.Buildings)
timeStateFunc(Calc.Empty.Land)
timeStateFunc(Calc.Tech.Total)
timeStateFunc(Calc.Missiles.Total)
timeStateFunc(Calc.Networth)
timeStateFunc(Calc.Construction.Cost)

timeStateFunc(Calc.Destruction.Cost)
timeStateFunc(Calc.Oil.Consumption)
timeStateFunc(Calc.Food.Consumption)
timeStateFunc(Calc.Food.Produced)
timeStateFunc(Calc.Food.Decay)
timeStateFunc(Calc.Food)
timeStateFunc(Calc.Food.Net)
timeStateFunc(Init.Gov.Bonus)
timeStateFunc(Calc.Net.Income)
timeStateFunc(Calc.Cashing)
timeStateFunc(Calc.PCI)
timeStateFunc(Calc.Revenue)
timeStateFunc(Calc.Buildings.Per.Turn)
timeStateFunc(Calc.Tech.Per.Turn)
timeStateFunc(Calc.Land.Upkeep) 
timeStateFunc(Calc.Military.Upkeep)
timeStateFunc(Calc.Change.Government)
timeStateFunc(Calc.Tech.Percentage)
timeStateFunc(Calc.Total.Expense)
timeStateFunc(Calc.Explore.Rate)
timeStateFunc(Calc.Max.Population)
timeStateFunc(Calc.Population.Growth)

timeStateFunc(Manage.End.Turn)
timeStateFunc(Build)
timeStateFunc(Explore)




system.time({
  r <- 0
  for(i in 1:loops)
  {
    r <- r + i * 2
  }
}
)
