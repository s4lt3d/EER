# Unit tests for ee_sim

source("ee_sim.R")

Test.Inialize.State <- function()
{
  state <- Initialize.State()
  
  test <- state %>% filter(
    land == 100,
    population == 1000,
    money == 25000,
    at.war == F,
    gdi.member == F,
    tax.rate == 35,
    troops.forces == 100,
    food == 100,
    government == "Monarchy",
    oil == 0,
    turns.left == 0,
    turns.taken == 0,
    turns.stored == 0,
    enterprise.zones == 0,
    residences.zones == 0,
    industrial.zones == 0,
    military.zones == 0,
    research.zones == 0,
    farms.zones == 0,
    oil.zones == 0,
    construction.zones == 0,
    military.tech == 0,
    medical.tech == 0,
    business.tech == 0,
    residential.tech == 0,
    agricultural.tech == 0,
    warfare.tech == 0,
    military.strategy.tech == 0,
    weapons.tech == 0,
    industrial.tech == 0,
    spy.tech == 0,
    sdi.tech == 0,
    spies.forces == 0,
    jets.forces == 0,
    turrets.forces == 0,
    tanks.forces == 0,
    nuclear.missiles.forces == 0,
    chemical.missiles.forces == 0,
    cruise.missiles.forces == 0
  )
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    stop("Unit Test Failed!  Test.Initialize.State")
    return(FALSE)
  }
}

Test.Calc.Buildings <- function()
{
  state <- Initialize.State()
  state <- state %>% mutate(
      enterprise.zones = 23,
      residences.zones = 9,
      industrial.zones = 43,
      military.zones = 27,
      research.zones = 62,
      farms.zones = 11,
      oil.zones = 21, 
      land = 1000
  )
  
  state <- Calc.Buildings(state)
  
  test <- state %>% filter(
    buildings == (23+9+43+27+62+11+21),
    land == 1000
  )
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    stop("Unit Test Failed!  Test.Calc.Buildings")
    return(FALSE)
  }
}

Test.Calc.Empty.Land <- function()
{
  state <- Initialize.State()
  state <- state %>% mutate(
    enterprise.zones = 23,
    residences.zones = 9,
    industrial.zones = 43,
    military.zones = 27,
    research.zones = 62,
    farms.zones = 11,
    oil.zones = 21, 
    land = 1000
  )
  
  state <- Calc.Empty.Land(state)
  
  test <- state %>% filter(empty.land == 804)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    stop("Unit Test Failed!  Test.Calc.Empty.Land")
    return(FALSE)
  }
}

Test.Calc.Tech.Total <- function(state)
{
  
  state <- Initialize.State()
  
  state <- state %>% mutate(military.tech = 222,
                              medical.tech = 243,
                              business.tech = 1023,
                              residential.tech = 29,
                              agricultural.tech = 56,
                              warfare.tech = 2023,
                              military.strategy.tech = 2123400,
                              weapons.tech = 20012,
                              industrial.tech = 4560,
                              spy.tech = 75440,
                              sdi.tech = 1111
                            )
  state <- Calc.Tech.Total(state)
  
  test <- state %>% filter(tech.total == (222 + 243 + 1023+ 29 + 56 + 2023 + 2123400 + 20012 + 4560 + 75440 + 1111))
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    stop("Unit Test Failed!  Test.Calc.Tech.Total")
    return(FALSE)
  }
}


Test.Calc.Missiles.Total <- function()
{
  state <- Initialize.State()
  
  state <- state %>% mutate(nuclear.missiles.forces = 6,
                              chemical.missiles.forces = 8,
                              cruise.missiles.forces = 23
  )
  state <- Calc.Missiles.Total(state)
  
  test <- state %>% filter(missles.total == (6 + 8 + 23))
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    stop("Unit Test Failed!  Test.Calc.Missiles.Total")
    return(FALSE)
  }
}

Test.Calc.Networth <- function(state)
{
  state <- Initialize.State()
  state <- state %>% mutate(
    troops.forces = 1876,
      jets.forces = 1320,
      turrets.forces = 1586,
      tanks.forces = 1310,
      spies.forces = 46,
      tech.total = 442,
      land = 8277,
      buildings = (8277 - 4029),
      money = 20893499,
      food = 7329195,
      missles.total = 2,
      population = 101738,
      oil = 4794
  )
  
  state <- Calc.Networth(state)
  print(state$networth)
  
  test <- state %>% filter(networth == 557753)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    stop("Unit Test Failed!  Test.Calc.Networth")
    return(FALSE)
  }
  
  return(state)
}


Test.Inialize.State()
Test.Calc.Buildings()
Test.Calc.Empty.Land()
Test.Calc.Tech.Total()
Test.Calc.Missiles.Total()
Test.Calc.Networth()