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
    tax.rate == 0.35,
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
    enterprise.zones = 5,
    residences.zones = 94,
    industrial.zones = 5,
    military.zones = 5,
    research.zones = 5,
    farms.zones = 4110,
    oil.zones = 51, 
    construction.zones = 126,
    land = 8764
  )
  
  state <- Calc.Empty.Land(state)
  
  test <- state %>% filter(empty.land == 4363)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    stop("Unit Test Failed!  Test.Calc.Empty.Land")
    return(FALSE)
  }
}

Test.Calc.Tech.Total <- function()
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

Test.Calc.Networth <- function()
{
  state <- Initialize.State()
  state <- state %>% mutate(
    troops.forces = 1972,
      jets.forces = 1416,
      turrets.forces = 1682,
      tanks.forces = 1310,
      spies.forces = 94,
      tech.total = 454,
      land = 8764,
      buildings = (8764 - 4363),
      money = 18234169,
      food = 8548163,
      missles.total = 2,
      population = 107483,
      oil = 9690
  )
  
  state <- Calc.Networth(state)
  
  
  test <- state %>% filter(networth == 587350)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    print("Got")
    print(state$networth)
    print("Expected")
    print("587350")
    stop("Unit Test Failed!  Test.Calc.Networth")
    return(FALSE)
  }
  
  return(state)
}


Test.Calc.Construction.Cost <- function()
{
  state <- Initialize.State()
  state <- state %>% mutate(land = 8383)
  state <- Calc.Construction.Cost(state)
  
  test <- state %>% filter(construction.cost ==  26649)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    stop("Unit Test Failed!  Test.Calc.Construction.Cost")
    return(FALSE)
  }
}


Test.Calc.Oil.Consumption <- function()
{
  
  state <- Initialize.State()
  state <- state %>% mutate( troops.forces  = 3000, 
                                jets.forces  = 500, 
                                tanks.forces = 200  
  )
  
  state <- Calc.Oil.Consumption(state)
  
  test <- state %>% filter(oil.consumption ==  148)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    stop("Unit Test Failed!  Test.Calc.Construction.Cost")
    return(FALSE)
  }
}

Test.Calc.Destruction.Cost <- function()
{
  state <- Initialize.State()
  
  state <- state %>% mutate(land = 9333)
  
  state <- Calc.Destruction.Cost(state)
  
  test <- state %>% filter(destruction.cost == 5899)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    stop("Unit Test Failed!  Test.Calc.Destruction.Cost")
    return(FALSE)
  }
}


Test.Calc.Food.Consumption <- function()
{
  state <- Initialize.State()
  
  state <- state %>% mutate( population = 107417,
                             spies.forces = 93,
                             troops.forces = 1970,
                             jets.forces = 1414,
                             turrets.forces = 1680,
                             tanks.forces = 1310,
                             food = 8536342,
                             enterprise.zones = 5,
                             residences.zones = 94,
                             industrial.zones = 5,
                             military.zones = 5,
                             research.zones = 5,
                             farms.zones = 4110,
                             oil.zones = 51,
                             construction.zones = 126,
                             agricultural.tech = 1.0036,
                             land = 8764,
                             gov.food.production.bonus = 1
  )
  
  state <- Calc.Food.Consumption(state)
  test <- state %>% filter(food.consumption == 3232)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    stop("Unit Test Failed!  Test.Calc.Food.Consumption")
    return(FALSE)
  }
}

Test.Calc.Food.Produced <- function()
{
  state <- Initialize.State()
  state <- state %>% mutate( population = 107417,
                             spies.forces = 93,
                             troops.forces = 1970,
                             jets.forces = 1414,
                             turrets.forces = 1680,
                             tanks.forces = 1310,
                             food = 8536342,
                             enterprise.zones = 5,
                             residences.zones = 94,
                             industrial.zones = 5,
                             military.zones = 5,
                             research.zones = 5,
                             farms.zones = 4110,
                             oil.zones = 51,
                             construction.zones = 126,
                             agricultural.tech = 1.0036,
                             land = 8764,
                             gov.food.production.bonus = 1
  )
  
  state <- Calc.Food.Produced(state)

  test <- state %>% filter(food.produced == 23612)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    stop("Unit Test Failed!  Test.Calc.Food.Produced")
    return(FALSE)
  }
}

Test.Calc.Food.Decay <- function()
{
  state <- Initialize.State()

  state <- state %>% mutate( population = 107417,
                             spies.forces = 93,
                             troops.forces = 1970,
                             jets.forces = 1414,
                             turrets.forces = 1680,
                             tanks.forces = 1310,
                             food = 8536342,
                             enterprise.zones = 5,
                             residences.zones = 94,
                             industrial.zones = 5,
                             military.zones = 5,
                             research.zones = 5,
                             farms.zones = 4110,
                             oil.zones = 51,
                             construction.zones = 126,
                             agricultural.tech = 1.0036,
                             land = 8764,
                             gov.food.production.bonus = 1
  )
  
  state <- Calc.Food.Decay(state)
  
  test <- state %>% filter(food.decay == 8556)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    stop("Unit Test Failed!  Test.Calc.Food.Decay")
    return(FALSE)
  }
}

Test.Calc.Food <- function()
{
  state <- Initialize.State()
  
  state <- state %>% mutate( population = 107417,
                             spies.forces = 93,
                             troops.forces = 1970,
                             jets.forces = 1414,
                             turrets.forces = 1680,
                             tanks.forces = 1310,
                             food = 8536342,
                             enterprise.zones = 5,
                             residences.zones = 94,
                             industrial.zones = 5,
                             military.zones = 5,
                             research.zones = 5,
                             farms.zones = 4110,
                             oil.zones = 51,
                             construction.zones = 126,
                             agricultural.tech = 1.0036,
                             land = 8764,
                             gov.food.production.bonus = 1
  )
  
  state <- Calc.Food(state)
  
  test <- state %>% filter(food == 8548166)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    stop("Unit Test Failed!  Test.Calc.Food")
    return(FALSE)
  }
}


Test.Calc.PCI <- function()
{
  state <- Initialize.State()  
  
  state <- state %>% mutate(population = 107483,
                            spies.forces = 94,
                            troops.forces = 1972,
                            jets.forces = 1416,
                            turrets.forces = 1682,
                            tanks.forces = 1310,
                            food = 8548163,
                            enterprise.zones = 5,
                            residences.zones = 94,
                            industrial.zones = 5,
                            military.zones = 5,
                            research.zones = 5,
                            farms.zones = 4110,
                            oil.zones = 51,
                            construction.zones = 126,
                            agricultural.tech = 1.0036,
                            land = 8764,
                            money = 18234169,
                            gov.food.production.bonus = 1, 
                            gov.pci.bonus = 1,
                            tax.rate = 0.35, 
                            networth = 587350,
                            business.tech = 1
                            )
  
  
  state <- Calc.PCI(state)

  test <- state %>% filter(pci == 14.70)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    stop("Unit Test Failed!  Test.Calc.PCI")
    return(FALSE)
  }
}

Test.Calc.Revenue <- function()
{
  state <- Initialize.State()  
  state <- state %>% mutate(pci = 14.70,
                            population = 107483,
                            tax.rate = 0.35
                            )
  
  state <- Calc.Revenue(state)
  
  test <- state %>% filter(revenue == 553000)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    print("Expected")
    print(55300)
    print("Returned")
    print(test$revenue)
    stop("Unit Test Failed!  Test.Calc.Revenue")
    return(FALSE)
  }
}

Test.Calc.Buildings.Per.Turn <- function()
{
  state <- Initialize.State()  
  state <- state %>% mutate(construction.sites = 126,
                            gov.construction.speed.bonus = 1.4)
  
  state <- Calc.Buildings.Per.Turn(state)
  
  test <- state %>% filter(buildings.per.turn == 51)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    print("Expected")
    print(51)
    print("Returned")
    print(state$buildings.per.turn)
    stop("Unit Test Failed!  Test.Calc.Buildings.Per.Turn")
    return(FALSE)
  }
}

Test.Calc.Tech.Per.Turn <- function()
{
  state <- Initialize.State()  
  state <- state %>% mutate(research.zones = 5,
                  land = 8764)
  state <- Calc.Tech.Per.Turn(state)
  
  test <- state %>% filter(tech.per.turn == 4)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    print("Expected")
    print(4)
    print("Returned")
    print(test$tech.per.turn)
    stop("Unit Test Failed!  Test.Calc.Tech.Per.Turn")
    return(FALSE)
  }
}

Test.Calc.Land.Upkeep <- function()
{
  state <- Initialize.State()  
  state <- state %>% mutate(land = 120)
  state <- Calc.Land.Upkeep(state)
  
  test <- state %>% filter(land.upkeep == 427)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    print("Expected")
    print(427)
    print("Returned")
    print(state$land.upkeep)
    stop("Unit Test Failed!  Test.Calc.Land.Upkeep")
    return(FALSE)
  }
}

Test.Calc.Military.Upkeep <- function()
{
  state <- Initialize.State()  
  state <- state %>% mutate(troops.forces                   = 1972,
                            jets.forces                     = 1416, 
                            turrets.forces                  = 1682,
                            tanks.forces                    = 1310,
                            spies.forces                    = 94,
                            military.tech                   = 1,
                            networth                        = 587350,
                            gov.military.upkeep.costs.bonus = 1,
                            military.zones                  = 5,
                            land                            = 8764
                            )  
  state <- Calc.Military.Upkeep(state)
  
  test <- state %>% filter(military.upkeep == 1564)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    print("Expected")
    print(1564)
    print("Returned")
    print(state$military.upkeep)
    stop("Unit Test Failed!  Test.Calc.Military.Upkeep")
    return(FALSE)
  }
}



Test.Inialize.State()
Test.Calc.Buildings()
Test.Calc.Empty.Land()
Test.Calc.Tech.Total()
Test.Calc.Missiles.Total()
Test.Calc.Networth()
Test.Calc.Construction.Cost()
Test.Calc.Oil.Consumption()
Test.Calc.Destruction.Cost()
Test.Calc.Food.Produced()
Test.Calc.Food.Consumption()
Test.Calc.Food.Decay()
Test.Calc.Food()
Test.Calc.PCI()
Test.Calc.Revenue()
Test.Calc.Buildings.Per.Turn()
Test.Calc.Tech.Per.Turn()
Test.Calc.Land.Upkeep()
Test.Calc.Military.Upkeep()