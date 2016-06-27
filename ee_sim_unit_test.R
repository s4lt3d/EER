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
    taxrate == 0.35,
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
    food                     = 11711956,
    money                    = 154913855,
    enterprise.zones         = 4,
    residences.zones         = 1842,
    industrial.zones         = 4,
    military.zones           = 4,
    research.zones           = 1704,
    farms.zones              = 3464,
    oil.zones                = 43,
    construction.zones       = 108,
    military.tech            = 11000,
    medical.tech             = 6000,
    business.tech            = 6000,
    residential.tech         = 6000,
    agricultural.tech        = 6000,
    warfare.tech             = 6000,
    military.strategy.tech   = 6000,
    weapons.tech             = 6000,
    industrial.tech          = 6000,
    spy.tech                 = 6000,
    sdi.tech                 = 6000,
    spies.forces             = 530,
    troops.forces            = 2594,
    jets.forces              = 2116,
    turrets.forces           = 2345,
    tanks.forces             = 1126,
    nuclear.missiles.forces  = 0,
    chemical.missiles.forces = 2,
    cruise.missiles.forces   = 0, 
    land                     = 10017,
    population               = 118453,
    networth                 = 895333, 
    oil                      = 55924
  )
  
  state <- Calc.Networth(state)
  
  
  test <- state %>% filter(networth == 895333)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    print("Got")
    print(state$networth)
    print("Expected")
    print("895333")
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
                            taxrate = 0.35, 
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
                            taxrate = 0.35
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
  state <- state %>% mutate(construction.zones = 126,
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

Test.Calc.Change.Government <- function()
{

  state <- Initialize.State()  
  state <- state %>% mutate(food                     = 11907444,
                            money                    = 79851821,
                            enterprise.zones         = 5,
                            residences.zones         = 2185,
                            industrial.zones         = 5,
                            military.zones           = 5,
                            research.zones           = 821,
                            farms.zones              = 4110,
                            oil.zones                = 51,
                            construction.zones       = 126,
                            military.tech            = 20,
                            medical.tech             = 30,
                            business.tech            = 2856,
                            residential.tech         = 3140,
                            agricultural.tech        = 253,
                            warfare.tech             = 9,
                            military.strategy.tech   = 8,
                            weapons.tech             = 7,
                            industrial.tech          = 6,
                            spy.tech                 = 5,
                            sdi.tech                 = 4,
                            spies.forces             = 355,
                            troops.forces            = 2494,
                            jets.forces              = 1938,
                            turrets.forces           = 2204,
                            tanks.forces             = 1310,
                            nuclear.missiles.forces  = 1,
                            chemical.missiles.forces = 1,
                            cruise.missiles.forces   = 1, 
                            land                     = 9997)
   state <- Calc.Networth(state)
   state <- Calc.Change.Government(state)
  
   test <- state %>% filter(food                     == 10240401,
                            money                    == 68672566,
                            enterprise.zones         == 4,
                            residences.zones         == 1879,
                            industrial.zones         == 4,
                            military.zones           == 4,
                            research.zones           == 706,
                            farms.zones              == 3534,
                            oil.zones                == 43,
                            construction.zones       == 108,
                            military.tech            == 17,
                            medical.tech             == 25,
                            business.tech            == 2456,
                            residential.tech         == 2700,
                            agricultural.tech        == 217,
                            warfare.tech             == 7,
                            military.strategy.tech   == 6,
                            weapons.tech             == 6,
                            industrial.tech          == 5,
                            spy.tech                 == 4,
                            sdi.tech                 == 3,
                            spies.forces             == 305,
                            troops.forces            == 2144,
                            jets.forces              == 1666,
                            turrets.forces           == 1895,
                            tanks.forces             == 1126,
                            nuclear.missiles.forces  == 0,
                            chemical.missiles.forces == 0,
                            cruise.missiles.forces   == 0
                           
                           )
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    stop("Unit Test Failed!  Test.Calc.Change.Military")
    return(FALSE)
  }
}


# Very unverified
Test.Calc.Tech.Percentage <- function(state)
{
  state <- Initialize.State()  
  
  state <- state %>% mutate(military.tech            = 11000,
                            medical.tech             = 6000,
                            business.tech            = 6000,
                            residential.tech         = 6000,
                            agricultural.tech        = 6000,
                            warfare.tech             = 6000,
                            military.strategy.tech   = 6000,
                            weapons.tech             = 6000,
                            industrial.tech          = 6000,
                            spy.tech                 = 6000,
                            sdi.tech                 = 6000,
                            land                     = 10017, 
                            government               = "C")
  
  state <- Calc.Tech.Percentage(state)
  
  test <- state %>% filter(business.tech.per == 1.08009,
                           residential.tech.per == 1.08009, 
                           agricultural.tech.per == 1.13014, 
                           sdi.tech.per == 0.099098,
                           medical.tech.per == 0.953504, 
                           military.tech.per == 0.966271,
                           warfare.tech.per == 0.00680529,
                           military.strategy.tech.per == 1.04004,
                           weapons.tech.per == 1.05006,
                           industrial.tech.per == 1.06007,
                           spy.tech.per == 1.05006 )
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    stop("Unit Test Failed!  Test.Calc.Change.Military")
    return(FALSE)
  }
}

Test.Calc.Total.Expense <- function()
{
  
  state <- Initialize.State()
  
  state <- state %>% mutate(military.upkeep = 1763, 
                            land.upkeep = 99970
                           )
  
  state <- Calc.Total.Expense(state)
  
  test <- state %>% filter(total.expense == 101733)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    stop("Unit Test Failed!  Test.Calc.Total.Expense")
    return(FALSE)
  }
}

Test.Calc.Explore.Rate <- function()
{
  
  state <- Initialize.State()
  
  state <- state %>% mutate(land = 10017)
  
  state <- Calc.Explore.Rate(state)
  
  test <- state %>% filter(explore.rate == 11)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    print("Expected")
    print(11)
    print("Returned")
    print(state$explore.rate)
    stop("Unit Test Failed!  Test.Calc.Explore.Rate")
    return(FALSE)
  }
}



Test.Calc.Max.Population <- function()
{
  state <- Initialize.State()
  
  state <- state %>% mutate(land = 160, 
                            taxrate = 0.35,
                            residences.zones = 32, 
                            residential.tech = 0, 
                            government = "M", 
                            population = 1715
  )
  
  state <- Calc.Tech.Percentage(state)
  state <- Calc.Max.Population(state)  
  
  test <- state %>% filter(max.population == 1794)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    print("Expected")
    print(118532)
    print("Returned")
    print(state$max.population)
    stop("Unit Test Failed!  Test.Calc.Max.Population")
    return(FALSE)
  }
}



Test.Calc.Population.Growth <- function()
{
  state <- Initialize.State()
  
  state <- state %>% mutate(land = 160, 
                            taxrate = 0.35,
                            residences.zones = 32, 
                            residential.tech = 0, 
                            government = "M", 
                            population = 1715
  )
  
  state <- Calc.Population.Growth(state)  
  
  test <- state %>% filter(population.growth == 26)
  
  if(tally(test) == 1) 
  {
    return(TRUE)
  } else {
    print("Expected")
    print(26)
    print("Returned")
    print(state$population.growth)
    stop("Unit Test Failed!  Calc.Population.Growth")
    return(FALSE)
  }
}

# Test.Inialize.State()
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
Test.Calc.Change.Government()
Test.Calc.Tech.Percentage()
Test.Calc.Total.Expense()
Test.Calc.Explore.Rate()
Test.Calc.Max.Population()
Test.Calc.Population.Growth()
