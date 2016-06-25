library(dplyr)

# This is a full sim of EE so that bots can train faster

Initialize.State <- function(id=0)
{
  state.Names <- c("id", 
                   "money", 
                   "tax.rate",
                   "government",
                   "food",
                   "oil",
                   "population",
                   "turns.left",
                   "turns.taken",
                   "turns.stored",
                   "enterprise.zones",
                   "residences.zones",
                   "industrial.zones",
                   "military.zones",
                   "research.zones",
                   "farms.zones",
                   "oil.zones",
                   "construction.zones",
                   "military.tech",
                   "medical.tech",
                   "business.tech",
                   "residential.tech",
                   "agricultural.tech",
                   "warfare.tech",
                   "military.strategy.tech",
                   "weapons.tech",
                   "industrial.tech",
                   "spy.tech",
                   "sdi.tech",
                   "spies.forces",
                   "troops.forces",
                   "jets.forces",
                   "turrets.forces",
                   "tanks.forces",
                   "nuclear.missiles.forces",
                   "chemical.missiles.forces",
                   "cruise.missiles.forces",
                   "at.war",
                   "gdi.member"
                   )
  
  state <- as.data.frame(t(as.data.frame(rep(0, length(state.Names)))))
  colnames(state) <- state.Names
  state$land <- 100
  state$population <- 1000
  state$money <- 25000
  state$at.war <- F
  state$gdi.member <- F
  state$tax.rate <- 35
  state$troops.forces <- 100
  state$id <- id
  state$food <- 100
  state$government <- "Monarchy"
  state <- tbl_dt(state)
  return(state)
}

Calc.Buildings <- function(state)
{
  state <- state %>% mutate(buildings = enterprise.zones +
                            residences.zones +
                            industrial.zones +
                            military.zones +
                            research.zones +
                            farms.zones +
                            oil.zones)
  return(state)
}

Calc.Empty.Land <- function(state)
{
  state <- Calc.Buildings(state)
  state <- state %>% mutate(empty.land = land - buildings)
  return(state)
}

Calc.Tech.Total <- function(state)
{
  state <- state %>% mutate(tech.total = military.tech +
                              medical.tech +
                              business.tech +
                              residential.tech +
                              agricultural.tech +
                              warfare.tech +
                              military.strategy.tech +
                              weapons.tech +
                              industrial.tech +
                              spy.tech +
                              sdi.tech
                              )
  return(state)
}

Calc.Missiles.Total <- function(state)
{
  state <- state %>% mutate(missles.total = nuclear.missiles.forces +
                              chemical.missiles.forces +
                              cruise.missiles.forces
                            )
  return(state)
}

Calc.Networth <- function(state)
{
  state <- state %>% mutate(networth = as.integer(
                              (troops.forces * 0.5) + 
                              (jets.forces * 0.6) + 
                              (turrets.forces * 0.6) + 
                              (tanks.forces * 2) + 
                              (spies.forces * 1) + 
                              (tech.total * 2) + 
                              (land * 45) + 
                              (buildings * 35) + 
                              (money / 20000) + 
                              (food/1000) + 
                              (missles.total * 2500) + 
                              (population/6) + 
                              (oil/100))
                            )
  return(state)
}

Calc.Construction.Cost <- function(state)
{
  state <- state %>% mutate(construction.cost = 3*land+1500)
  return(state)
}

Calc.Oil.Consumption <- function(state)
{
  state <- state %>% mutate(oil.consumption = (troops.forces + 
                                               jets.forces + 
                                               tanks.forces)/25)
  return(state)
}

Calc.Destruction.Cost <- function(state)
{
  state <- state %>% mutate(destruction.cost = 0.2 * (3 * land + 1500))
  return(state)
}

Calc.Food.Consumption <- function(state)
{
  state <- state %>% mutate(food.consumption = 
                            as.integer(
                              (population * 0.03) + 
                              (spies.forces * 0.005) + 
                              ((troops.forces + jets.forces + turrets.forces) * 0.001) + 
                              (tanks.forces * 0.003)   
                            )
                           )
  
  return(state)
}

Calc.Food.Produced <- function(state)
{
  state <- state %>% mutate(food.produced = 
                              as.integer(
                                (farms.zones * 5.3) + 
                                (empty.land * 0.4) * gov.food.production.bonus * 
                              agricultural.tech
                              )
                           )
  return(state)
}

Calc.Food.Decay <- function(state)
{
  state <- state %>% mutate(food.decay = 
                   as.integer((food - food.consumption + food.produced) / 1000)
                  )
  return(state)
}

Calc.Food <- function(state)
{
  state <- state %>% mutate(food = food + food.produced - (food.consumption + food.decay) )
  return(state)
}

Init.Gov.Bonus <- function(state)
{
  state <- state %>% mutate(gov.food.production.bonus = 1,
                            gov.strike.bonus = 1,
                            gov.oil.production.bonus = 1,
                            gov.pci.bonus = 1,
                            gov.population.bonus = 1,
                            gov.attack.gains.bonus = 1,
                            gov.attack.turns.bonus = 1,
                            gov.military.upkeep.costs.bonus = 1,
                            gov.military.strength.bonus = 1,
                            gov.spy.bonus = 1,
                            gov.construction.speed.bonus = 1,
                            gov.ghost.acres = 1,
                            gov.technology.bonus = 1,
                            gov.industrial.production.bonus = 1,
                            gov.market.sales.size.cap.bonus = 1,
                            gov.market.commision.bonus = 1,
                            gov.private.market.military.cost.bonus = 1,
                            gov.maximum.technology.bonus = 1,
                            gov.maximum.population.bonus = 1,
                            gov.gdi.bonus = 1,
                            gov.exploration.bonus = 1,
                            gov.maximum.pci.bonus = 1,
                            gov.market.commision.bonus = 1
                            )
  return(state)
}

Calc.PCI <- function(state)
{
  state <- state %>% mutate(pci = 22.5 * (1 - tax.rate) * 
                            (1 + ((networth/land)/90000)) * 
                            (1 + (2 * (enterprise.zones/land))) * 
                            business.tech * gov.pci.bonus  
  )
  return(state)
}

Calc.Revenue <- function(state)
{
  state <- mutate(revenue = pci * population * tax.rate)
  return(state)
}

Calc.Buildings.Per.Turn <- function(state)
{
  state <- state %>% mutate(buildings.per.turn = as.integer((construction.sites/4) + 5) * 
                              gov.construction.speed.bonus)
  
  return(state)
}

Calc.Tech.Per.Turn <- function(state)
{
  state <- mutate(tech.per.turn = round(0.17 * research.zones * 
                                          (1 + research.zones / land)) + 3)
  return(state)
}

Calc.Land.Upkeep <- function(state)
{
  state <- mutate(land.upkeep = ifelse (land < 1500, 
                                       (land ^ 2) * 7 / 1500 + land * 3, 
                                       land * 10))
  return(state)
}

Calc.Military.Upkeep <- function(state)
{
  state <- state %>% mutate(military.upkeep = ((troops.forces * .11) + (jets.forces * .14) + (turrets.forces * .18) + (tanks.forces * .57) + spies.forces) * military.tech * (1 + networth/40000000) * gov.military.upkeep.costs.bonus * 
                              min(0.61, 1 - 1.3 * (military.zones / land)))  
  return(state)
}


test.state <- Initialize.State()
test.state <- Init.Gov.Bonus(test.state)
test.state <- Calc.Buildings(test.state)
test.state <- Calc.Empty.Land(test.state)
test.state <- Calc.Tech.Total(test.state)
test.state <- Calc.Missiles.Total(test.state)
test.state <- Calc.Networth(test.state)
test.state <- Calc.Construction.Cost(test.state)
test.state <- Calc.Destruction.Cost(test.state)
test.state <- Calc.Oil.Consumption(test.state)
test.state <- Calc.Food.Produced(test.state)
test.state <- Calc.Food.Consumption(test.state)
test.state <- Calc.Food.Decay(test.state)
#test.state <- Calc.Food(test.state)

test.state$networth
test.state$construction.cost
test.state$food
test.state$food.produced
test.state$food.consumption
test.state$food.decay