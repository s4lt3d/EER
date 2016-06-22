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



test.state <- Initialize.State()
test.state <- Calc.Buildings(test.state)
test.state <- Calc.Empty.Land(test.state)
test.state <- Calc.Tech.Total(test.state)
test.state <- Calc.Missiles.Total(test.state)
test.state <- Calc.Networth(test.state)
test.state <- Calc.Construction.Cost(test.state)
test.state <- Calc.Destruction.Cost(test.state)
test.state <- Calc.Oil.Consumption(test.state)
test.state$networth
test.state$construction.cos