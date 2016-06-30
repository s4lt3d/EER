library("plyr")
library("dplyr") 
library("jsonlite") 
library("httr") 
library("randomNames") 
library("tidyr")
library("stats")
#library("RSQLite")
options(warn=-2)

#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)

source('web.r') # contains all the code for getting data in and out of the server
#source('stat.r')
#source('advisor.R')
#source('game_functions.r')

source('explore_rates.R')
# This is a full sim of EE so that bots can train faster

# TODO 
# Rebuilding Cost ?
# Alliance Upkeep
# GDI Penalty
# SPAL
# Standard Strike
# Planned Strike
# Helper function
# keep a number between two numbers elementwise

pconstrain <- function( x, r1, r2 )
{
  if(length(x) != length(r1) | length(x) != length(r2)){ stop ('Must be the same length') }
  
  lower.bounds <- pmin(r1, r2)
  upper.bounds <- pmax(r1, r2)
  
  return(pmin(pmax(x, lower.bounds), upper.bounds))
}

Initialize.State <- function(cnum=0)
{
  state.Names <- c("cnum", "money", "taxrate", "government", "food", "oil", "population", 
                   "turns.left", "turns.taken", "turns.stored", "enterprise.zones", 
                   "residences.zones", "industrial.zones", "military.zones", "research.zones", 
                   "farms.zones", "oil.zones", "construction.zones", "military.tech", 
                   "medical.tech", "business.tech", "residential.tech", "agricultural.tech", 
                   "warfare.tech", "military.strategy.tech", "weapons.tech", "industrial.tech", 
                   "spy.tech", "sdi.tech", "spies.forces", "troops.forces", "jets.forces", 
                   "turrets.forces", "tanks.forces", "nuclear.missiles.forces", 
                   "chemical.missiles.forces", "cruise.missiles.forces", "at.war", "gdi.member", 
                   "turns", "success", "pci", "pci.growth", "food.net", "oil.production", 
                   "net.income.prev", "net.income", "land", "gov.food.production.bonus", 
                   "gov.strike.bonus", "gov.oil.production.bonus", "gov.pci.bonus", 
                   "gov.population.bonus", "gov.attack.gains.bonus", "gov.attack.turns.bonus", 
                   "gov.military.upkeep.costs.bonus", "gov.military.strength.bonus", 
                   "gov.spy.bonus", "gov.construction.speed.bonus", "gov.ghost.acres", 
                   "gov.technology.bonus", "gov.industrial.production.bonus", 
                   "gov.market.sales.size.cap.bonus", "gov.market.commision.bonus", 
                   "gov.private.market.military.cost.bonus", "gov.maximum.technology.bonus", 
                   "gov.maximum.population.bonus", "gov.gdi.bonus", "gov.exploration.bonus", 
                   "gov.maximum.pci.bonus", "tech.total", "buildings", "missles.total", 
                   "networth", "business.tech.per", "residential.tech.per", 
                   "agricultural.tech.per", "sdi.tech.per", "medical.tech.per", 
                   "military.tech.per", "warfare.tech.per", "military.strategy.tech.per", 
                   "weapons.tech.per", "industrial.tech.per", "spy.tech.per", "empty.land", 
                   "construction.cost", "destruction.cost", "oil.consumption", "food.consumption", 
                   "food.produced", "food.decay", "max.population", "population.growth", 
                   "revenue", "buildings.per.turn", "tech.per.turn", "land.upkeep", 
                   "milcost", "military.upkeep", "total.expense", "explore.rate", "cashing", 
                   "troops.upkeep", "spies.upkeep", "jets.upkeep", "turrets.upkeep", "tanks.upkeep", "rank")
  
  state <- as.data.frame(t(as.data.frame(rep(0, length(state.Names)))))
  colnames(state) <- state.Names
  state$land <- 120
  state$population <- 1000
  state$money <- 25000
  state$at.war <- F
  state$gdi.member <- F
  state$taxrate <- 0.35
  state$troops.forces <- 100
  state$cnum <- cnum
  state$food <- 100
  state$government <- "M"
  state$success <= TRUE
  state$pci <- 20
  state$pci.growth = 0
  state <- tbl_dt(state)
  state <- Init.Gov.Bonus(state)
  state <- Update.State(state)
  state$net.income <- 6562
  state$net.income.prev <- 6562
  state$pci <- 20
  state$pci.growth = 0
  state$total.expense <- 438
  state$revenue <- 7000
  state$food.produced <- 48
  state$food.consumption <- 30
  state$food.net <- 18
  
  return(state)
}

Update.State <- function(state)
{
  state <- Calc.Networth(state)
  state <- Calc.Buildings(state)
  state <- Calc.Tech.Total(state)
  state <- Calc.Tech.Percentage(state)
  state <- Calc.Empty.Land(state)
  state <- Calc.Missiles.Total(state)
  state <- Calc.Construction.Cost(state)
  state <- Calc.Destruction.Cost(state)
  state <- Calc.Oil.Consumption(state)
  state <- Calc.Food.Consumption(state)
  state <- Calc.Food.Produced(state)
  state <- Calc.Food.Decay(state)
  state <- Calc.PCI(state)
  state <- Calc.Revenue(state)
  state <- Calc.Buildings.Per.Turn(state)
  state <- Calc.Tech.Per.Turn(state)
  state <- Calc.Land.Upkeep(state) 
  state <- Calc.Military.Upkeep(state)
  state <- Calc.Total.Expense(state)
  state <- Calc.Explore.Rate(state)
  state <- Calc.Max.Population(state)
  state <- Calc.Population.Growth(state)
  state <- Calc.Net.Income(state)
  state <- Calc.Cashing(state)
  return(state)
}

#Sync state with web game
Sync.State <- function(state)
{
  state$success <- FALSE
  adv           <- advisor(state$cnum)
  if(is.null(adv))
    return(state)

  state$government              <- adv$govt
  state$networth                <- adv$networth
  state$land                    <- adv$land
  state$empty.land              <- adv$empty
  state$money                   <- adv$money
  state$population              <- adv$pop
  state$pci                     <- adv$pci
  state$food                    <- adv$food
  state$food.net                <- adv$foodnet
  state$food.consumption        <- adv$foodcon
  state$oil                     <- adv$oil
  state$oil.production          <- adv$oilpro
  state$enterprise.zones        <- adv$b_ent
  state$residences.zones        <- adv$b_res
  state$military.zones          <- adv$b_mb
  state$industrial.zones        <- adv$b_indy
  state$farms.zones             <- adv$b_farm
  state$research.zones          <- adv$b_lab
  state$construction.zones      <- adv$b_cs
  state$spies.forces            <- adv$m_spy
  state$troops.forces           <- adv$m_tr
  state$jets.forces             <- adv$m_j
  state$turrets.forces          <- adv$m_tu
  state$tanks.forces            <- adv$m_ta

  state$military.tech           <- adv$t_mil
  state$medical.tech            <- adv$t_med
  state$business.tech           <- adv$t_bus
  state$residential.tech        <- adv$t_res
  state$industrial.tech         <- adv$t_indy
  state$agricultural.tech       <- adv$t_agri
  state$warfare.tech            <- adv$t_war
  state$weapons.tech            <- adv$t_weap
  state$military.strategy.tech  <- adv$t_ms
  state <- Calc.Tech.Percentage(state) # better resolution than adv
  state$tech.total              <- adv$t_tot
  
  state$tech.per.turn           <- adv$tpt
  state$land.upkeep             <- adv$expenses_land
  state$military.upkeep         <- adv$expenses_mil
  state$troops.upkeep           <- adv$expense_tr
  state$jets.upkeep             <- adv$expense_j
  state$spies.upkeep            <- adv$expense_spy
  state$turrets.upkeep          <- adv$expense_tu
  state$tanks.upkeep            <- adv$expense_ta
  state$revenue                 <- adv$taxes
  state$cashing                 <- adv$cashing
  state$government              <- adv$govt
  state$taxrate                 <- as.numeric(adv$taxrate) / 100
  state$net.income              <- adv$income
  state$turns.stored            <- adv$turns_stored
  state$turns.taken             <- adv$turns_played
  state$turns.left              <- adv$turns
  state$buildings.per.turn      <- adv$bpt
  state$explore.rate            <- adv$explore_rate

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
                            oil.zones + 
                            construction.zones)
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
  state <- Calc.Tech.Total(state)
  state <- Calc.Buildings(state)
  state <- Calc.Missiles.Total(state)
  state <- state %>% mutate(networth = 
      round((troops.forces * 0.5) + 
        (jets.forces * 0.6) + 
        (turrets.forces * 0.6) + 
        (tanks.forces * 2) + 
        (spies.forces * 1)) + 
      (tech.total * 2) + 
      (land * 45) + 
      (buildings * 35) + 
      as.integer((money / 20000)) + 
      as.integer((food/1000)) + 
      as.integer(missles.total * 2500) + 
      as.integer(as.integer(population/6)) + 
      as.integer(oil/100))
                            
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
  state <- state %>% mutate(destruction.cost = as.integer(0.2 * (3 * land + 1500)))
  return(state)
}

Calc.Food.Consumption <- function(state)
{
  state <- state %>% mutate(food.consumption = 
                            round(
                              (population * 0.03) + 
                              (spies.forces * 0.005) + 
                              ((troops.forces + jets.forces + turrets.forces) * 0.001) + 
                              (tanks.forces * 0.003)   
                            )
                           )
  
  return(state)
}

#Different than wiki
Calc.Food.Produced <- function(state)
{
  state <- Calc.Empty.Land(state)
  
  state <- state %>% mutate(food.produced = 
                              ceiling(
                                ((farms.zones * 5.3) + 
                                (empty.land * 0.4)) * gov.food.production.bonus * 
                              agricultural.tech.per
                              )
                           )
  return(state)
}

Calc.Food.Decay <- function(state)
{
  state <- Calc.Food.Consumption(state)
  state <- Calc.Food.Produced(state)
  state <- state %>% mutate(food.decay = 
                   as.integer((food - food.consumption + food.produced) / 1000)
                  )
  return(state)
}

Calc.Food <- function(state)
{
  state <- Calc.Food.Consumption(state)
  state <- Calc.Food.Produced(state)
  state <- Calc.Food.Decay(state)
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

Calc.Net.Income <- function(state) {
  state <- state %>% mutate(net.income.prev = net.income)
  state <- state %>% mutate(net.income = round(revenue - total.expense))
  return(state)
}

Calc.Cashing <- function(state) {
  state <- state %>% mutate(cashing = round(revenue * 1.2 - total.expense))
  return(state)
}

Calc.PCI <- function(state)
{
  state <- Calc.Population.Growth(state)
  
  #state <- state %>% mutate(pci = round(
  #  22.5 * (1 - taxrate) * (1 + ((networth/land)/18000)) * (1 + (2 * (enterprise.zones/land))) * business.tech * gov.pci.bonus, 2)  
  #)
  
  denominator = 18000
  maxPCI=round(22.5*(1-state$taxrate)*(1+(state$networth/state$land)/denominator)*(1+2*state$enterprise.zones/state$land)*state$gov.pci.bonus*state$business.tech.per, 2)
  pciA=(1+0.03*(1-state$taxrate))*(1-0.5*state$population.growth/(state$population.growth+state$population))*state$pci-state$pci
  pciB=(0.035+0.3*state$taxrate)*state$pci
  pciC=maxPCI-state$pci
  
  if (maxPCI>=state$pci) {
    state$pci.growth <- min(pciA, pciC);
  } else {
    state$pci.growth <- -1*min(pciB, max(-1*pciA, -1*pciC));
  }
  
  state$pci <- state$pci + state$pci.growth
  
  return(state)
}

Calc.Revenue <- function(state)
{
  state <- state %>% mutate(revenue = round(round(pci * population) * taxrate))
  return(state)
}

Calc.Buildings.Per.Turn <- function(state)
{
  state <- state %>% mutate(buildings.per.turn = round(((construction.zones/4) + 5) * 
                              gov.construction.speed.bonus))
  
  return(state)
}

Calc.Tech.Per.Turn <- function(state)
{
  state <- state %>% mutate(tech.per.turn = as.integer(round(0.17 * research.zones * 
                                          (1 + research.zones / land)) + 3))
  return(state)
}

Calc.Land.Upkeep <- function(state)
{
  state <- state %>% mutate(land.upkeep = ifelse (land < 1500, 
                                       as.integer((land ^ 2) * 7 / 1500 + land * 3), 
                                       land * 10))
  return(state)
}

# different than wiki
Calc.Military.Upkeep <- function(state)
{
  state <- state %>% mutate(milcost = 
                   ifelse(military.zones / land < 0.295, 
                          1-1.3*round(military.zones/land, 2), 0.61))

  state <- state %>% mutate(military.upkeep = 
                              round(
                                round((troops.forces * .11) + 
         (jets.forces * .14) + 
         (turrets.forces * .18) + 
         (tanks.forces * .57) + 
         spies.forces * 1) * 
       military.tech.per * (1 + networth / 200000000) * 
       gov.military.upkeep.costs.bonus * 
        milcost
      ))  
  
  return(state)
}


Calc.Change.Government <- function(state)
{
  loss <- 1 - 0.14;

  if(state$networth > 12000000 & state$networth < 30000000){
    loss <- 1 - (0.14 + 0.25 * (state$networth - 12000000) / 18000000)
  }
  if(state$networth >= 30000000) {
    loss <- 1 - 0.39
  }
  
  
  state <- state %>% mutate(food                     = as.integer(food * loss),
                            money                    = as.integer(money * loss),
                            enterprise.zones         = as.integer(enterprise.zones * loss),
                            residences.zones         = as.integer(residences.zones * loss),
                            industrial.zones         = as.integer(industrial.zones * loss),
                            military.zones           = as.integer(military.zones * loss),
                            research.zones           = as.integer(research.zones * loss),
                            farms.zones              = as.integer(farms.zones * loss),
                            oil.zones                = as.integer(oil.zones * loss),
                            construction.zones       = as.integer(construction.zones * loss),
                            military.tech            = as.integer(military.tech * loss),
                            medical.tech             = as.integer(medical.tech * loss),
                            business.tech            = as.integer(business.tech * loss),
                            residential.tech         = as.integer(residential.tech * loss),
                            agricultural.tech        = as.integer(agricultural.tech * loss),
                            warfare.tech             = as.integer(warfare.tech * loss),
                            military.strategy.tech   = as.integer(military.strategy.tech * loss),
                            weapons.tech             = as.integer(weapons.tech * loss),
                            industrial.tech          = as.integer(industrial.tech * loss),
                            spy.tech                 = as.integer(spy.tech * loss),
                            sdi.tech                 = as.integer(sdi.tech * loss),
                            spies.forces             = as.integer(spies.forces * loss),
                            troops.forces            = as.integer(troops.forces * loss),
                            jets.forces              = as.integer(jets.forces * loss),
                            turrets.forces           = as.integer(turrets.forces * loss),
                            tanks.forces             = as.integer(tanks.forces * loss),
                            nuclear.missiles.forces  = as.integer(nuclear.missiles.forces * loss),
                            chemical.missiles.forces = as.integer(chemical.missiles.forces * loss),
                            cruise.missiles.forces   = as.integer(cruise.missiles.forces * loss)
                          )
  return(state)
}


# Military	        8000	  97.5%
# Medical	          3000	  97.6%
# Business        	3000	 104.1%
# Residential	      3000	 104.1%
# Agricultural	    3000	 106.7%
# Warfare	          3000	 0.447%
# Military Strategy	3000	 102.1%
# Weapons	          3000	 102.6%
# Industrial	      3000	 103.1%
# Spy	              3000	 102.6%
# SDI	              3000 	  5.57%
# land              9997  


# Military	        11,000	96.6%
# Medical	          6000	  95.4%
# Business	        6000	 108.0%
# Residential	      6000	 108.0%
# Agricultural	    6000	 113.0%
# Warfare	          6000	 0.681%
# Military Strategy	6000	 104.0%
# Weapons	          6000	 105.0%
# Industrial	      6000	 106.0%
# Spy	              6000	 105.0%
# SDI	              6000	 9.91%


#        $c = ($country ? $country : $this->country);
#$c1 = 192;
#$c2 = 6.795;
#$gvtmax = ($c['govt'] == 'H' ? ($this->govt_version > 0 ? 0.65 : 0.5) : ($c['govt'] == 'D' && $this->govt_version > 0 ? 1.1 : 1.0));
#$g_t_eff = ($c['govt'] == 'C' ? 1.2 : 1);
#return ($baseTech+($maxTech-$baseTech)*$gvtmax*(1-exp(-1*$g_t_eff*$c[$tech_handle]/($c1+$c2*$c['land']))))/100;     ///----updated to return actual percentage

Calc.Tech.Percentage <- function(state)
{
  techstate <- state %>% select(business.tech, residential.tech, agricultural.tech, sdi.tech,
                                medical.tech, military.tech, warfare.tech, military.strategy.tech, 
                                weapons.tech, industrial.tech, spy.tech )
  gov.eff  <- rep(1, 11)
  gov.tech <- rep(1, 11)
                # x      x      x              x        x         x       x        x      x      x  
                # bus    res    agri   sdi     med       mil       war     mil.str  weap   indy   spy
  base <-       c(1,     1,     1,     0.01,   1,        1,        0.002,  1,       1,     1,     1)
  max.tech <-   c(1.8,   1.8,   2.3,   0.9,    0.66666,  0.83333,  0.05,   1.4,     1.5,   1.6,   1.5)
  C1 <-         c(192,   192,   192,   192,    1650,     780,      192,    192,     192,   192,   192)
  C2 <-         c(6.795, 6.795, 6.795, 6.795,  4.62,     5.75,     6.795,  6.795,   6.795, 6.795, 6.795)

  if (state$government == "C") {
    gov.eff  <- rep(1.2, 11)
  }
  if (state$government == "D") {
    gov.tech <- rep(1.1, 11)
    max.tech <- c(1.88,  1.88,  0.243, 0.989,  0.633333, 0.816666, 0.055,  1.44,    1.55,  1.66,  1.55)
  }
  if (state$government == "H") {
    gov.tech <- rep(0.65, 11)
    max.tech <- c(1.52,  1.52,  1.845, 0.5885, 0.783333, 0.8916,   0.0332, 1.26,    1.325, 1.39,  1.325)
  }
  
  land <- rep(state$land, 11)
  ones <- rep(1, 11)
  
  m1 <- as.matrix(cbind(C1, C2))
  m2 <- as.matrix(rbind(ones, land))

  tech.per <- signif(base + (max.tech - base ) * gov.tech * (1 - exp(-1 * gov.eff * techstate / (m1 %*% m2)[,1])),6)
  tech.per <- pconstrain(tech.per, max.tech, base)

  state <- state %>% mutate(business.tech.per = tech.per$business.tech,
                            residential.tech.per = tech.per$residential.tech,
                            agricultural.tech.per = tech.per$agricultural.tech,
                            sdi.tech.per = tech.per$sdi.tech,
                            medical.tech.per = tech.per$medical.tech,
                            military.tech.per = tech.per$military.tech,
                            warfare.tech.per = tech.per$warfare.tech,
                            military.strategy.tech.per = tech.per$military.strategy.tech,
                            weapons.tech.per = tech.per$weapons.tech,
                            industrial.tech.per = tech.per$industrial.tech,
                            spy.tech.per = tech.per$spy.tech
                           )
  return(state)
}

Calc.Total.Expense <- function(state)
{
  state <- Calc.Military.Upkeep(state)
  state <- state %>% mutate(total.expense = military.upkeep + land.upkeep)
  
  return(state)
}

Calc.Explore.Rate <- function(state)
{
  explorerate <- max(select(filter(explore.rates, land > state$land, gov == "O"), rate))
  
  if(state$government == 'R'){
    explorerate <- max(select(filter(explore.rates, land > state$land, gov == "R"), rate))
  }
  
  state <- state %>% mutate(explore.rate = explorerate)
  
  return(state)
}


Calc.Population.Growth <- function(state) {
  
  BioFactor <- 1
  
  state <- Calc.Tech.Percentage(state)
  state <- Calc.Max.Population(state)
  
  
  state <- state %>% mutate(population.growth = ifelse(max.population > population, 
                                                       round( 
                                                         min(
                                                           (max.population - population) / 3, 
                                                           max( 40,
                                                                0.03 * taxrate * population
                                                           )
                                                         )
                                                       )
                                                       ,
                                                       floor(  -1 * min( (0.05 + 0.15 * taxrate) * population, 
                                                                         (population - max.population) / 3) 
                                                       )
  )
  )
  
  
  return(state)
}

Calc.Oil.Production <- function(state)
{
  state <- state %>% mutate(oil.production = oil.zones * 2 * gov.oil.production.bonus)
  return(state)
}

Calc.Food.Net <- function(state){
  
  state <- state %>% mutate(food.net = food.produced - (food.consumption + food.decay))
  return(state)
}

Calc.Max.Population <- function(state)
{
  state <- state %>% mutate(max.population = 
                              round(
                                (1 - 0.95 * taxrate) * (24 * residences.zones + 12 * land) * 
                                  residential.tech.per * 1 * 1))
  
  
  #  (1-0.95*$this->tax)*(24*$this->country['b_res']+12*$this->country['land'])*$this->g_pop*$this->getTechPercent('t_res')
  
  return(state)
}

Manage.End.Turn <- function(state, cashing = F){
  
  state <- Calc.Food.Decay(state)
  state <- Calc.Food.Produced(state)
  state <- Calc.Food.Consumption(state)
  state <- Calc.Food.Net(state)
  state$food <- state$food + state$food.net
  
  state <- Calc.Oil.Production(state)
  
  state <- Calc.PCI(state)
  state <- Calc.Population.Growth(state)
  
  
  
  state$population <- state$population + state$population.growth
  state <- Calc.Revenue(state)
  
  
  state$turns.taken <- state$turns.taken + 1
  
  state <- Calc.Net.Income(state)
  state <- Calc.Cashing(state)
  
  
  if(cashing == F){
    state$money <- as.integer(state$money + state$net.income)
  } else {
    state$money <- as.integer(state$money + state$revenue * 1.2 - state$total.expense)
  }
  
  
  return(state)
}

Build <- function(state, enterprize=0, residences=0, industrial=0, military=0, research=0, farms=0, oil.rigs=0, construction = 0 ) {
  state$success <- FALSE
  total <- sum(enterprize +  residences +  industrial +  military + research + farms + oil.rigs + construction)

  if(state$empty.land < total) 
    return(state)
  
  if(state$construction.cost * total > state$money)
    return(state)
  
  state <- state %>% mutate(enterprise.zones   = enterprise.zones + enterprize,
                            residences.zones   = residences.zones + residences,
                            industrial.zones   = industrial.zones + industrial,
                            military.zones     = military.zones + military,
                            research.zones     = research.zones + research,
                            farms.zones        = farms.zones + farms,
                            oil.zones          = oil.zones + oil.rigs,
                            construction.zones = construction.zones + construction,
                            money              = money - state$construction.cost * total
                            )
  
  state <- Update.State(state)
  state$success <- TRUE
  state <- Manage.End.Turn(state)
  return(state)
}
# TO DO Build Web Testing
Build.Web <- function(state, enterprize=0, residences=0, industrial=0, military=0, research=0, farms=0, oil.rigs=0, construction = 0 ) {
  state$success <- FALSE
  total <- sum(enterprize +  residences +  industrial +  military + research + farms + oil.rigs + construction)
  
  if(state$empty.land < total) 
    return(state)
  
  if(state$construction.cost * total > state$money)
    return(state)
  
  
  response <- buildTurn(cnum = state$cnum, ent=enterprize, res=residences,
                        indy = industrial, mb = military, lab = research, 
                        farm = farms, rig = oil.rigs, cs = construction)
  
  return(!is.null(response$turns))
  
  
  
  
  state <- state %>% mutate(enterprise.zones   = enterprise.zones + enterprize,
                            residences.zones   = residences.zones + residences,
                            industrial.zones   = industrial.zones + industrial,
                            military.zones     = military.zones + military,
                            research.zones     = research.zones + research,
                            farms.zones        = farms.zones + farms,
                            oil.zones          = oil.zones + oil.rigs,
                            construction.zones = construction.zones + construction,
                            money              = money - state$construction.cost * total
  )
  
  state <- Update.State(state)
  state$success <- TRUE
  state <- Manage.End.Turn(state)
  return(state)
}


Explore <- function(state, turns=1, sd=0) {
  state$success <- FALSE
  state <- Calc.Explore.Rate(state)
  if(state$empty.land > state$land / 2) 
    return(state)
  
  
  explore.rate <- state$explore.rate
  
  for(i in 1:turns)
  {
    state$land <- state$land + round(rnorm(1, mean = explore.rate, sd = state$explore.rate / 10 + 2))
    state <- Update.State(state)    
    state <- Manage.End.Turn(state)
    
    if(state$empty.land > state$land / 2) {
      break
    }
  }
  
  state$success <- TRUE
  return(state)
}
  
Cash <- function(state, enterprize=0, residences=0, industrial=0, military=0, research=0, farms=0, oil.rigs=0, construction = 0 ) {
  state$success <- FALSE

  state 
  
  state <- Update.State(state)
  state$success <- TRUE
  state <- Manage.End.Turn(state, cashing = 1.2)
  return(state)
}





# test.state <- Initialize.State()
# test.state <- Init.Gov.Bonus(test.state)
# test.state <- Calc.Buildings(test.state)
# test.state <- Calc.Empty.Land(test.state)
# test.state <- Calc.Tech.Total(test.state)
# test.state <- Calc.Missiles.Total(test.state)
# test.state <- Calc.Networth(test.state)
# test.state <- Calc.Construction.Cost(test.state)
# test.state <- Calc.Destruction.Cost(test.state)
# test.state <- Calc.Oil.Consumption(test.state)
# test.state <- Calc.Food.Produced(test.state)
# test.state <- Calc.Food.Consumption(test.state)
# test.state <- Calc.Food.Decay(test.state)
# #test.state <- Calc.Food(test.state)
# 
# test.state$food
# test.state$food.produced
# test.state$food.consumption
# test.state$food.decay
