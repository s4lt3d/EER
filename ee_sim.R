library(dplyr)

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

Initialize.State <- function(id=0)
{
  state.Names <- c("id", "money","tax.rate", "government", "food", "oil", 
                   "population", "turns.left", "turns.taken", "turns.stored", 
                   "enterprise.zones", "residences.zones", "industrial.zones", 
                   "military.zones", "research.zones", "farms.zones", 
                   "oil.zones", "construction.zones", "military.tech", 
                   "medical.tech", "business.tech", "residential.tech", 
                   "agricultural.tech", "warfare.tech", 
                   "military.strategy.tech", "weapons.tech", "industrial.tech",
                   "spy.tech", "sdi.tech", "spies.forces", "troops.forces", 
                   "jets.forces", "turrets.forces", "tanks.forces", 
                   "nuclear.missiles.forces", "chemical.missiles.forces", 
                   "cruise.missiles.forces", "at.war", "gdi.member")
  
  state <- as.data.frame(t(as.data.frame(rep(0, length(state.Names)))))
  colnames(state) <- state.Names
  state$land <- 100
  state$population <- 1000
  state$money <- 25000
  state$at.war <- F
  state$gdi.member <- F
  state$tax.rate <- 0.35
  state$troops.forces <- 100
  state$id <- id
  state$food <- 100
  state$government <- "M"
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
                              as.integer(
                                ((farms.zones * 5.3) + 
                                (empty.land * 0.4)) * gov.food.production.bonus * 
                              agricultural.tech
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

Calc.PCI <- function(state)
{
  state <- state %>% mutate(pci = round(22.5 * (1 - tax.rate) * 
                            (1 + ((networth/land)/18000)) * 
                            (1 + (2 * (enterprise.zones/land))) * 
                            business.tech * gov.pci.bonus, 2)  
  )
  return(state)
}

Calc.Revenue <- function(state)
{
  state <- state %>% mutate(revenue = as.integer(pci * population * tax.rate))
  return(state)
}

Calc.Buildings.Per.Turn <- function(state)
{
  state <- state %>% mutate(buildings.per.turn = round(((construction.sites/4) + 5) * 
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
       military.tech * (1 + networth / 200000000) * 
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
  state <- state %>% mutate(total.expense = military.upkeep + land.upkeep)
  
  return(state)
}

Calc.Explore.Rate <- function(state)
{
  explore.rate <- max(select(filter(explore.rates, land > state$land, gov == "O"), rate))
  
  if(state$government == 'R'){
    explore.rate <- max(select(filter(explore.rates, land > state$land, gov == "R"), rate))
  }
  
  state <- state %>% mutate(explore.rate = explore.rate)
  
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
