


source('ee_sim.R')


state <- Initialize.State(61)

state <- Build(state, farms = state$buildings.per.turn)

fileName <- 'advisor_report_template.txt'

advisor_template <- readChar(fileName, file.info(fileName)$size)
#cat(advisor_template)
output <- sprintf(advisor_template, 
                  state$cnum, 
                  state$government,
                  state$turns, 
                  state$money, 
                  state$food, 
                  state$networth, 
                  state$turns, 
                  0, # taxrevenue
                  0, #turns taken
                  state$taxrate * 100,
                  0, #turns stored
                  state$pci,
                  0, #ranks
                  state$total.expense,
                  state$networth,
                  0, #net income
                  state$land,
                  0, #cashing
                  state$money,
                  state$food,
                  state$population,
                  state$food.produced,
                  0, #at.war,
                  state$food.consumption,
                  0, #GDI Member
                  state$food.decay,
                  0, # food net change
                  state$oil,
                  0, #oil production
                  state$enterprise.zones,
                  state$buildings.per.turn,
                  state$residences.zones,
                  state$explore.rate,
                  state$industrial.zones,
                  state$tech.per.turn,
                  state$military.zones,
                  state$research.zones,
                  state$farms.zones,
                  state$oil.zones,
                  state$spies.forces,
                  state$construction.zones,
                  state$troops.forces, 
                  state$land - state$empty.land,
                  state$jets.forces,
                  state$empty.land,
                  state$turrets.forces,
                  state$tanks.forces,
                  state$nuclear.missiles.forces,
                  state$chemical.missiles.forces,
                  state$military.tech,
                  state$military.tech.per * 100,
                  state$cruise.missiles.forces,
                  state$medical.tech,
                  state$medical.tech.per * 100,
                  state$business.tech,
                  state$business.tech.per * 100,
                  state$residential.tech,
                  state$residential.tech.per * 100,
                  state$agricultural.tech,
                  state$agricultural.tech.per * 100,
                  state$total.expense, 
                  state$warfare.tech,
                  state$warfare.tech.per * 100,
                  0, 
                  state$military.strategy.tech,
                  state$military.strategy.tech.per * 100,
                  0,
                  state$weapons.tech,
                  state$weapons.tech.per * 100,
                  0, 
                  state$industrial.tech, 
                  state$industrial.tech.per * 100,
                  0, 
                  state$spy.tech,
                  state$spy.tech.per * 100,
                  0,
                  state$sdi.tech,
                  state$sdi.tech.per * 100, 
                  0,
                  0,
                  state$tech.total, 
                  state$land.upkeep,
                  0
                  )

cat(output)
