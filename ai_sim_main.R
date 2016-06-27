
source('ee_sim.R')

state <- Initialize.State()

state <- Build(state, farms = state$buildings.per.turn)

