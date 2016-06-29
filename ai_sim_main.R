source('ee_sim.R')
source('print_state.R')

state <- Initialize.State(61)
#printState(state)

state <- Cash(state, T)
#for(i in 1:80)
#  state <- Build(state, construction = 1)
printState(state)

#state <- Build(state, farms = state$buildings.per.turn)

#state <- Explore(state, turns=50)
#printState(state)
