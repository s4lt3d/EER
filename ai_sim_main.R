library(RSQLite)

source('ee_sim.R')
source('print_state.R')

con <- dbConnect(RSQLite::SQLite(), dbname="state.sqlite")

#dbSendQuery(conn = con, "DROP TABLE IF EXISTS states")
dbListTables(con)


state <- Initialize.State(36)
printState(state)
state <- Sync.State(state)
printState(state)

#state <- Cash(state, T)
for(i in 1:80){
  state <- Build(state, construction = 1)
  dbWriteTable(conn=con, name="states", as.data.frame(state), row.names = FALSE, append=TRUE)
}
  
printState(state)

#state <- Build(state, farms = state$buildings.per.turn)

#state <- Explore(state, turns=50)
#printState(state)



allstates <- dbReadTable(conn= con, name="states")

print(allstates)

dbDisconnect(con)

