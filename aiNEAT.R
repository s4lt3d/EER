# Playing with RNeat. Nothing to see here. Seriously

list.of.packages <- c("dplyr", "jsonlite", "httr", "randomNames", "plyr", "tidyr", "stats", "RSQLite", "sqldf", "devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library("dplyr") 
library("jsonlite") 
library("httr") 
library("randomNames") 
library("plyr")
library("tidyr")
library("stats")
library("RSQLite")
library("sqldf")

#special install of RNeat from github
#install.packages("devtools")
library("devtools")
#install_github("RNeat","ahunteruk") #Install from github as not yet on CRAN
library("RNeat")


this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source('web.r') # contains all the code for getting data in and out of the server
source('stat.r')
source('advisor.R')
source('game_functions.r')

server = getServer()

if(!exists("advisor.history"))
{
  advisor.history <<- tbl_df(read.table(file="EE_History.csv", header = TRUE, sep=","))
}

# get only the current round for the bots
advisor.history <- filter(advisor.history, round_num == server$round_num)

getInfo()



sim.InitialState <- function()
{
  #this should be the same length as the input
  state <- list()
  state[1] <- 1
  state[2] <- 2
  state[3] <- 1
  
  return(state)
}

sim.UpdateState <- function(currentState, neuralNetOutputs)
{
  #Here we can do something with the neuralNetOutputs that will affect the state
  currentState <- neuralNetOutputs
  return (currentState)
}

sim.ConvertStateToNeuralNetInputs <- function(currentState)
{
  return(currentState) # advanced, just return the currentState
}

sim.UpdateFitness <- function(oldState, updatedState, oldFitness)
{
  #Here we score our ai as a single number
  newFitness <- 0
  us <<- updatedState
  score <- (updatedState[[1]] * 13 + updatedState[[2]] * 7 + updatedState[[2]] * 11)
  
  score <- abs(score - 22)
  
  newFitness <- 1 / score 
#  print(newFitness)
  
  return(newFitness)
}

sim.CheckForTermination <- function(frameNum, oldState, updatedState, oldFitness, newFitness)
{
  # Has our bot worked or failed?
  # If its fails miserabily just end simulation
  
  # return T or F
  if(newFitness > 10000) return(T)
  if(newFitness < .1) return(T)
  if(frameNum > 1000) return(T)
  return(F)
}
sim.PlotState <- function(updatedState)
{
  score <- (updatedState[[1]] * 13 + updatedState[[2]] * 7 + updatedState[[2]] * 11)
  
  score <- abs(score - 22)
  
  newFitness <- 1 / score 
  print(newFitness)
  
}




config <- newConfigNEAT(3,3,100,25)
eeSimulation <- newNEATSimulation(config, sim.InitialState,
                                    sim.UpdateState,
                                    sim.ConvertStateToNeuralNetInputs,
                                    sim.UpdateFitness,
                                    sim.CheckForTermination,
                                    sim.PlotState)

for(i in seq(1,1000)){ 
  eeSimulation <- NEATSimulation.RunSingleGeneration(eeSimulation,F,"videos","poleBalance",1/simulation.timestep) 
}
