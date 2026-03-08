# EER (Earth Empires Robot)

> Advanced R-based Earth Empires bot with full game simulation engine, advisor system, unit tests, and NEAT evolution framework.

---

## Overview

EER is the most complete Earth Empires AI bot implementation to date. It features:

- **Full Game Simulation** — Complete Earth Empires mechanics implemented in R
- **Playable Bot** — Successfully plays full rounds from start to finish
- **Historical Data Analysis** — Tracks 2.6MB+ of historical game data (CSV format)
- **Unit Testing Framework** — Comprehensive test suite for simulation accuracy
- **NEAT Evolution** — Neuroevolution algorithm for strategy optimization
- **Database Persistence** — SQLite integration for state tracking and history
- **Visualization & Reporting** — R-based analysis and plotting tools

**Status:** Functional but requires optimization and advanced strategies for competitive play.

---

## Architecture

### Core Modules

**ee_sim.R** — Game Simulation Engine (~1000 lines)
- Complete Earth Empires game mechanics simulation
- Supports all resources, buildings, troops, tech trees
- Tested against real game behavior
- Efficient calculation of turns, income, production, decay
- Government bonuses and special effects

**advisor.R** — Advisor System
- Fetches current game state from server
- Parses and validates advisor data
- Stores history in SQLite database
- Calculates trends and growth rates
- Visualization via plotting functions
- Real-time state tracking

**web.r** — Server Communication
- HTTP requests to Earth Empires API
- JSON request/response handling
- Authentication and session management
- Handles advisor, market, and transaction queries

**game_functions.r** — Game Logic Utilities
- Individual calculation functions for game mechanics
- Isolated, testable components
- Helper functions for strategy evaluation

**aiNEAT.R** — Neuroevolution
- NEAT (NeuroEvolution of Augmenting Topologies) implementation
- Trains neural networks to optimize bot decisions
- Adapts strategy during gameplay

**ai.r / ai_sim_main.R** — Main AI Logic
- Advisor decision-making
- Strategy selection
- Resource allocation decisions

### Supporting Files

**ee_sim_unit_test.R** — Test Suite
- Validates simulation accuracy
- Tests against known game behavior
- Ensures calculation correctness

**ee_sim_benchmark.r** — Performance Testing
- Measures simulation speed
- Optimizes calculation efficiency

**stat.r** — Statistical Analysis
- Data analysis and metrics
- Trend calculation

**print_state.R** — State Visualization
- Formatted output of game state
- Debugging and inspection

**explore_rates.R** — Parameter Exploration
- Tests different strategy parameters
- Evaluates effectiveness

---

## Data Files

### Databases (SQLite)

- **advisor.sqlite** — Advisor state history
- **country.sqlite** — Country information
- **state.sqlite** — Game state snapshots
- **eer.sqlite** — Main EER database
- **eertest.sqlite** — Test database (18MB)

### Historical Data

- **EE_History.csv** — 2.6MB historical game data
- **history.RData** — R binary data format
- **.RData, .Rhistory** — R session data

### Configuration

- **EER2.Rproj** — R Studio project file
- **advisor_report_template.txt** — Output formatting
- **explore_rates.csv** — Parameter exploration results

---

## Getting Started

### Prerequisites

- R 3.5+ (or later)
- RStudio (recommended)
- R packages:
  - `dplyr`, `tidyr` — Data manipulation
  - `jsonlite`, `httr` — API communication
  - `RSQLite` — Database access
  - `randomNames`, `stats` — Utilities

### Setup

```bash
# Install dependencies
Rscript -e "install.packages(c('dplyr', 'tidyr', 'jsonlite', 'httr', 'RSQLite', 'randomNames'))"

# Open EER2.Rproj in RStudio, or run:
source('ee_sim.R')
```

### Basic Usage

```r
# Load simulation
source('ee_sim.R')

# Get current game state
state <- get.advisor(cnum=61)

# Run simulation
sim_result <- Initialize.State(cnum=61)

# Visualize trends
plot.advisor(cnum=61, tail.n=20)

# Run unit tests
source('ee_sim_unit_test.R')
```

---

## Game Mechanics Simulated

The simulation implements:

### Economy
- Taxation and revenue
- Per capita income (PCI)
- Money accumulation and spending
- Food production and consumption
- Oil production

### Infrastructure
- 8 building types with land requirements
- Construction speed and costs
- Production rates per building type
- Land management and distribution

### Technology
- 8 technology tracks (Military, Medical, Business, Residential, Agricultural, Warfare, Spy, SDI)
- Tech levels from 0-100%
- Tech cost calculations
- Tech bonuses on production/military

### Military
- 5 troop types (Spies, Troops, Jets, Turrets, Tanks)
- 3 missile types (Nuclear, Chemical, Cruise)
- Military strength calculations
- Warfare mechanics (attacks, defense, casualties)

### Government System
- 10 government types with unique bonuses
- Food production bonuses
- Strike/attack bonuses
- Oil production bonuses
- Population/PCI bonuses
- Military upkeep/strength bonuses

---

## Development & Testing

### Run Unit Tests

```r
source('ee_sim_unit_test.R')
# Tests verify simulation matches real game behavior
```

### Benchmark Performance

```r
source('ee_sim_benchmark.r')
# Measures simulation speed for optimization
```

### Explore Strategy Parameters

```r
source('explore_rates.R')
# Tests different strategy configurations
# Results saved to explore_rates.csv
```

---

## Known Limitations & TODOs

The code comments indicate work-in-progress areas:

- [ ] Rebuilding cost calculations
- [ ] Alliance upkeep costs
- [ ] GDI penalties
- [ ] SPAL (Strategic Planned Attack Launch)
- [ ] Standard/Planned strike mechanics
- [ ] Government change system
- [ ] Advanced advisor strategies
- [ ] Optimization for competitive play

---

## Strategy Development

EER provides a framework for testing strategies:

1. **Simulate decisions** using `ee_sim.R`
2. **Evaluate outcomes** with unit tests
3. **Optimize parameters** using `explore_rates.R`
4. **Train neural networks** with `aiNEAT.R`
5. **Analyze results** with historical data
6. **Iterate and improve**

---

## References

- [Earth Empires](http://earthempires.com) — Official game server
- [EE Wiki](https://wiki.earthempires.com) — Game mechanics
- R packages: dplyr, tidyr, jsonlite, httr, RSQLite
- NEAT algorithm for evolutionary learning

---

## License

Copyright © Walter Gordy 
