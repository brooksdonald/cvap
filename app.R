library("readxl")
library("writexl")
library("countrycode")
library("dplyr")
library("lubridate")
library("tidyr")
library("data.table")
library("bit64")

source("src/helpers/joins.r")
source("src/helpers/transformations.r")


# source("src/supply/run_supply.R")
# source("src/entity/run_entity.r")
# source("src/population/run_population.r")
# source("src/base/run_base.r")
source("src/vaccines/run_vaccines.r")

# supply_env <- run_supply()
# pop_env <- run_population()
# base_env <- run_base()
vax_env <- run_vaccines()
