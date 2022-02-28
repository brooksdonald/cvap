library("readxl")
library("writexl")
library("countrycode")
library("dplyr")
library("lubridate")
library("tidyr")
library("data.table")
library("bit64")


# HELPERS

source("helpers/joins.r")
source("helpers/transformations.r")

# ETL

source("src/base/run_base.r")
source("src/entity/run_entity.r")
source("src/population/run_population.r")
source("src/supply/run_supply.R")
source("src/vaccines/run_vaccines.r")

base_env <- run_base()
entity_env <- run_entity()
pop_env <- run_population()
supply_env <- run_supply()
vaccines_env <- run_vaccines(entity_characteristics)

# EDA

source("eda/vxrate/run_vxrate.r")

vxrate_env <- run_vxrate(entity_characteristics, population_data, uptake_gender_data, c_vxrate_latest)

