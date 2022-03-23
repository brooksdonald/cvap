library("readxl")
library("writexl")
library("countrycode")
library("dplyr")
library("lubridate")
library("tidyr")
library("data.table")
library("bit64")
library("openxlsx")

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
source("eda/supplies/run_supplies.r")
source("eda/coverage/run_coverage.r")
source("eda/product/run_product.r")
source("eda/rank-bin/run_rank_bin.r")
source("eda/consolidate/run_consolidate.r")
source("eda/export/run_export.r")


vxrate_env <- run_vxrate(entity_characteristics, population_data, uptake_gender_data, c_vxrate_latest)
supplies_env <- run_eda_supplies(a_data, supply_secured, delivery_courses_doses)
coverage_env <- run_coverage(a_data)
product_env <- run_product(a_data, base_env$b_smartsheet, base_env$b_csl)
ranking_env <- run_binning(a_data)
consolidate_env <- run_consolidate(a_data)
export_env <- run_export()

