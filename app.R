#  Clear environment
rm(list = ls())

# Load packages
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
source("src/finance/run_finance.r")
source("src/demand_planning/run_demand_planning.r")

base_env <- run_base()
entity_env <- run_entity()
pop_env <- run_population()
supply_env <- run_supply()
vaccines_env <- run_vaccines(entity_characteristics)
finance_env <- run_financing()
demand_plan_env <- run_dp()

# EDA

source("eda/vxrate/run_vxrate.r")
source("eda/supplies/run_supplies.r")
source("eda/coverage/run_coverage.r")
source("eda/product/run_product.r")
source("eda/financing/run_financing.r")
source("eda/rank_bin/run_rank_bin.r")
source("eda/combination/run_combination.r")

vxrate_env <- run_vxrate(
    c_vxrate_latest,
    entity_characteristics,
    c_vxrate_latest_red,
    population_data,
    uptake_gender_data,
    b_who_dashboard,
    b_smartsheet,
    supply_secured,
    delivery_courses_doses,
    b_dp,
    c_delivery_product,
    b_fin_fund_del_sum
)
supplies_env <- run_eda_supplies(a_data)
coverage_env <- run_coverage(a_data)
product_env <- run_product(a_data)
financing_env <- run_financing(a_data)
ranking_env <- run_binning(a_data)
combination_env <- run_combination(a_data)

# CONSOLIDATE

source("consolidate/run_consolidate.r")

consolidate_env <- run_consolidate(a_data)

# Export

source("eda/export/run_export.r")

# export_env <- run_export()