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
library("tidyverse")
library("httr")
library("jsonlite")
library("AzureAuth")
library("dotenv")

# Configuation Variables
# load_dot_env(file = ".env")

# HELPERS

source("helpers/joins.r")
source("helpers/transformations.r")

# STATIC DATES
.GlobalEnv$refresh_date <- as.Date("2022-06-02")
.GlobalEnv$t70_deadline <- as.Date("2022-06-30")
.GlobalEnv$dataset_date <- "2022-05-26" # dataset_date is passed to sec_date
.GlobalEnv$del_date <- as.Date("2022-05-30")

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
vaccines_env <- run_vaccines(entity_env$entity_characteristics)
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
    vaccines_env$c_vxrate_latest,
    entity_env$entity_characteristics,
    c_vxrate_latest_red,
    pop_env$population_data,
    pop_env$uptake_gender_data,
    base_env$b_who_dashboard,
    base_env$b_smartsheet,
    supply_env$supply_secured,
    supply_env$delivery_courses_doses,
    demand_plan_env$b_dp,
    .GlobalEnv$c_delivery_product,
    .GlobalEnv$b_fin_fund_del_sum
)
supplies_env <- run_eda_supplies(vxrate_env$a_data)
coverage_env <- run_coverage(supplies_env$a_data)
product_env <- run_product(coverage_env$a_data)
financing_env <- run_financing(product_env$a_data)
ranking_env <- run_binning(financing_env$a_data)
combination_env <- run_combination(ranking_env$a_data)

# CONSOLIDATE

source("consolidate/run_consolidate.r")

consolidate_env <- run_consolidate(combination_env$a_data)

# EXPORT

print(" > Exporting data output from pipeline to Excel Workbooks...")
all_df <- list(
    "0_base_data" = combination_env$a_data,
    "1_absorption_month" = vaccines_env$d_absorption,
    "1_absorption_month_country" = vaccines_env$combined,
    "1_stock" = vaccines_env$combined_three,
    "1_adm_long_smooth" = vaccines_env$b_vxrate_amc_smooth,
    "1_adm_all_long" = vaccines_env$b_vxrate_pub,
    "1_delivery_doses" = .GlobalEnv$supply_received_by_product,
    "1_secview" = .GlobalEnv$z_temp,
    "1_secview_lm" = .GlobalEnv$z_temp_lm,
    "1_secview_all" = .GlobalEnv$z_secview_long,
    "1_funding_source" = finance_env$b_fin_fund_del_source,
    "2_dvr_perchange_count" = consolidate_env$f_dvr_change_count,
    "2_cov_change_count" = consolidate_env$f_cov_change_count,
    "2_dvr_perchange_count_af" = consolidate_env$f_dvr_change_count_af,
    "2_cov_change_count_af" = consolidate_env$f_cov_change_count_af,
    "8_dvr_cat" = consolidate_env$e_vrcat_all,
    "8_dvr_lm_trend" = consolidate_env$e_trend_all,
    "8_tarpast_cat" = consolidate_env$e_tar_past_all,
    "8_curtar_cat" = consolidate_env$e_tar_cur_all,
    "8_curtar_scale_cat" = consolidate_env$e_tar_cur_scale_all,
    "8_booster_status" = consolidate_env$e_booster_all,
    "8_secdelpu_cat" = consolidate_env$e_secdelpu_all,
    "8_cov_cat" = consolidate_env$e_cov_all,
    "8_ndvp_tar_cat" = consolidate_env$e_ndvp_all,
    "9_values" = consolidate_env$z_values
)
write_xlsx(all_df, "data/output/220602_output_powerbi_29_June_app_3.xlsx")
# write_xlsx(all_df, "data/output/220602_output_powerbi.xlsx)
# write_xlsx(api, "data/output/220602_output_api.xlsx")

print(" > Output exported to Excel successfully!")