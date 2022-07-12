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
library("reticulate")

# STATIC DATES
.GlobalEnv$refresh_date <- as.Date("2022-06-02")
.GlobalEnv$t70_deadline <- as.Date("2022-06-30")
.GlobalEnv$dataset_date <- "2022-05-26" # dataset_date is passed to sec_date
.GlobalEnv$del_date <- as.Date("2022-05-30")

# Configuation Variables
load_dot_env(".env")

my_tokens <- Sys.getenv(
    list(
        "authn_resource",
        "authn_tenant",
        "authn_app",
        "authn_auth_type",
        "authn_password",
        "authn_use_cache"
        )
    )

print(" > Getting Azure tokens...")
tok0 <- get_azure_token(
    resource = my_tokens[[1]],
    tenant = my_tokens[[2]],
    app = my_tokens[[3]],
    auth_type = my_tokens[[4]],
    password = my_tokens[[5]],
    use_cache = my_tokens[[6]]
)
access_token <- tok0$credentials$access_token
bearer <- paste("Bearer", access_token)
headers <- add_headers(Authorization = bearer)
print(" > Tokens obtained successfully...")

# HELPERS

source("helpers/joins.r")
source("helpers/transformations.r")

# ETL

source("src/vaccination_rate/run_vaccination_rate.r")
source("src/base/run_base.r")
source("src/entity/run_entity.r")
source("src/population/run_population.r")
source("src/supply/run_supply.R")
source("src/vaccines/run_vaccines.r")
source("src/finance/run_finance.r")
source("src/demand_planning/run_demand_planning.r")

python_env <- run_vaccination_rate()
base_env <- run_base()
entity_env <- run_entity()
pop_env <- run_population()
supply_env <- run_supply(.GlobalEnv$dataset_date, .GlobalEnv$del_date)
vaccines_env <- run_vaccines(entity_env$entity_characteristics, .GlobalEnv$refresh_date, python_env$adm_data)
finance_env <- run_financing(entity_env$entity_characteristics)
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
    pop_env$population_data,
    pop_env$uptake_gender_data,
    base_env$b_who_dashboard,
    base_env$b_smartsheet,
    supply_env$supply_secured,
    supply_env$delivery_courses_doses,
    demand_plan_env$b_dp,
    supply_env$c_delivery_product,
    finance_env$b_fin_fund_del_sum,
    .GlobalEnv$refresh_date,
    .GlobalEnv$t70_deadline
)
supplies_env <- run_eda_supplies(vxrate_env$a_data)
coverage_env <- run_coverage(supplies_env$a_data, vxrate_env$timeto_t70, vaccines_env$c_vxrate_sept_t10, vaccines_env$c_vxrate_dec_t2040)
product_env <- run_product(coverage_env$a_data, base_env$b_smartsheet, .GlobalEnv$refresh_date, vxrate_env$timeto_t70)
financing_env <- run_financing(product_env$a_data)
ranking_env <- run_binning(financing_env$a_data)
combination_env <- run_combination(ranking_env$a_data)

# CONSOLIDATE

source("data/interim/consolidate/run_consolidate.r")

consolidate_env <- run_consolidate(
    combination_env$a_data,
    financing_env$a_data_amc,
    financing_env$a_data_africa,
    financing_env$a_data_csc,
    financing_env$a_data_ifc,
    vaccines_env$b_vxrate_change_lw)

# EXPORT

print(" > Exporting data output from pipeline to Excel Workbooks...")
all_df <- list(
    "0_base_data" = combination_env$a_data,
    "1_absorption_month" = vaccines_env$d_absorption,
    "1_absorption_month_country" = vaccines_env$combined,
    "1_stock" = vaccines_env$combined_three,
    "1_adm_long_smooth" = vaccines_env$b_vxrate_amc_smooth,
    "1_adm_all_long" = vaccines_env$b_vxrate_pub,
    "1_delivery_doses" = supply_env$supply_received_by_product,
    "1_secview" = combination_env$z_temp,
    "1_secview_lm" = combination_env$z_temp_lm,
    "1_secview_all" = combination_env$z_secview_long,
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
# write_xlsx(all_df, "data/output/220602_output_powerbi.xlsx")
# write_xlsx(financing_env$api, "data/output/220602_output_api.xlsx")

print(" > Output exported to Excel successfully!")
