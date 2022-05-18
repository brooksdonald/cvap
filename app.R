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

# STATIC DATES
.GlobalEnv$refresh_date <- as.Date("2022-05-18")
.GlobalEnv$t70_deadline <- as.Date("2022-06-30")
.GlobalEnv$dataset_date <- "2022-05-12" # dataset_date is passed to sec_date
.GlobalEnv$del_date <- as.Date("2022-05-17")

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

# EXPORT

print(" > Exporting data output from pipeline to Excel Workbooks...")
all_df <- list(
    "0_base_data" = a_data,
    "1_absorption_month" = d_absorption,
    "1_absorption_month_country" = combined,
    "1_stock" = combined_three,
    "1_adm_long_smooth" = b_vxrate_amc_smooth,
    "1_adm_all_long" = b_vxrate_pub,
    "1_delivery_doses" = supply_received_by_product,
    "1_secview" = z_temp,
    "1_secview_lm" = z_temp_lm,
    "1_secview_all" = z_secview_long,
    "1_funding_source" = b_fin_fund_del_source,
    "2_dvr_perchange_count" = f_dvr_change_count,
    "2_cov_change_count" = f_cov_change_count,
    "2_dvr_perchange_count_af" = f_dvr_change_count_af,
    "2_cov_change_count_af" = f_cov_change_count_af,
    "8_dvr_cat" = e_vrcat_all,
    "8_dvr_lm_trend" = e_trend_all,
    "8_tarpast_cat" = e_tar_past_all,
    "8_curtar_cat" = e_tar_cur_all,
    "8_curtar_scale_cat" = e_tar_cur_scale_all,
    "8_booster_status" = e_booster_all,
    "8_secdelpu_cat" = e_secdelpu_all,
    "8_cov_cat" = e_cov_all,
    "8_ndvp_tar_cat" = e_ndvp_all,
    "9_values" = z_values
)
write_xlsx(all_df, "data/output/220518_output_powerbi.xlsx")
write_xlsx(api, "data/output/220518_output_api.xlsx")

print(" > Output exported successfully!")