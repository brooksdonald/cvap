# WHO COVID-19 vaccine analysis pipeline (CVAP)

# Authors: BROOKS, Donald Joseph; PANLILIO, Rafael; DODT, Sebastian; 
#          KARARI, Alexander; COOPER, Jeremy; GACIC-DOBO, Marta

# Setup -------------------------------------------------------------------
setwd("/Users/brooksdonald/Documents/GitHub/cvap/")

rm(list = ls())
gc()
options(scipen = 999)

lib <- c("tidyverse",
         "openxlsx",
         "readxl",
         "writexl",
         "countrycode",
         "lubridate",
         "data.table",
         "stringr",
         "bit64",
         "httr",
         "jsonlite",
         "readr",
         # "AzureAuth",
         "dotenv",
         "reticulate",
         "ggrepel",
         "ISOweek")
    
lib_na <- lib[!(lib %in% installed.packages()[, "Package"])]
if (length(lib_na)) install.packages(lib_na)
lapply(lib, library, character.only = TRUE)


# Static values -----------------------------------------------------------

.GlobalEnv$date_refresh <- as.Date("2023-12-31")
.GlobalEnv$date_del <- as.Date("2023-11-20")
.GlobalEnv$auto_cleaning <- TRUE # set to FALSE for no automised cleaning
.GlobalEnv$adm_api <- TRUE # DO NOT TOUCH. Set to FALSE to use base_dvr_current.xlsx
.GlobalEnv$refresh_api <- TRUE # set to FALSE to use last API call
.GlobalEnv$refresh_timeseries <- TRUE 
.GlobalEnv$refresh_finance_timeseries <- FALSE
.GlobalEnv$refresh_supply_timeseries <- FALSE 


# Helpers -----------------------------------------------------------------

source("helpers/joins.r")
source("helpers/transformations.r")
source("helpers/api.r")


# Extract, transform, load (ETL) ------------------------------------------

source("src/entity_characteristics/run_entity_characteristics.r")
source("src/dvr/run_dvr.r")
source("src/supply/run_supply.r")
source("src/adm_cov/run_adm_cov.r")
source("src/cov_disag/run_cov_disag.r")
source("src/finance/run_finance.r")
source("src/add_data/run_add_data.r")

entity_env <- run_entity()
dvr_env <- run_dvr(
  .GlobalEnv$auto_cleaning,
  api_env$headers,
  .GlobalEnv$refresh_api)
supply_env <- run_supply(.GlobalEnv$date_del,
                         .GlobalEnv$date_refresh,
                         .GlobalEnv$refresh_timeseries)
adm_cov_env <- run_adm_cov(
  entity_env$entity_characteristics,
  .GlobalEnv$date_refresh,
  dvr_env$dvr_data,
  supply_env$sup_ts_wide)
cov_disag_env <- run_cov_disag(api_env$headers, .GlobalEnv$refresh_api)
finance_env <- run_finance(entity_env$entity_characteristics)
add_data_env <- run_add_data(.GlobalEnv$refresh_api)


# Exploratory data analysis (EDA) -----------------------------------------

source("eda/adm_cov/run_adm_cov.r")
source("eda/supply/run_supply.r")
source("eda/prod_util/run_prod_util.r")
source("eda/cov_targets/run_cov_targets.r")
source("eda/finance/run_finance.r")
source("eda/rank_bin/run_rank_bin.r")
source("eda/pin/run_pin.r")
source("eda/export/run_export.r")

eda_adm_cov_env <- run_eda_adm_cov(
    adm_cov_env$c_vxrate_latest, 
    entity_env$entity_characteristics,
    entity_env$population,
    cov_disag_env$uptake_gender_data,
    add_data_env$who_dashboard,
    supply_env$sup_rec,
    add_data_env$b_dp,
    supply_env$sup_rec_jj,
    finance_env$fin_del_sum,
    .GlobalEnv$date_refresh,
    cov_disag_env$target_hcwold,
    adm_cov_env$combined_three,
    adm_cov_env$adm_tot_ts_daily,
    add_data_env$population_pin
)
supplies_env <- run_eda_supplies(eda_adm_cov_env$a_data, 
                                 supply_env$sup_rec_dose_prod)
prod_util_env <- run_prod_util(
    supplies_env$a_data,
    .GlobalEnv$date_refresh)
cov_targets_env <- run_cov_targets(
    prod_util_env$a_data,
    adm_cov_env$adm_tot_sep21,
    adm_cov_env$adm_tot_dec21,
    adm_cov_env$adm_tot_jun22)
financing_env <- run_financing(cov_targets_env$a_data)
rank_bin_env <- run_rank_bin(financing_env$a_data)
eda_pin_env <- run_eda_pin(rank_bin_env$a_data, add_data_env$population_pin)
export_env <- run_export(eda_pin_env$a_data) 


# Export ------------------------------------------------------------------

print(" > Exporting data outputs from pipeline to Excel workbooks...")
all_df <- list(
    "0_base_data" = eda_pin_env$a_data,
    "1_absorption_month" = adm_cov_env$d_absorption,
    "1_stock" = eda_adm_cov_env$timeseries,
    "1_adm_all_long" = adm_cov_env$adm_tot_ts_daily,
    "1_delivery_doses" = supply_env$sup_rec_dose_prod,
    "1_funding_source" = finance_env$fin_del_sum_source
    # "9_values" = consolidate_env$z_values
)

# write_xlsx(all_df, paste0("data/output/", format(date_refresh, "%y%m%d"), "_output_powerbi.xlsx"))
# write_xlsx(export_env$api, paste0("data/output/", format(date_refresh, "%y%m%d"), "output_api.xlsx"))
write_xlsx(all_df, "data/output/output_master_test.xlsx")
# # write_xlsx(export_env$dashboard, paste0("data/output/", format(date_refresh, "%y%m%d"), "_output_dashboard.xlsx"))

print(" > Output exported to Excel successfully.")