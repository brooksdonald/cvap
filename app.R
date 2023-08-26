
# SET WD
setwd("C:/Users/brooksd/OneDrive - World Health Organization/Documents/GitHub/covid19_vaccination_analysis") #Donald

# CLEAR ENVIRONMENT
rm(list = ls())
gc()

# PACKAGES

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
         "AzureAuth",
         "dotenv",
         "reticulate",
         "ggrepel")
    
lib_na <- lib[!(lib %in% installed.packages()[, "Package"])]
if (length(lib_na)) install.packages(lib_na)
lapply(lib, library, character.only = TRUE)

# STATIC VARIABLES

.GlobalEnv$date_refresh <- as.Date("2023-08-25")
.GlobalEnv$date_del <- as.Date("2023-08-11")
.GlobalEnv$auto_cleaning <- TRUE # set to FALSE for no automised cleaning
.GlobalEnv$adm_api <- TRUE # DO NOT TOUCH. Set to FALSE to use base_dvr_current.xlsx
.GlobalEnv$refresh_api <- TRUE # set to FALSE to use last API call
.GlobalEnv$refresh_timeseries <- TRUE # FALSE reads ../static/supply.xlsx Unless stated otherwise by Donald 
.GlobalEnv$refresh_supply_timeseries <- TRUE # FALSE reads ../static/supply.xlsx Unless stated otherwise by Donald 

# HELPERS

source("helpers/joins.r")
source("helpers/transformations.r")
source("helpers/api.r")
api_env <- run_api()


# Extract, transform, load (ETL) ------------------------------------------

source("src/entity_characteristics/run_entity_characteristics.r")
source("src/dvr/run_dvr.r")
source("src/supply/run_supply.r")
source("src/adm_cov/run_adm_cov.r")
source("src/cov_disag/run_cov_disag.r")
source("src/finance/run_finance.r")
source("src/add_data/run_add_data.r")
source("src/last_month/run_last_month.r")

entity_env <- run_entity()
dvr_env <- run_dvr(
  .GlobalEnv$auto_cleaning,
  api_env$headers,
  .GlobalEnv$refresh_api,
  .GlobalEnv$adm_api)
supply_env <- run_supply(.GlobalEnv$date_del,
                         .GlobalEnv$date_refresh,
                         .GlobalEnv$refresh_timeseries)
adm_cov_env <- run_adm_cov(
  entity_env$entity_characteristics,
  .GlobalEnv$date_refresh,
  dvr_env$dvr_data,
  .GlobalEnv$refresh_supply_timeseries)
cov_disag_env <- run_cov_disag(api_env$headers, .GlobalEnv$refresh_api)
finance_env <- run_finance(entity_env$entity_characteristics)
add_data_env <- run_add_data(.GlobalEnv$refresh_api)
last_month_env <- run_last_month()


# Exploratory data analysis (EDA) -----------------------------------------

source("eda/adm_cov/run_adm_cov.r")
source("eda/supply/run_supply.r")
source("eda/prod_util/run_prod_util.r")
source("eda/cov_targets/run_cov_targets.r")
source("eda/finance/run_finance.r")
source("eda/rank_bin/run_rank_bin.r")
source("eda/pin/run_pin.r")
source("eda/sov/run_sov.r")
source("eda/last_month/run_last_month.r")
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
    finance_env$b_fin_fund_del_sum,
    .GlobalEnv$date_refresh,
    cov_disag_env$target_hcwold,
    adm_cov_env$combined_three,
    finance_env$overall_fin_cumul_long,
    adm_cov_env$b_vxrate_pub,
    add_data_env$population_pin
)

supplies_env <- run_eda_supplies(eda_adm_cov_env$a_data, supply_env$sup_rec_dose_prod)
prod_util_env <- run_prod_util(
    supplies_env$a_data,
    .GlobalEnv$date_refresh)
cov_targets_env <- run_cov_targets(
    prod_util_env$a_data,
    adm_cov_env$c_vxrate_sept_t10,
    adm_cov_env$c_vxrate_dec_t2040,
    adm_cov_env$c_vxrate_jun_t70)
financing_env <- run_financing(cov_targets_env$a_data)
rank_bin_env <- run_rank_bin(financing_env$a_data)
eda_pin_env <- run_eda_pin(rank_bin_env$a_data, add_data_env$population_pin)
eda_sov_env <- run_sov(eda_pin_env$a_data)
eda_last_month_env <- run_last_month(eda_sov_env$a_data, last_month_env$a_data_lm)
export_env <- run_export(eda_last_month_env$a_data) 


# Consolidate -------------------------------------------------------------

source("consolidate/run_consolidate.r")

consolidate_env <- run_consolidate(
  financing_env$a_data,
  financing_env$a_data_amc,
  financing_env$a_data_africa,
  financing_env$a_data_csc,
  adm_cov_env$b_vxrate_change_lw,
  .GlobalEnv$date_refresh
)

source("consolidate/last_month/run_last_month.r")
source("consolidate/funding_tracker/run_funding_tracker.r")
# 
 last_month_env <- run_last_month()
 # funding_tracker_env <- run_funding_tracker()

# EXPORT

print(" > Exporting data outputs from pipeline to Excel workbooks...")
all_df <- list(
    "0_base_data" = eda_sov_env$a_data,
    "0_base_data_lm_change" = last_month_env$base_data_lm_change,
    "1_absorption_month" = adm_cov_env$d_absorption,
    "1_absorption_month_country" = adm_cov_env$combined,
    "1_cum_absorb_month_country" = adm_cov_env$d_absorption_country_new,
    "1_stock" = eda_adm_cov_env$timeseries,
    "1_adm_all_long" = adm_cov_env$b_vxrate_pub,
    "1_delivery_doses" = supply_env$sup_rec_dose_prod,
    "1_funding_source" = finance_env$b_fin_fund_del_source,
    "8_cov_com_hcw_all" = consolidate_env$e_cov_com_hcw_all,
    "8_cov_com_60p_all" = consolidate_env$e_cov_com_60p_all,
    "8_cov_com_hcw_csc" = consolidate_env$e_cov_com_hcw_csc,
    "8_cov_com_60p_csc" = consolidate_env$e_cov_com_60p_csc,
    "9_values" = consolidate_env$z_values
)

write_xlsx(all_df, paste0("data/output/", format(date_refresh, "%y%m%d"), "_output_powerbi.xlsx"))
write_xlsx(export_env$api, paste0("data/output/", format(date_refresh, "%y%m%d"), "output_api.xlsx"))
write_xlsx(all_df, "data/output/output_master.xlsx")

print(" > Output exported to Excel successfully!")
