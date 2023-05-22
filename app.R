
# SET WD
# setwd("C:/Users/brooksd/OneDrive - World Health Organization/Documents/GitHub/covid19_vaccination_analysis") #Donald
setwd("C:/Users/rafae/OneDrive - World Health Organization/covid19_vaccination_analysis") #Rafael

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

.GlobalEnv$refresh_date <- as.Date("2023-05-22")
.GlobalEnv$del_date <- as.Date("2023-05-16")
.GlobalEnv$t70_deadline <- as.Date("2023-06-30")
.GlobalEnv$auto_cleaning <- TRUE # set to FALSE for no automised cleaning
.GlobalEnv$adm_api <- TRUE # DO NOT TOUCH. Set to FALSE to use base_dvr_current.xlsx
.GlobalEnv$refresh_api <- TRUE # set to FALSE to use last API call
.GlobalEnv$refresh_supply_timeseries <- TRUE # FALSE reads ../static/supply.xlsx Unless stated otherwise by Donald 

# HELPERS

source("helpers/joins.r")
source("helpers/transformations.r")
source("helpers/api.r")
api_env <- run_api()

# ETL

source("src/dvr/run_dvr.r")
source("src/entity_characteristics/run_entity_characteristics.r")
source("src/adm_cov/run_adm_cov.r")
source("src/cov_disag/run_cov_disag.r")
source("src/supply/run_supply.r")
source("src/finance/run_finance.r")
source("src/demand_planning/run_demand_planning.r")
source("src/add_data/run_add_data.r")
source("src/pin/run_pin.r")
source("src/one_budget_tracker/run_one_budget_tracker.r")
source("src/last_month/run_last_month.r")

dvr_env <- run_dvr(.GlobalEnv$adm_api,
                   .GlobalEnv$auto_cleaning,
                   api_env$headers,
                   .GlobalEnv$refresh_api)
entity_env <- run_entity()
adm_cov_env <- run_adm_cov(
  entity_env$entity_characteristics,
  .GlobalEnv$refresh_date,
  dvr_env$dvr_data,
  .GlobalEnv$adm_api,
  .GlobalEnv$auto_cleaning,
  .GlobalEnv$refresh_supply_timeseries)
cov_disag_env <- run_cov_disag(api_env$headers, .GlobalEnv$refresh_api)
supply_env <- run_supply(.GlobalEnv$del_date)
finance_env <- run_finance(entity_env$entity_characteristics)
demand_plan_env <- run_dp()
add_data_env <- run_add_data(.GlobalEnv$refresh_api)
pin_env <- run_pin()
one_budget_tracker_env <- run_one_budget_tracker()
last_month_env <- run_last_month()


# EDA

source("eda/adm_cov/run_adm_cov.r")
source("eda/supply/run_supply.r")
source("eda/prod_util/run_prod_util.r")
source("eda/cov_targets/run_cov_targets.r")
source("eda/finance/run_finance.r")
source("eda/qual_data/run_qual_data.r")
source("eda/rank_bin/run_rank_bin.r")
source("eda/pin/run_pin.r")
source("eda/sov/run_sov.r")
source("eda/last_month/run_last_month.r")

eda_adm_cov_env <- run_eda_adm_cov(
  adm_cov_env$c_vxrate_latest,
  entity_env$entity_characteristics,
  entity_env$population_data,
  cov_disag_env$uptake_gender_data,
  add_data_env$b_who_dashboard,
  supply_env$delivery_courses_doses,
  demand_plan_env$b_dp,
  supply_env$c_delivery_product,
  finance_env$b_fin_fund_del_sum,
  .GlobalEnv$refresh_date,
  .GlobalEnv$t70_deadline,
  cov_disag_env$target_hcwold,
  adm_cov_env$combined_three,
  adm_cov_env$overall_fin_cumul_long,
  adm_cov_env$b_vxrate_pub,
  pin_env$population_pin
)

eda_supply_env <- run_eda_supply(eda_adm_cov_env$a_data, supply_env$supply_received_by_product)
eda_prod_util_env <- run_eda_prod_util(
  eda_supply_env$a_data,
  .GlobalEnv$refresh_date,
  eda_adm_cov_env$timeto_t70)
eda_cov_targets_env <- run_eda_cov_targets(
  eda_prod_util_env$a_data,
  eda_adm_cov_env$timeto_t70,
  adm_cov_env$c_vxrate_sept_t10,
  adm_cov_env$c_vxrate_dec_t2040,
  adm_cov_env$c_vxrate_jun_t70,
  .GlobalEnv$t70_deadline)
eda_finance_env <- run_eda_finance(eda_cov_targets_env$a_data)
eda_qual_data_env <- run_eda_qual_data(eda_finance_env$a_data)
eda_rank_bin_env <- run_eda_rank_bin(eda_qual_data_env$a_data)
eda_pin_env <- run_eda_pin(eda_rank_bin_env$a_data)
eda_sov_env <- run_eda_sov(eda_pin_env$a_data)
eda_last_month_env <- run_eda_last_month(eda_sov_env$a_data, last_month_env$a_data_lm)


# CONSOLIDATE

source("consolidate/run_consolidate.r")

consolidate_env <- run_consolidate(
  eda_qual_data_env$a_data,
  eda_finance_env$a_data_amc,
  eda_finance_env$a_data_africa,
  eda_finance_env$a_data_csc,
  eda_finance_env$a_data_ifc,
  adm_cov_env$b_vxrate_change_lw,
  .GlobalEnv$refresh_date
)

# Data Checks
source("eda/data_checks/run_check.r")
eda_data_checks_env <- run_check(.GlobalEnv$refresh_date, 
                                 eda_sov_env$a_data, 
                                 eda_last_month_env$a_data_lm_change,
                                 adm_cov_env$d_absorption,
                                 adm_cov_env$combined,
                                 adm_cov_env$d_absorption_country_new,
                                 eda_adm_cov_env$timeseries,
                                 adm_cov_env$b_vxrate_pub,
                                 supply_env$supply_received_by_product,
                                 one_budget_tracker_env$base_one_budget_tracker,
                                 one_budget_tracker_env$base_one_budget_cds)

# EXPORT

print(" > Exporting data outputs from pipeline to Excel workbooks...")
all_df <- list(
  "0_base_data" = eda_data_checks_env$a_data,
  "0_base_data_lm_change" = eda_last_month_env$a_data_lm_change,
  "1_absorption_month" = adm_cov_env$d_absorption,
  "1_absorption_month_country" = adm_cov_env$combined,
  "1_cum_absorb_month_country" = adm_cov_env$d_absorption_country_new,
  "1_stock" = eda_adm_cov_env$timeseries,
  "1_adm_all_long" = adm_cov_env$b_vxrate_pub,
  "1_delivery_doses" = eda_supply_env$supply_received_by_product,
  "1_funding_source" = finance_env$b_fin_fund_del_source,
  "8_dvr_cat" = consolidate_env$e_vrcat_all,
  "8_cov_com_hcw_all" = consolidate_env$e_cov_com_hcw_all,
  "8_cov_com_60p_all" = consolidate_env$e_cov_com_60p_all,
  "8_cov_com_hcw_csc" = consolidate_env$e_cov_com_hcw_csc,
  "8_cov_com_60p_csc" = consolidate_env$e_cov_com_60p_csc,
  "8_ndvp_tar_cat" = consolidate_env$e_ndvp_all,
  "9_values" = consolidate_env$z_values,
  "1_fund_one_budget_tracker" = one_budget_tracker_env$base_one_budget_tracker,
  "1_fund_one_budget_cds" = one_budget_tracker_env$base_one_budget_cds,
  "1_fund_requests" = one_budget_tracker_env$base_requests,
  "0_output_master_checks" = eda_data_checks_env$output_master_checks
)

write_xlsx(all_df, "data/output/230522_output_powerbi.xlsx")
write_xlsx(eda_finance_env$api, "data/output/230522_output_api.xlsx")
write_xlsx(all_df, "data/output/output_master.xlsx")

print(" > Output exported to Excel successfully!")
