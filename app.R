
# SET WD
# setwd("C:/Users/brooksd/OneDrive - World Health Organization/Documents/GitHub/covid19_vaccination_analysis") #Donald
setwd("C:/Users/rafae/OneDrive - World Health Organization/covid19_vaccination_analysis") #Rafael

# CLEAR ENVIRONMENT
rm(list = ls())

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

.GlobalEnv$refresh_date <- as.Date("2023-02-09")
.GlobalEnv$del_date <- as.Date("2023-02-06")
.GlobalEnv$t70_deadline <- as.Date("2023-06-30")
.GlobalEnv$auto_cleaning <- TRUE # set to FALSE for no automised cleaning
.GlobalEnv$adm_api <- TRUE # DO NOT TOUCH. Set to FALSE to use base_dvr_current.xlsx
.GlobalEnv$refresh_api <- FALSE # set to FALSE to use last API call
.GlobalEnv$refresh_supply_timeseries <- TRUE # FALSE reads ../static/supply.xlsx Unless stated otherwise by Donald 

# HELPERS

source("helpers/joins.r")
source("helpers/transformations.r")
source("helpers/api.r")
api_env <- run_api()

# ETL

source("src/dvr/run_dvr.r")
source("src/entity_characteristics/run_entity_characteristics.r")
source("src/supply/run_supply.r")
source("src/adm_cov/run_adm_cov.r")
source("src/cov_disag/run_cov_disag.r")
source("src/finance/run_finance.r")
source("src/demand_planning/run_demand_planning.r")
source("src/add_data/run_add_data.r")
source("src/pin/run_pin.r")

dvr_env <- run_dvr(.GlobalEnv$adm_api,
    .GlobalEnv$auto_cleaning,
    api_env$headers,
    .GlobalEnv$refresh_api)
entity_env <- run_entity()
supply_env <- run_supply(.GlobalEnv$del_date)
adm_cov_env <- run_adm_cov(
    entity_env$entity_characteristics,
    .GlobalEnv$refresh_date, dvr_env$dvr_data,
    .GlobalEnv$adm_api,
    .GlobalEnv$auto_cleaning,
    .GlobalEnv$refresh_supply_timeseries)
cov_disag_env <- run_cov_disag(api_env$headers, .GlobalEnv$refresh_api)
finance_env <- run_finance(entity_env$entity_characteristics)
demand_plan_env <- run_dp()
add_data_env <- run_add_data(.GlobalEnv$refresh_api)
pin_env <- run_pin()

# EDA

source("eda/adm_cov/run_adm_cov.r")
source("eda/supply/run_supply.r")
source("eda/prod_util/run_prod_util.r")
source("eda/cov_targets/run_cov_targets.r")
source("eda/finance/run_finance.r")
source("eda/qual_data/run_qual_data.r")
source("eda/rank_bin/run_rank_bin.r")
source("eda/pin/run_pin.r")

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

supplies_env <- run_eda_supplies(eda_adm_cov_env$a_data)
prod_util_env <- run_prod_util(
    supplies_env$a_data,
    .GlobalEnv$refresh_date,
    eda_adm_cov_env$timeto_t70)
cov_targets_env <- run_cov_targets(
    prod_util_env$a_data,
    eda_adm_cov_env$timeto_t70,
    adm_cov_env$c_vxrate_sept_t10,
    adm_cov_env$c_vxrate_dec_t2040,
    adm_cov_env$c_vxrate_jun_t70,
    .GlobalEnv$t70_deadline)
financing_env <- run_financing(cov_targets_env$a_data)
qual_data_env <- run_qual_data(financing_env$a_data)
rank_bin_env <- run_rank_bin(qual_data_env$a_data)
eda_pin_env <- run_eda_pin(rank_bin_env$a_data, pin_env$population_pin)


# CONSOLIDATE

source("consolidate/run_consolidate.r")

consolidate_env <- run_consolidate(
  qual_data_env$a_data,
    financing_env$a_data_amc,
    financing_env$a_data_africa,
    financing_env$a_data_csc,
    financing_env$a_data_ifc,
    adm_cov_env$b_vxrate_change_lw,
    .GlobalEnv$refresh_date
)

source("consolidate/last_month/run_last_month.r")
source("consolidate/funding_tracker/run_funding_tracker.r")

last_month_env <- run_last_month()
funding_tracker_env <- run_funding_tracker()

# EXPORT

print(" > Exporting data outputs from pipeline to Excel workbooks...")
all_df <- list(
    "0_base_data" = eda_pin_env$a_data,
    "0_base_data_lm_change" = last_month_env$base_data_lm_change,
    "1_absorption_month" = adm_cov_env$d_absorption,
    "1_absorption_month_country" = adm_cov_env$combined,
    "1_cum_absorb_month_country" = adm_cov_env$d_absorption_country_new,
    "1_stock" = eda_adm_cov_env$timeseries,
    "1_adm_all_long" = adm_cov_env$b_vxrate_pub,
    "1_delivery_doses" = supply_env$supply_received_by_product,
    "1_funding_source" = finance_env$b_fin_fund_del_source,
    "8_dvr_cat" = consolidate_env$e_vrcat_all,
    "8_cov_com_hcw_all" = consolidate_env$e_cov_com_hcw_all,
    "8_cov_com_60p_all" = consolidate_env$e_cov_com_60p_all,
    "8_cov_com_hcw_csc" = consolidate_env$e_cov_com_hcw_csc,
    "8_cov_com_60p_csc" = consolidate_env$e_cov_com_60p_csc,
    "8_ndvp_tar_cat" = consolidate_env$e_ndvp_all,
    "9_values" = consolidate_env$z_values,
    "1_funding_long" = finance_env$b_fin_fund_del_long,
    "1_funding_urgent" = finance_env$base_fin_urg_fun_sum,
    "1_fund_urg_long" = finance_env$base_fin_urg_fun_long,
    "1_fund_cds_long" = finance_env$base_fin_cds_red,
    "1_fund_one_budget_tracker" = funding_tracker_env$base_one_budget_tracker,
    "1_fund_one_budget_cds" = funding_tracker_env$base_one_budget_cds,
    "1_fund_requests" = funding_tracker_env$base_requests
)

  write_xlsx(all_df, "data/output/230214_output_powerbi.xlsx")
  write_xlsx(financing_env$api, "data/output/230214_output_api.xlsx")
  write_xlsx(all_df, "data/output/output_master.xlsx")

print(" > Output exported to Excel successfully!")
