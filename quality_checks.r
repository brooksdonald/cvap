# Load required packages
library("readxl")
library("writexl")
library("countrycode")
library("dplyr")
library("lubridate")
library("openxlsx")
library("tidyverse")

# Ingesting current and past week datasets
## Current week
print(" > Ingesting current week data...")
current_week <- data.frame(read_excel("data/output/output_master.xlsx"))

print(" > Selecting columns needed for quality check...")
current_week <- select(
    current_week,
    c(
        "a_iso",
        "a_name_short",
        "a_covax_status",
        "adm_td",
        "adm_fv",
        "adm_fv_hcw",
        "adm_fv_60p",
        "cov_total_fv",
        "cov_hcw_fv",
        "cov_60p_fv",
        "dvr_4wk_td",
        "del_dose_total",
        "pu_del_rem"
    )
)
print(" > Done selecting columns needed from current week...")

# Filtering AMC countries from current_week
print(" > Filtering AMC countries from current week...")
amc_current <- filter(current_week, a_covax_status == "AMC")
print(" > Done.")

# Comparing the aggregate variables for anomalies
## Number of AMC92 reporting on HCW vaccination
print(" > Obtaining number of AMC92 reporting on HCW vaccination for current week...")
hcw_vax <- sum(!is.na(amc_current$adm_fv_hcw))
print(paste(" > The number of AMC92 reporting on HCW vaccination for current week is:", hcw_vax))
print(" > Done.")

## Number of AMC92 reporting on older adults (60p) vaccination
print(" > Obtaining number of AMC92 reporting on older adults (60p) vaccination for current week...")
old_adults <- sum(!is.na(amc_current$adm_fv_60p))
print(paste(" > The number of AMC92 reporting on older adults (60p) vaccination for current week is:", old_adults))
print(" > Done.")

## Past week
print(" > Ingesting past week data...")
past_week <- data.frame(read_excel("data/output/220519_output_powerbi.xlsx"))

print(" > Selecting columns needed for quality check...")
past_week <- select(
    past_week,
    c(
        "a_iso",
        "a_name_short",
        "a_covax_status",
        "adm_td",
        "adm_fv",
        "adm_fv_hcw",
        "adm_fv_60p",
        "cov_total_fv",
        "cov_hcw_fv",
        "cov_60p_fv",
        "dvr_4wk_td",
        "del_dose_total",
        "pu_del_rem"
    )
)
print(" > Done selecting columns needed from past week dataset...")

# Filtering AMC countries from past_week
print(" > Filtering AMC countries from past week...")
amc_past <- filter(past_week, a_covax_status == "AMC")
print(" > Done.")

## Number of AMC92 reporting on HCW vaccination
print(" > Obtaining number of AMC92 reporting on HCW vaccination for past week...")
hcw_vax <- sum(!is.na(amc_past$adm_fv_hcw))
print(paste(" > The number of AMC92 reporting on HCW vaccination for past week is:", hcw_vax))
print(" > Done.")

## Number of AMC92 reporting on older adults (60p) vaccination
print(" > Obtaining number of AMC92 reporting on older adults (60p) vaccination for past week...")
old_adults <- sum(!is.na(amc_past$adm_fv_60p))
print(paste(" > The number of AMC92 reporting on older adults (60p) vaccination for past week is:", old_adults))
print(" > Done.")

# Checking value difference between current and past week
## Joining the two dataframes
print(" > Joining current and past dataframes...")
df <- left_join(
    current_week,
    past_week,
    by = c("a_iso", "a_name_short", "a_covax_status")
)
print(" > Done joining both dataframes...")

# Get difference in value change in current from past week
print(" > Calculating difference in value change in current from past week...")
a_iso <- df$a_iso
a_name_short <- df$a_name_short
adm_td <- current_week$adm_td - past_week$adm_td
adm_fv <- current_week$adm_fv - past_week$adm_fv
adm_fv_hcw <- current_week$adm_fv_hcw - past_week$adm_fv_hcw
adm_fv_60p <- current_week$adm_fv_60p - past_week$adm_fv_60p
cov_total_fv <- current_week$cov_total_fv - past_week$cov_total_fv
cov_hcw_fv <- current_week$cov_hcw_fv - past_week$cov_hcw_fv
cov_60p_fv <- current_week$cov_60p_fv - past_week$cov_60p_fv
dvr_4wk_td <- current_week$dvr_4wk_td - past_week$dvr_4wk_td
del_dose_total <- current_week$del_dose_total - past_week$del_dose_total
pu_del_rem <- current_week$pu_del_rem - past_week$pu_del_rem
print(" > Done calculating difference in value change...")

# Create a df from the differences
print(" > Creating new df...")
df <- data.frame(
    a_iso,
    a_name_short,
    adm_td,
    adm_fv,
    adm_fv_hcw,
    adm_fv_60p,
    cov_total_fv,
    cov_hcw_fv,
    cov_60p_fv,
    dvr_4wk_td,
    del_dose_total,
    pu_del_rem
)
print(" > Done.")

# Exporting quality checks df to excel output
print(" > Exporting quality checks df to excel output...") 
quality_check_df <- list(
    "quality_checks" = df
)
write_xlsx(quality_check_df, "data/output/quality_check.xlsx")
print(" > Done.")