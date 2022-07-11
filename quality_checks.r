#  Clear environment
rm(list = ls())

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
        "a_who_region",
        "adm_td",
        "adm_fv",
        "adm_fv_hcw",
        "adm_fv_60p",
        "cov_total_fv",
        "cov_hcw_fv",
        "cov_60p_fv",
        "dvr_4wk_td",
        "del_dose_total",
        "pu_del_rem",
        "a_pop_male",
        "a_pop_female",
        "a_pop",
        "a_pop_older"
    )
)
print(" > Done selecting columns needed from current week...")

cw <- select(
    current_week,
    c(
        "a_iso",
        "a_name_short",
        "a_covax_status",
        "a_who_region",
        "adm_td",
        "adm_fv",
        "adm_fv_hcw",
        "adm_fv_60p",
        "cov_total_fv",
        "cov_hcw_fv",
        "cov_60p_fv",
        "dvr_4wk_td",
        "del_dose_total",
        "pu_del_rem",
        "a_pop_male",
        "a_pop_female",
        "a_pop",
        "a_pop_older"
    )
)
colnames(cw) <- c(
    "a_iso",
    "a_name_short",
    "a_covax_status",
    "a_who_region",
    "cw_adm_td",
    "cw_adm_fv",
    "cw_adm_fv_hcw",
    "cw_adm_fv_60p",
    "cw_cov_total_fv",
    "cw_cov_hcw_fv",
    "cw_cov_60p_fv",
    "cw_dvr_4wk_td",
    "cw_del_dose_total",
    "cw_pu_del_rem",
    "cw_a_pop_male",
    "cw_a_pop_female",
    "cw_a_pop",
    "cw_a_pop_older"
)

# Filtering AMC countries from current_week
print(" > Filtering AMC countries from current week...")
amc_current <- filter(current_week, a_covax_status == "AMC")
print(" > Done.")

## Number of AMC92 reporting on HCW vaccination
print(" > Obtaining number of AMC92 reporting on HCW vaccination for current week...")
hcw_vax_cw <- sum(!is.na(amc_current$adm_fv_hcw))
print(paste(" > The number of AMC92 reporting on HCW vaccination for current week is:", hcw_vax_cw))
print(" > Done.")

## Number of AMC92 reporting on older adults (60p) vaccination
print(" > Obtaining number of AMC92 reporting on older adults (60p) vaccination for current week...")
old_adults_cw <- sum(!is.na(amc_current$adm_fv_60p))
print(paste(" > The number of AMC92 reporting on older adults (60p) vaccination for current week is:", old_adults_cw))
print(" > Done.")

## Number of AMC92 reporting on gender-disaggregated - males
males_cw <- sum(!is.na(amc_current$a_pop_male))
print(paste(" > The number of AMC92 reporting on gender-disaggregated for males in current week is:", males_cw))
print(" > Done.")

## Number of AMC92 reporting on gender-disaggregated - females
females_cw <- sum(!is.na(amc_current$a_pop_female))
print(paste(" > The number of AMC92 reporting on gender-disaggregated for females in current week is:", females_cw))
print(" > Done.")

## Past week
print(" > Ingesting past week data...")
past_week <- data.frame(read_excel("data/output/220705_output_powerbi.xlsx"))

print(" > Selecting columns needed for quality check...")
past_week <- select(
    past_week,
    c(
        "a_iso",
        "a_name_short",
        "a_covax_status",
        "a_who_region",
        "adm_td",
        "adm_fv",
        "adm_fv_hcw",
        "adm_fv_60p",
        "cov_total_fv",
        "cov_hcw_fv",
        "cov_60p_fv",
        "dvr_4wk_td",
        "del_dose_total",
        "pu_del_rem",
        "a_pop_male",
        "a_pop_female",
        "a_pop",
        "a_pop_older"
    )
)
print(" > Done selecting columns needed from past week dataset...")

pw <- select(
    past_week,
    c(
        "a_iso",
        "a_name_short",
        "a_covax_status",
        "a_who_region",
        "adm_td",
        "adm_fv",
        "adm_fv_hcw",
        "adm_fv_60p",
        "cov_total_fv",
        "cov_hcw_fv",
        "cov_60p_fv",
        "dvr_4wk_td",
        "del_dose_total",
        "pu_del_rem",
        "a_pop_male",
        "a_pop_female",
        "a_pop",
        "a_pop_older"
    )
)
colnames(pw) <- c(
    "a_iso",
    "a_name_short",
    "a_covax_status",
    "a_who_region",
    "pw_adm_td",
    "pw_adm_fv",
    "pw_adm_fv_hcw",
    "pw_adm_fv_60p",
    "pw_cov_total_fv",
    "pw_cov_hcw_fv",
    "pw_cov_60p_fv",
    "pw_dvr_4wk_td",
    "pw_del_dose_total",
    "pw_pu_del_rem",
    "pw_a_pop_male",
    "pw_a_pop_female",
    "pw_a_pop",
    "pw_a_pop_older"
)

# Filtering AMC countries from past_week
print(" > Filtering AMC countries from past week...")
amc_past <- filter(past_week, a_covax_status == "AMC")
print(" > Done.")

## Number of AMC92 reporting on HCW vaccination
print(" > Obtaining number of AMC92 reporting on HCW vaccination for past week...")
hcw_vax_pw <- sum(!is.na(amc_past$adm_fv_hcw))
print(paste(" > The number of AMC92 reporting on HCW vaccination for past week is:", hcw_vax_pw))
print(" > Done.")

## Number of AMC92 reporting on older adults (60p) vaccination
print(" > Obtaining number of AMC92 reporting on older adults (60p) vaccination for past week...")
old_adults_pw <- sum(!is.na(amc_past$adm_fv_60p))
print(paste(" > The number of AMC92 reporting on older adults (60p) vaccination for past week is:", old_adults_pw))
print(" > Done.")

## Number of AMC92 reporting on gender-disaggregated - males
males_pw <- sum(!is.na(amc_past$a_pop_male))
print(paste(" > The number of AMC92 reporting on gender-disaggregated for males in past week is:", males_pw))
print(" > Done.")

## Number of AMC92 reporting on gender-disaggregated - females
females_pw <- sum(!is.na(amc_past$a_pop_female))
print(paste(" > The number of AMC92 reporting on gender-disaggregated for females in past week is:", females_pw))
print(" > Done.")

# Checking value difference between current and past week
## Joining the two dataframes
df_combined <- left_join(
    cw,
    pw,
    by = c("a_iso", "a_name_short", "a_covax_status", "a_who_region")
)

print(" > Joining current and past dataframes...")
df <- left_join(
    current_week,
    past_week,
    by = c("a_iso", "a_name_short", "a_covax_status", "a_who_region")
)
print(" > Done joining both dataframes...")

#Rearrange for easy visualization
df_combined <- select(df_combined, c(
  "a_name_short",
  "a_covax_status",
  "a_who_region",
  "cw_adm_td",
  "pw_adm_td",
  "cw_adm_fv",
  "pw_adm_fv",
  "cw_adm_fv_hcw",
  "pw_adm_fv_hcw",
  "cw_adm_fv_60p",
  "pw_adm_fv_60p",
  "cw_cov_total_fv",
  "pw_cov_total_fv",
  "cw_cov_hcw_fv",
  "pw_cov_hcw_fv",
  "cw_cov_60p_fv",
  "pw_cov_60p_fv",
  "cw_dvr_4wk_td",
  "pw_dvr_4wk_td",
  "cw_del_dose_total",
  "pw_del_dose_total",
  "cw_pu_del_rem",
  "pw_pu_del_rem"
))

# Get difference in value change in current from past week
print(" > Calculating difference in value change in current from past week...")
a_iso <- df$a_iso
a_name_short <- df$a_name_short
a_who_region <- df$a_who_region
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
a_pop <- current_week$a_pop - past_week$a_pop
a_pop_older <- current_week$a_pop_older - past_week$a_pop_older
print(" > Done calculating difference in value change...")

# Create a df from the differences
print(" > Creating new df...")
df <- data.frame(
    a_iso,
    a_name_short,
    a_who_region,
    adm_td,
    adm_fv,
    adm_fv_hcw,
    adm_fv_60p,
    cov_total_fv,
    cov_hcw_fv,
    cov_60p_fv,
    dvr_4wk_td,
    del_dose_total,
    pu_del_rem,
    a_pop,
    a_pop_older
)
print(" > Done.")

# Exporting quality checks df to excel output
print(" > Exporting quality checks df to excel output...")
quality_check_df <- list(
    "Combined_numbers" = df_combined,
    "Difference_in_Numbers" = df
    # "HCW_vx_in_current_week" = hcw_vax_cw,
    # "Older_adults_in_current_week" = old_adults_cw,
    # "Males_in_current_week" = males_cw,
    # "females_in_current_week" = females_cw,
    # "HCW_vx_past_week" = hcw_vax_pw,
    # "Older_adults_past_week" = old_adults_pw,
    # "Males_in_past_week" = males_pw,
    # "females_in_past_week" = females_pw
)
write_xlsx(quality_check_df, "data/output/quality_check.xlsx")
print(" > Done.")
