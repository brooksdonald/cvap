# Load required packages
library("tidyverse")
library("readxl")
library("writexl")
library("countrycode")
library("openxlsx")

# Ingesting current and past week datasets

## Current week
print(" > Ingesting current week data...")
current_week <- data.frame(read_excel("data/output/output_master_cw.xlsx"))

print(" > Selecting columns needed for quality check...")
current_week <- select(
    current_week,
    c(
        "a_iso",
        "a_name_short",
        "a_covax_status",
        "a_who_region",
        "a_csc_status",
        "adm_fv_gen_repstat",
        "adm_td",
        "adm_fv",
        "adm_fv_hcw",
        "adm_fv_60p",
        "adm_booster",
        "del_dose_total",
        "adm_fv_hcw_repstat",
        "adm_fv_60p_repstat"
    )
)
print(" > Done.")
print(" > Renaming current week's column names...")
cw <- select(
    current_week,
    c(
        "a_iso",
        "a_name_short",
        "a_covax_status",
        "a_who_region",
        "a_csc_status",
        "adm_fv_gen_repstat",
        "adm_td",
        "adm_fv",
        "adm_fv_hcw",
        "adm_fv_60p",
        "adm_booster",
        "del_dose_total",
        "adm_fv_hcw_repstat",
        "adm_fv_60p_repstat"
    )
)
colnames(cw) <- c(
    "a_iso",
    "a_name_short",
    "a_covax_status",
    "a_who_region",
    "a_csc_status",
    "cw_adm_fv_gen_repstat",
    "cw_adm_td",
    "cw_adm_fv",
    "cw_adm_fv_hcw",
    "cw_adm_fv_60p",
    "cw_adm_booster",
    "cw_del_dose_total",
    "cw_adm_fv_hcw_repstat",
    "cw_adm_fv_60p_repstat"
)
print(" > Done.")

# No. of countries reporting on HCW, 60P & Gender

## Filtering AMC countries from current_week
print(" > Filtering AMC countries from current week...")
amc_current <- filter(cw, a_covax_status == "AMC")
amc_current_rep <- amc_current
amc_current_no_rep <- amc_current
print(" > Done.")

## Number of AMC92 reporting on HCW vaccination
print(" > Obtaining number of AMC92 reporting $ not reporting on HCW vaccination for current week...")
amc_current_rep$hcw_vax_cw <- sum(amc_current_rep$cw_adm_fv_hcw_repstat == "Reporting")
amc_current_no_rep$hcw_vax_cw <- sum(amc_current_rep$cw_adm_fv_hcw_repstat == "Not reporting")
print(" > Done.")

## Number of AMC92 reporting on older adults (60p) vaccination
print(" > Obtaining number of AMC92 reporting & not reporting on older adults (60p) vaccination for current week...")
amc_current_rep$old_adults_cw <- sum(amc_current_rep$cw_adm_fv_60p_repstat == "Reporting")
amc_current_no_rep$old_adults_cw <- sum(amc_current_no_rep$cw_adm_fv_60p_repstat == "Not reporting")
print(" > Done.")

## Number of AMC92 reporting on vaccination coverage disaggregated by gender
print(" > Obtaining number of AMC92 reporting on vaccination coverage disaggregated by gender for current week...")
amc_current_rep$gender_disag_cw <- sum(amc_current_rep$cw_adm_fv_gen_repstat == "Reporting")
amc_current_no_rep$gender_disag_cw <- sum(amc_current_no_rep$cw_adm_fv_gen_repstat == "Not reporting")

print(" > Done.")

## Consolidate AMC reporting numbers to a df
amc_current_rep <- amc_current_rep[1, c('hcw_vax_cw', 'old_adults_cw', 'gender_disag_cw')]
amc_current_no_rep <- amc_current_no_rep[1, c('hcw_vax_cw', 'old_adults_cw', 'gender_disag_cw')]

## Past week
print(" > Ingesting past week data...")
past_week <- data.frame(read_excel("data/output/output_master_pw.xlsx"))

print(" > Selecting columns needed for quality check...")
past_week <- select(
    past_week,
    c(
        "a_iso",
        "a_name_short",
        "a_covax_status",
        "a_who_region",
        "a_csc_status",
        "adm_fv_gen_repstat",
        "adm_td",
        "adm_fv",
        "adm_fv_hcw",
        "adm_fv_60p",
        "adm_booster",
        "del_dose_total",
        "adm_fv_hcw_repstat",
        "adm_fv_60p_repstat"
    )
)
print(" > Done.")
print(" > Renaming past week's column names...")
pw <- select(
    past_week,
    c(
        "a_iso",
        "a_name_short",
        "a_covax_status",
        "a_who_region",
        "a_csc_status",
        "adm_fv_gen_repstat",
        "adm_td",
        "adm_fv",
        "adm_fv_hcw",
        "adm_fv_60p",
        "adm_booster",
        "del_dose_total",
        "adm_fv_hcw_repstat",
        "adm_fv_60p_repstat"
    )
)
colnames(pw) <- c(
    "a_iso",
    "a_name_short",
    "a_covax_status",
    "a_who_region",
    "a_csc_status",
    "pw_adm_fv_gen_repstat",
    "pw_adm_td",
    "pw_adm_fv",
    "pw_adm_fv_hcw",
    "pw_adm_fv_60p",
    "pw_adm_booster",
    "pw_del_dose_total",
    "pw_adm_fv_hcw_repstat",
    "pw_adm_fv_60p_repstat"
)
print(" > Done.")

## Filtering AMC countries from past_week
print(" > Filtering AMC countries from past week...")
amc_past <- filter(pw, a_covax_status == "AMC")
amc_past_rep <- amc_past
amc_past_no_rep <- amc_past
print(" > Done.")

## Number of AMC92 reporting & not reporting on HCW vaccination
print(" > Obtaining number of AMC92 reporting & not reporting on HCW vaccination for past week...")
amc_past_rep$hcw_vax_pw <- sum(amc_past_rep$pw_adm_fv_hcw_repstat == "Reporting")
amc_past_no_rep$hcw_vax_pw <- sum(amc_past_rep$pw_adm_fv_hcw_repstat == "Not reporting")
print(" > Done.")

## Number of AMC92 reporting on older adults (60p) vaccination
print(" > Obtaining number of AMC92 reporting & not reporting on older adults (60p) vaccination for past week...")
amc_past_rep$old_adults_pw <- sum(amc_past_rep$pw_adm_fv_60p_repstat == "Reporting")
amc_past_no_rep$old_adults_pw <- sum(amc_past_no_rep$pw_adm_fv_60p_repstat == "Not reporting")
print(" > Done.")

## Number of AMC92 reporting on vaccination coverage disaggregated by gender
print(" > Obtaining number of AMC92 reporting on vaccination coverage disaggregated by gender for past week...")
amc_past_rep$gender_disag_pw <- sum(amc_past_rep$pw_adm_fv_gen_repstat == "Reporting")
amc_past_no_rep$gender_disag_pw <- sum(amc_past_no_rep$pw_adm_fv_gen_repstat == "Not reporting")

## Consolidate AMC reporting numbers to a df
amc_past_rep <- amc_past_rep[1, c('hcw_vax_pw', 'old_adults_pw', 'gender_disag_pw')]
amc_past_no_rep <- amc_past_no_rep[1, c('hcw_vax_pw', 'old_adults_pw', 'gender_disag_pw')]

## Merging current & past week reporting numbers
reporting_numbers <- left_join(
    amc_current_rep,
    amc_past_rep,
    by = character()
)

## Re-arrange reporting numbers
reporting_numbers <- select(
    reporting_numbers,
    c(
        "hcw_vax_cw",
        "hcw_vax_pw",
        "old_adults_cw",
        "old_adults_pw",
        "gender_disag_cw",
        "gender_disag_pw"
    )
)

## Merging current & past week none reporting numbers
no_rep_numbers <- left_join(
    amc_current_no_rep,
    amc_past_no_rep,
    by = character()
)

## Re-arrange none reporting countries
no_rep_numbers <- select(
    no_rep_numbers,
    c(
        "hcw_vax_cw",
        "hcw_vax_pw",
        "old_adults_cw",
        "old_adults_pw",
        "gender_disag_cw",
        "gender_disag_pw"
    )
)

# Merge both cw and pw dataframes
df_joined <- left_join(
    cw,
    pw,
    by = c(
        "a_iso",
        "a_name_short",
        "a_covax_status",
        "a_who_region",
        "a_csc_status"
    )
)

# Rearrange for easy visualization
df_joined <- select(
    df_joined,
    c(
        "a_name_short",
        "a_covax_status",
        "a_who_region",
        "a_csc_status",
        "cw_adm_td",
        "pw_adm_td",
        "cw_adm_fv",
        "pw_adm_fv",
        "cw_adm_fv_hcw",
        "pw_adm_fv_hcw",
        "cw_adm_fv_60p",
        "pw_adm_fv_60p",
        "cw_adm_booster",
        "pw_adm_booster",
        "cw_del_dose_total",
        "pw_del_dose_total",
        "cw_adm_fv_gen_repstat",
        "pw_adm_fv_gen_repstat"
    )
)

## Calculating the difference between numeric values
df_joined$diff_adm_td <- df_joined$cw_adm_td - df_joined$pw_adm_td
df_joined$diff_adm_fv <- df_joined$cw_adm_fv - df_joined$cw_adm_fv
df_joined$diff_adm_fv_hcw <- df_joined$cw_adm_fv_hcw - df_joined$pw_adm_fv_hcw
df_joined$diff_adm_fv_60p <- df_joined$cw_adm_fv_60p - df_joined$pw_adm_fv_60p
df_joined$diff_adm_booster <- df_joined$cw_adm_booster - df_joined$pw_adm_booster
df_joined$diff_del_dose_total <- df_joined$cw_del_dose_total - df_joined$pw_del_dose_total

# Flag negative values
df_joined <- df_joined %>%
    mutate(td_neg_flag = if_else(diff_adm_td < 0, "True", "False")) %>%
    mutate(fv_neg_flag = if_else(diff_adm_fv < 0, "True", "False")) %>%
    mutate(hcw_neg_flag = if_else(diff_adm_td < 0, "True", "False")) %>%
    mutate(fv_60p_neg_flag = if_else(diff_adm_fv_60p < 0, "True", "False")) %>%
    mutate(booster_neg_flag = if_else(diff_adm_booster < 0, "True", "False")) %>%
    mutate(del_dose_neg_flag = if_else(diff_del_dose_total < 0, "True", "False"))

# Flag positive values from last week/last month
df_joined <- df_joined %>%
    mutate(td_pos_flag = if_else(((cw_adm_td - pw_adm_td)/cw_adm_td) > 0.25, "True", "False")) %>%
    mutate(fv_pos_flag = if_else(((cw_adm_fv - pw_adm_fv)/cw_adm_fv) > 0.25, "True", "False")) %>%
    mutate(hcw_pos_flag = if_else(((cw_adm_fv_hcw - pw_adm_fv_hcw)/cw_adm_fv_hcw) > 0.25, "True", "False")) %>%
    mutate(fv_60p_pos_flag = if_else(((cw_adm_fv_60p - pw_adm_fv_60p)/cw_adm_fv_60p) > 0.25, "True", "False")) %>%
    mutate(booster_pos_flag = if_else(((cw_adm_booster - pw_adm_booster)/cw_adm_booster) > 0.25, "True", "False")) %>%
    mutate(del_dose_pos_flag = if_else(((cw_del_dose_total - pw_del_dose_total)/cw_del_dose_total) > 0.25, "True", "False"))


## Re-organizing columns in df_joined
print(" > Re-organizing columns in df_joined.")
df_joined <- select(
    df_joined,
    c(
        "a_name_short",
        "a_covax_status",
        "a_who_region",
        "a_csc_status",
        "cw_adm_td",
        "pw_adm_td",
        "diff_adm_td",
        "td_neg_flag",
        "td_pos_flag",
        "cw_adm_fv",
        "pw_adm_fv",
        "diff_adm_fv",
        "fv_neg_flag",
        "fv_pos_flag",
        "cw_adm_fv_hcw",
        "pw_adm_fv_hcw",
        "diff_adm_fv_hcw",
        "hcw_neg_flag",
        "hcw_pos_flag",
        "cw_adm_fv_60p",
        "pw_adm_fv_60p",
        "diff_adm_fv_60p",
        "fv_60p_neg_flag",
        "fv_60p_pos_flag",
        "cw_adm_booster",
        "pw_adm_booster",
        "diff_adm_booster",
        "booster_neg_flag",
        "booster_pos_flag",
        "cw_del_dose_total",
        "pw_del_dose_total",
        "diff_del_dose_total",
        "del_dose_neg_flag",
        "del_dose_pos_flag"
    )
)
print(" > Done.")

# Combining reporting numbers data fames
## First row is reporting countries, second row is non-repoting numbers
rep_numbers <- rbind(reporting_numbers, no_rep_numbers)

# Exporting combined numbers to an excel output
print(" > Exporting quality checks df to excel output...")
quality_check_df <- list(
    "Combined_numbers" = df_joined,
    "Reporting_status" = rep_numbers
)
write_xlsx(quality_check_df, "data/output/QC.xlsx")
print(" > Done.")