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
hcw_vax <- sum(!is.na(amc_current$adm_fv_hcw))
print(paste(" > The number of AMC92 reporting on HCW vaccination for current week is:", hcw_vax))
print(" > Done.")

## Number of AMC92 reporting on older adults (60p) vaccination
print(" > Obtaining number of AMC92 reporting on older adults (60p) vaccination for current week...")
old_adults <- sum(!is.na(amc_current$adm_fv_60p))
print(paste(" > The number of AMC92 reporting on older adults (60p) vaccination for current week is:", old_adults))
print(" > Done.")

## Number of AMC92 reporting on gender-disaggregated - males
males <- sum(!is.na(amc_current$a_pop_male))
print(paste(" > The number of AMC92 reporting on gender-disaggregated for males in current week is:", males))
print(" > Done.")

## Number of AMC92 reporting on gender-disaggregated - females
females <- sum(!is.na(amc_current$a_pop_female))
print(paste(" > The number of AMC92 reporting on gender-disaggregated for females in current week is:", females))
print(" > Done.")

## Past week
print(" > Ingesting past week data...")
past_week <- data.frame(read_excel("data/output/220525_output_powerbi.xlsx"))

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
hcw_vax <- sum(!is.na(amc_past$adm_fv_hcw))
print(paste(" > The number of AMC92 reporting on HCW vaccination for past week is:", hcw_vax))
print(" > Done.")

## Number of AMC92 reporting on older adults (60p) vaccination
print(" > Obtaining number of AMC92 reporting on older adults (60p) vaccination for past week...")
old_adults <- sum(!is.na(amc_past$adm_fv_60p))
print(paste(" > The number of AMC92 reporting on older adults (60p) vaccination for past week is:", old_adults))
print(" > Done.")

## Number of AMC92 reporting on gender-disaggregated - males
males <- sum(!is.na(amc_past$a_pop_male))
print(paste(" > The number of AMC92 reporting on gender-disaggregated for males in past week is:", males))
print(" > Done.")

## Number of AMC92 reporting on gender-disaggregated - females
females <- sum(!is.na(amc_past$a_pop_female))
print(paste(" > The number of AMC92 reporting on gender-disaggregated for females in past week is:", females))
print(" > Done.")

# Checking value difference between current and past week
## Joining the two dataframes
df_combined <- left_join(
    cw,
    pw,
    by = c("a_iso", "a_name_short", "a_covax_status")
)

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
a_pop <- current_week$a_pop - past_week$a_pop
a_pop_older <- current_week$a_pop_older - past_week$a_pop_older
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
)
write_xlsx(quality_check_df, "data/output/quality_check.xlsx")
print(" > Done.")

check_entire_file_for_changes <- function(new_file = "data/output/220602_output_powerbi.xlsx",
        original_file = "data/output/220609_output_powerbi.xlsx") {
    sheets_original <- readxl::excel_sheets(original_file)
    df_original <- lapply(sheets_original, function(X) readxl::read_excel(original_file, sheet = X))
    #if(!tibble) df_original <- lapply(df_original, as.data.frame)
    names(df_original) <- sheets_original

    sheets_new <- readxl::excel_sheets(new_file)
    df_new <- lapply(sheets_new,
        function(X) readxl::read_excel(new_file, sheet = X))
    #if(!tibble) df_new <- lapply(df_new, as.data.frame)
    names(df_new) <- sheets_new

    differences = 0

    if (sum(sheets_original != sheets_new) > 0) {
        print("Sheet missing or do not match!")
    } else {
        sheets_list <- list()
        sheets_list_new <- list()
        for (sheet in df_original) {
            sheets_list <- append(sheets_list, sheet)
        }
        for (sheet in df_new) {
            sheets_list_new <- append(sheets_list_new, sheet)
        }
        if (length(sheets_list) != length(sheets_list_new)) {
            print("differing number of columns")
        } else {
            for (column in c(1:length(sheets_list))) {
                difference = FALSE
                for (row in 1:length(sheets_list[column])) {
                    if ((!is.na(unlist(sheets_list[column])[row])) &
                        (sum(unlist(sheets_list[column])[row] !=
                        unlist(sheets_list_new[column])[row]) > 0)) {
                            difference = TRUE
                        }
                }
                if (difference == TRUE) {
                    differences = differences + 1
                    print("difference in ", str(names(sheets_list[column])))
                }
            }
        }
        if (differences == 0) print("no differences")
    }
}