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

    names(df_original) = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y')
    names(df_new) = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y')

    a = df_original$A
    b = df_original$B
    c = df_original$C
    d = df_original$D
    e = df_original$E
    f = df_original$F
    g = df_original$G
    h = df_original$H
    i = df_original$I
    j = df_original$J
    k = df_original$K
    l = df_original$L
    m = df_original$M
    n = df_original$N
    o = df_original$O
    p = df_original$P
    q = df_original$Q
    r = df_original$R
    s = df_original$S
    t = df_original$T
    u = df_original$U
    v = df_original$V
    w = df_original$W
    x = df_original$X
    y = df_original$Y

    a_new = df_new$A
    b_new = df_new$B
    c_new = df_new$C
    d_new = df_new$D
    e_new = df_new$E
    f_new = df_new$F
    g_new = df_new$G
    h_new = df_new$H
    i_new = df_new$I
    j_new = df_new$J
    k_new = df_new$K
    l_new = df_new$L
    m_new = df_new$M
    n_new = df_new$N
    o_new = df_new$O
    p_new = df_new$P
    q_new = df_new$Q
    r_new = df_new$R
    s_new = df_new$S
    t_new = df_new$T
    u_new = df_new$U
    v_new = df_new$V
    w_new = df_new$W
    x_new = df_new$X
    y_new = df_new$Y
    originals <- list(a, b, c, d, e, f, g, h, i, j, k, l,
        m, n, o, p, q, r, s, t, u, v, w, x, y)
    news <- list(a_new, b_new, c_new, d_new, e_new, f_new,
        g_new, h_new, i_new, j_new, k_new, l_new, m_new,
        n_new, o_new, p_new, q_new, r_new, s_new, t_new,
        u_new, v_new, w_new, x_new, y_new)
    test <- 0
    for (sheet in 1:length(originals)) {
        print(paste0("Sheet: ",sheet))
        for (column in 1:ncol(originals[[sheet]])) {
            column_vector_original = data.frame(originals[[sheet]][column])
            column_vector_new = data.frame(news[[sheet]][column])
            name_of_column = names(column_vector_original)
            for (row in 1:nrow(column_vector_original)) {
                if (is.na(column_vector_original[row,1]) &
                    is.na(column_vector_new[row,1]) == FALSE) {
                    if (is.na(column_vector_original[row,1]) |
                        is.na(column_vector_new[row,1])) {
                        print("--------")
                        print(paste0("Difference in   ", name_of_column))
                        print(paste0("in row          ", as.character(row)))
                        print(paste0("Original value: ",
                            as.character(column_vector_original[row, 1])))
                        print(paste0("New value:      ",
                            as.character(column_vector_new[row, 1])))
                    } else {
                        if (column_vector_original[row, 1] !=
                        column_vector_new[row, 1]) {
                        print("--------")
                        print(paste0("Difference in   ", name_of_column))
                        print(paste0("in row          ", as.character(row)))
                        print(paste0("Original value: ",
                            as.character(column_vector_original[row, 1])))
                        print(paste0("New value:      ",
                            as.character(column_vector_new[row, 1])))
                        }
                    }
                }
            }
        }
    }
}

check_entire_file_for_changes(new_file = "data/output/220602_output_powerbi_after_changes_in_product_utilization.xlsx",
        original_file = "data/output/220602_output_powerbi_before_changes_in_product_utilization.xlsx")
