# Extract data ------------------------------------------------------------

# Load base data
raw_data_time <- data.frame(
  read_excel("data/output/output_master.xlsx",
             sheet = "1_adm_all_long"))

data_time <- raw_data_time %>%
  select(a_iso,
         adm_date,
         a_status_who,
         a_region_who,
         a_income_group,
         dvr_4wk_td,
         dvr_4wk_td_per) %>%
  filter(a_status_who == "Member State") %>%
  select(-a_status_who)

dvr_inc <- data_time %>%
  filter(a_income_group != "Other",
         adm_date <= as.Date("2023-12-31")) %>%
  group_by(adm_date, a_income_group) %>%
  summarize(dvr = sum(dvr_4wk_td, na.rm = TRUE)) %>%
  ungroup() %>% 
  rename(type = a_income_group) %>%
  mutate(group = "income")

dvr_reg <- data_time %>%
  filter(adm_date <= as.Date("2023-12-31")) %>%
  group_by(adm_date, a_region_who) %>%
  summarize(dvr = sum(dvr_4wk_td, na.rm = TRUE)) %>%
  ungroup() %>% 
  rename(type = a_region_who) %>%
  mutate(group = "region")

dvr <- rbind(dvr_inc, dvr_reg) %>%
  mutate(pop = case_when(
    type == "HIC" ~ pop_total_hic,
    type == "UMIC" ~ pop_total_umic,
    type == "LMIC" ~ pop_total_lmic,
    type == "LIC" ~ pop_total_lic,
    type == "AFR" ~ pop_total_afr,
    type == "AMR" ~ pop_total_amr,
    type == "EMR" ~ pop_total_emr,
    type == "EUR" ~ pop_total_eur,
    type == "SEAR" ~ pop_total_sear,
    type == "WPR" ~ pop_total_wpr,
    NA ~ TRUE
  ),
  dvr_per = dvr / pop)

# Max aggregate dvr
dvr_max <- dvr %>%
  group_by(type) %>%
  filter(dvr_per == max(dvr_per)) %>%
  select(-pop,
         -dvr)

knitr::kable(dvr_max) %>%
  kable_styling(bootstrap_options = c("hover", "condensed")) %>%
  column_spec(1, width = "25%")


# Max, min, average -------------------------------------------------------

data_time_mmm <- raw_data_time %>%
  select(a_iso,
         adm_date,
         a_status_who,
         a_region_who,
         a_income_group,
         dvr_4wk_td,
         dvr_4wk_td_per) %>%
  filter(a_status_who == "Member State") %>%
  select(-a_status_who) %>%
  group_by(adm_date, a_income_group) %>%  # Group by income level and date
  summarise(
    max_rate = max(dvr_4wk_td_per, na.rm = TRUE),  # Maximum rate
    min_rate = min(dvr_4wk_td_per, na.rm = TRUE),  # Minimum rate
    avg_rate = mean(dvr_4wk_td_per, na.rm = TRUE),  # Average rate
    perc_10 = quantile(dvr_4wk_td_per, 0.10, na.rm = TRUE),  # 10th percentile
    perc_90 = quantile(dvr_4wk_td_per, 0.90, na.rm = TRUE),  # 90th percentile
    perc_25 = quantile(dvr_4wk_td_per, 0.25, na.rm = TRUE),  # 10th percentile
    perc_75 = quantile(dvr_4wk_td_per, 0.75, na.rm = TRUE),   # 10th percentile
    sd_rate = sd(dvr_4wk_td_per, na.rm = TRUE)
  ) %>%
  mutate(
    lower_bound = avg_rate - sd_rate,  # Mean - 1 SD
    upper_bound = avg_rate + sd_rate   # Mean + 1 SD
  ) %>%
  ungroup()

