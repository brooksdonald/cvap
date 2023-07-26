load_admdvr <- function(dvr_data) {
  print(">> Carrying over adm / dvr data from src/dvr...")
  adm_dvr <- as.data.frame(dvr_env$dvr_data)
  adm_dvr$date <- as.Date(adm_dvr$date, format = "%Y-%m-%d")
  
  print(">> Selecting & renaming relevant current adm / dvr data...")
  adm_dvr <- adm_dvr %>%
    select(
      iso_code,
      date,
      total_doses,
      at_least_one_dose,
      fully_vaccinated,
      persons_booster_add_dose,
      rolling_4_week_avg_td,
      rolling_4_week_avg_fv,
      max_rolling_4_week_avg_td,
      rolling_4_week_avg_td_lastmonth,
      no_change_from_previous,
      at_least_one_dose_adj,
      fully_vaccinated_adj
    ) %>%
    rename(
      a_iso = iso_code,
      adm_date = date,
      adm_td = total_doses,
      adm_a1d = at_least_one_dose,
      adm_fv = fully_vaccinated,
      adm_booster = persons_booster_add_dose,
      dvr_4wk_td = rolling_4_week_avg_td,
      dvr_4wk_fv = rolling_4_week_avg_fv,
      dvr_4wk_td_max = max_rolling_4_week_avg_td,
      dvr_4wk_td_lm = rolling_4_week_avg_td_lastmonth,
      note_nochange = no_change_from_previous,
      adm_a1d_adj = at_least_one_dose_adj,
      adm_fv_adj = fully_vaccinated_adj
    )
  
  print(">> Done.")
  return(adm_dvr)
}

transform_admdvr <- function(adm_dvr, date_refresh) {
  print(">> Adding date fields to current adm / dvr data...")
  adm_dvr <- adm_dvr %>%
    mutate(
      adm_date_year = year(adm_date),
      adm_date_month = case_when(
        adm_date_year == 2021 ~ month(adm_date),
        adm_date_year == 2022 ~ month(adm_date) + 12,
        adm_date_year == 2023 ~ month(adm_date) + 24,
        TRUE ~ NA_real_
      ),
      adm_date_week = case_when(adm_date_year != 2020 ~ isoweek(adm_date),
                                TRUE ~ NA_real_)
    ) %>%
    filter(adm_date_year != 2020)
  
  print(">> Adding date-related flags to current adm / dvr data...")
  adm_dvr <- adm_dvr %>%
    group_by(a_iso, adm_date_week) %>%
    mutate(adm_date_maxweek = if_else(adm_date == max(adm_date), "Yes", "No")) %>%
    ungroup()
  
  adm_dvr <- adm_dvr %>%
    group_by(a_iso, adm_date_month) %>%
    mutate(adm_date_eom = if_else(adm_date == max(adm_date), "Yes", "No")) %>%
    ungroup()
  
  adm_dvr <- adm_dvr %>%
    group_by(a_iso) %>%
    mutate(adm_latest = if_else(adm_date == max(adm_date), "Yes", "No"),
           adm_is_current = case_when(
             adm_latest == "Yes" &
               (adm_date_week == isoweek(date_refresh) | adm_date_week == isoweek(date_refresh) - 1) ~ "Yes",
             TRUE ~ "No"),
           adm_last_week = if_else(adm_date == date_refresh - 14, "Yes", "No"),
           adm_last_month = if_else(adm_date == date_refresh - 35, "Yes", "No"),
           adm_two_month = if_else(adm_date == date_refresh - 63, "Yes", "No"),
           adm_date_lastmonth = case_when(
             adm_date_week == (max(adm_date_week) - 4) & adm_date_maxweek == "Yes" ~ "Yes",
             TRUE ~ "No")) %>%
    ungroup()
  
  print(">> Done.")
  return(adm_dvr)
}

subset_13jan <- function(adm_dvr, entity_characteristics) {
  print(">> Selecting necessary entity detail data...")
  ec_13jan <- select(entity_characteristics, c("a_iso", "date_13jan"))
  
  print(">> Merging entity detail data and filtering for 13 jan entries...")
  adm_dvr_13jan <- left_join(adm_dvr, ec_13jan, by = "a_iso") %>%
    select(a_iso,
           adm_date,
           date_13jan,
           adm_td,
           adm_a1d,
           adm_fv,
           adm_booster) %>%
    rename(adm_td_13jan = adm_td,
           adm_a1d_13jan = adm_a1d,
           adm_fv_13jan = adm_fv,
           adm_booster_13jan = adm_booster) %>%
    filter(adm_date == date_13jan) %>%
    select(-adm_date, -date_13jan)
  
  print(">> Done.")
  return(adm_dvr_13jan)
}

subset_amc <- function(adm_dvr, entity_characteristics) {
  print(">> Selecting necessary entity detail data...")
  ec_amc <- select(entity_characteristics, c("a_iso", "a_status_covax"))
  
  print(">> Merging entity detail data and filtering for AMC entries...")
  adm_dvr_amc <- left_join(adm_dvr, ec_amc, by = "a_iso") %>%
    filter(a_status_covax == "AMC" | a_iso == "GAB") %>%
    select(a_iso,
           adm_date,
           adm_td,
           adm_a1d,
           adm_fv,
           adm_booster,
           dvr_4wk_td)
  
  print(">> Done.")
  return(adm_dvr_amc)
}

subset_sep21 <- function(adm_dvr) {
  print(">> Creating end of September 2021 subset for target analysis...")
  adm_dvr_sep21 <- adm_dvr %>%
  filter(adm_date_eom == "Yes" & adm_date_month == 9 & adm_date_year == 2021)
  
  print(">> Done.")
  return(adm_dvr_sep21)
}

subset_dec21 <- function(adm_dvr) {
  print(">> Creating end of December 2021 subset for target analysis...")
  adm_dvr_dec21 <- adm_dvr %>%
    filter(adm_date_eom == "Yes" & adm_date_month == 12 & adm_date_year == 2021)
  
  print(">> Done.")
  return(adm_dvr_dec21)
}

subset_jun22 <- function(adm_dvr) {
  print(">> Creating end of June 2022 subset for target analysis...")
  adm_dvr_jun22 <- adm_dvr %>%
    filter(adm_date_eom == "Yes" & adm_date_month == 18 & adm_date_year == 2022)

  print(">> Done.")
  return(adm_dvr_jun22)
}

subset_latest <- function(adm_dvr) {
  print(">> Creating latest value subset...")
  adm_dvr_latest <- adm_dvr %>%
    filter(adm_latest == "Yes") %>%
    select(-adm_latest)
  
  print(">> Done.")
  return(adm_dvr_latest)
} 

transform_ts_adm <- function(adm_dvr, current_month) {
  print(">> Preparing administration timeseries figures...")
  adm_ts <- adm_dvr %>%
    filter(adm_date_eom == "Yes") %>%
    select(
      a_iso,
      adm_date_month,
      adm_td,
      adm_a1d,
      adm_fv,
      adm_booster
    ) %>%
    rename(
      adm_td_cumul = adm_td,
      adm_a1d_cumul = adm_a1d, 
      adm_fv_cumul = adm_fv,
      adm_booster_cumul = adm_booster
    ) %>%
    arrange(adm_date_month) %>%
    group_by(a_iso) %>%
    mutate(adm_td_add = adm_td_cumul - lag(adm_td_cumul),
           adm_a1d_add = adm_a1d_cumul - lag(adm_a1d_cumul),
           adm_fv_add = adm_fv_cumul - lag(adm_fv_cumul),
           adm_booster_add = adm_booster_cumul - lag(adm_booster_cumul)) %>%
    mutate(adm_td_add = coalesce(adm_td_add, adm_td_cumul),
           adm_a1d_add = coalesce(adm_a1d_add, adm_a1d_cumul),
           adm_fv_add = coalesce(adm_fv_add, adm_fv_cumul),
           adm_booster_add = coalesce(adm_booster_add, adm_booster_cumul))

  ## Note: list of months is automatically generated from "2021-01" to month of refresh_date
  adm_ts$month_name <- helper_mapping_months(adm_ts$adm_date_month, current_month)
  adm_ts_wide <- select(adm_ts, -c("adm_date_month"))
  
  print(">> Splitting wide-form timeseries into add and cumul dataframes...")
  adm_ts_wide_add <- adm_ts_wide %>%
    select(a_iso,
           month_name,
           adm_td_add,
           adm_a1d_add,
           adm_fv_add,
           adm_booster_add) %>%
    mutate(measure = "additive") %>%
    rename(adm_td = adm_td_add,
           adm_a1d = adm_a1d_add,
           adm_fv = adm_fv_add,
           adm_booster = adm_booster_add)
  
  adm_ts_wide_cumul <- adm_ts_wide %>%
    select(a_iso,
           month_name,
           adm_td_cumul,
           adm_a1d_cumul,
           adm_fv_cumul,
           adm_booster_cumul) %>%
    mutate(measure = "cumulative") %>%
    rename(adm_td = adm_td_cumul,
           adm_a1d = adm_a1d_cumul,
           adm_fv = adm_fv_cumul,
           adm_booster = adm_booster_cumul)
  
  print(">> Preparing long-form administration timeseries...")
  adm_ts_long <- rbind(adm_ts_wide_add, adm_ts_wide_cumul) %>%
    gather(key = type, value = value, -a_iso, -measure, -month_name)
  
  print(">> Preparing administration timeseries datalist...")
  datalist <- list("adm_ts_long" = adm_ts_long,
                   "adm_ts_wide" = adm_ts_wide)
  
  print(">> Done.")
  return(datalist)
}