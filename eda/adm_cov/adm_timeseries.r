
merge_timeseries <- function(a_data, combined_three, target_hcwold, overall_fin_cumul_long){
  
  # Select necessary entity characteristics
  a_data_temp_ts <- a_data %>%
    select(
      a_iso,
      a_pop_hcw,
      a_pop_older,
      adm_target_hcw
    )
  print(" > Adding month date...")
  # Prepare combined time series
  combined_three <- combined_three %>%
    mutate(month_name = as.Date(paste0(as.character(month_name), '-01'), format = '%Y-%m-%d')) %>%
    mutate(adm_date_month = if_else(year(month_name) == 2022, 
                                    as.numeric(month(month_name) + 12),
                                    as.numeric(month(month_name)))) %>%
    mutate(cov_total_fv = adm_fv / a_pop,
           cov_total_a1d = adm_a1d / a_pop,
           cov_total_booster = adm_booster / a_pop)

  print(" > Join dataframes...")
  # Merge finance timeseries data with HCW population data frame
  timeseries <- left_join(combined_three, a_data_temp_ts, by = c("iso" = "a_iso"), copy = TRUE) %>%
    left_join(., target_hcwold, by = c("iso" = "ISO_3_CODE", "adm_date_month" = "adm_date_month"), copy = TRUE) %>%
    left_join(., overall_fin_cumul_long, by = c("iso" = "ISO.Code", "month_name"), copy = TRUE)

  print(" > Calculating HCW specific fields...")
  # Calculate HCW flag & diff
  timeseries <- timeseries %>%
    mutate(hcw_flag = if_else(
      a_pop_hcw > adm_target_hcw,
      "Yes",
      NA_character_)) %>%
    mutate(hcw_diff = pmax(a_pop_hcw - adm_target_hcw, 0, na.rm = TRUE))
  
  # Calculate administration figures capped by HCW population
  timeseries <- timeseries %>%
    mutate(adm_a1d_hcw_cap = pmin(N_VACC_DOSE1, a_pop_hcw),
           adm_cps_hcw_cap = pmin(N_VACC_LAST_DOSE, a_pop_hcw),
           adm_booster_hcw_cap = pmin(N_VACC_BOOSTER_DOSE, a_pop_hcw))
  
  # Calculate HCW coverage
  timeseries <- timeseries %>%
    mutate(a_pop_hcw = as.numeric(a_pop_hcw),
           adm_a1d_hcw_cap = as.numeric(adm_a1d_hcw_cap),
           adm_cps_hcw_cap = as.numeric(adm_cps_hcw_cap),
           adm_booster_hcw_cap = as.numeric(adm_booster_hcw_cap),
           cov_a1d_hcw_cap = pmin(adm_a1d_hcw_cap / a_pop_hcw, 1),
           cov_cps_hcw_cap = pmin(adm_cps_hcw_cap / a_pop_hcw, 1),
           cov_booster_hcw_cap = pmin(adm_booster_hcw_cap / a_pop_hcw, 1))
  
  # Calculate adjusted cps administration figures
  timeseries <- timeseries %>%
    mutate(adm_cps_adj =
             pmin(N_VACC_LAST_DOSE + (hcw_diff * cov_total_fv), a_pop_hcw))
  
  # Calculate coverage estimations
  timeseries <- timeseries %>%
    mutate(cov_hcw_a1d = if_else(
      is.na(hcw_flag),
      pmin(N_VACC_DOSE1 / a_pop_hcw, 1),
      pmin((N_VACC_DOSE1 + (hcw_diff * cov_total_a1d)) / a_pop_hcw, 1))) %>%
    mutate(adm_hcw_a1d = if_else(
      is.na(hcw_flag),
      pmin(N_VACC_DOSE1, a_pop_hcw),
      pmin((N_VACC_DOSE1 + (hcw_diff * cov_total_a1d)), a_pop_hcw))) %>%
    mutate(cov_hcw_fv =
             pmin(
               (N_VACC_LAST_DOSE + if_else(
                 is.na(hcw_flag),
                 0,
                 hcw_diff * cov_total_fv
               )) / a_pop_hcw,
               1)) %>%
    mutate(adm_hcw_fv =
             pmin(
               (N_VACC_LAST_DOSE + if_else(
                 is.na(hcw_flag),
                 0,
                 hcw_diff * cov_total_fv
               )), a_pop_hcw)) %>%
    mutate(cov_hcw_booster =
             pmin(1, N_VACC_BOOSTER_DOSE / a_pop_hcw)) 
  
  
  # Calculate administration figures capped by older adult population
  timeseries <- timeseries %>%
    mutate(adm_a1d_old_cap = pmin(N_VACC_DOSE1_old, a_pop_older),
           adm_fv_old_cap = pmin(N_VACC_LAST_DOSE_old, a_pop_older),
           adm_booster_old_cap = pmin(N_VACC_BOOSTER_DOSE_old, a_pop_older))
  
  # Calculate older adult coverage
  timeseries <- timeseries %>%
    mutate(a_pop_older = as.numeric(a_pop_older),
           adm_a1d_old_cap = as.numeric(adm_a1d_old_cap),
           adm_fv_old_cap = as.numeric(adm_fv_old_cap),
           adm_booster_old_cap = as.numeric(adm_booster_old_cap),
           cov_old_a1d = pmin(adm_a1d_old_cap / a_pop_older, 1),
           cov_old_fv = pmin(adm_fv_old_cap / a_pop_older, 1),
           cov_old_booster = pmin(adm_booster_old_cap / a_pop_older, 1))

  # Calculate per capita funing amount
  timeseries <- timeseries %>%
    mutate(Funds_per_capita = Funding.Amount / a_pop)

  timeseries <- timeseries %>%
    select(
      iso,
      month_name,
      received,
      supply,
      absorbed,
      adm_td,
      adm_fv,
      adm_fv_change,
      adm_a1d,
      adm_a1d_change,
      adm_booster,
      adm_booster_change,
      est_stock,
      a_name_short,
      a_who_region,
      a_income_group,
      a_covax_status,
      a_csc_status,
      a_pop,
      cov_total_fv,
      cov_total_a1d,
      cov_total_booster,
      a_pop_hcw,
      a_pop_older,
      N_VACC_DOSE1,
      N_VACC_LAST_DOSE,
      N_VACC_BOOSTER_DOSE,
      adm_a1d_old_cap,
      adm_fv_old_cap,
      adm_booster_old_cap,
      adm_hcw_a1d,
      adm_hcw_fv,
      cov_hcw_a1d,
      cov_hcw_fv,
      cov_hcw_booster,
      cov_old_a1d,
      cov_old_fv,
      cov_old_booster,
      Funding.Amount,
      Funds_per_capita
    )
  
  timeseries <- timeseries %>%
    group_by(iso) %>%
    arrange(month_name) %>%
    fill(c("adm_fv",
           "cov_total_fv",
           "cov_total_a1d",
           "cov_total_booster",
           "cov_hcw_a1d",
           "cov_hcw_fv",
           "cov_hcw_booster",
           "N_VACC_DOSE1",
           "N_VACC_LAST_DOSE",
           "N_VACC_BOOSTER_DOSE",
           "adm_a1d_old_cap",
           "adm_fv_old_cap",
           "adm_booster_old_cap",
           "adm_hcw_a1d",
           "adm_hcw_fv",
           "cov_old_a1d",
           "cov_old_fv",
           "cov_old_booster"))
  
  print(" > Returning...")

  return(timeseries)
  
}