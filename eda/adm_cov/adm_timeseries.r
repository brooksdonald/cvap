
merge_timeseries <- function(a_data, combined_three, target_hcwold){
  
  base_dates <- data.frame(seq(from = ymd("2021-01-01"), 
                               to = ymd("2023-12-31"), 
                               by = "month")) 
  colnames(base_dates) <- "month_name"

  # Select necessary entity characteristics
  a_data_temp_ts <- a_data %>%
    select(
      a_iso,
      a_region_who,
      a_income_group,
      a_status_who,
      a_status_covax,
      a_pop,
      a_pop_hcw,
      a_pop_old,
      date_intro,
      adm_target_hcw
    )
  
  print(" > Adding month date...")

  # Prepare combined time series
  combined_three <- combined_three %>%
    mutate(month_name = as.Date(paste0(as.character(adm_date_month_name), '-01'), format = '%Y-%m-%d'),
           cov_tot_cps = adm_tot_cps / a_pop,
           cov_tot_a1d = adm_tot_a1d / a_pop,
           cov_tot_boost = adm_tot_boost / a_pop) %>%
    select(-adm_date_month,
           -adm_date_month_name,
           -a_status_csc,
           -a_pop,
           -a_pop_hcw,
           -a_status_who,
           -a_status_covax,
           -a_region_who,
           -a_income_group)
  
  print(" > Join dataframes...")
  # Merge finance timeseries data with HCW population data frame
  timeseries <- merge(base_dates, a_data_temp_ts, all = TRUE) %>%
    left_join(., combined_three, by = c("a_iso" = "a_iso", "month_name" = "month_name"), copy = TRUE) %>%
    left_join(., target_hcwold, by = c("a_iso" = "ISO_3_CODE", "month_name" = "DATE"), copy = TRUE)
    # left_join(., overall_fin_cumul_long, by = c("a_iso" = "ISO.Code", "month_name"), copy = TRUE)

  print(" > Calculating HCW specific fields...")
  # Calculate HCW flag & diff
  timeseries <- timeseries %>%
    mutate(hcw_flag = if_else(a_pop_hcw > adm_target_hcw, "Yes", NA_character_),
           hcw_diff = pmax(a_pop_hcw - adm_target_hcw, 0, na.rm = TRUE))
  
  # Calculate administration figures capped by HCW population
  timeseries <- timeseries %>%
    mutate(adm_hcw_a1d_cap = pmin(adm_hcw_a1d, a_pop_hcw),
           adm_hcw_cps_cap = pmin(adm_hcw_cps, a_pop_hcw),
           adm_hcw_boost_cap = pmin(adm_hcw_boost, a_pop_hcw))
  
  # Calculate HCW coverage
  timeseries <- timeseries %>%
    mutate(a_pop_hcw = as.numeric(a_pop_hcw),
           cov_hcw_a1d_cap = pmin(adm_hcw_a1d_cap / a_pop_hcw, 1),
           cov_hcw_cps_cap = pmin(adm_hcw_cps_cap / a_pop_hcw, 1),
           cov_hcw_boost_cap = pmin(adm_hcw_boost_cap / a_pop_hcw, 1))
  
  # Calculate coverage estimations
  timeseries <- timeseries %>%
    mutate(
      adm_hcw_a1d = pmin(adm_hcw_a1d + (hcw_diff * cov_tot_a1d), a_pop_hcw),
      cov_hcw_a1d = pmin(adm_hcw_a1d / a_pop_hcw, 1),
      adm_hcw_cps = pmin(adm_hcw_cps + (hcw_diff * cov_tot_cps), a_pop_hcw),
      cov_hcw_cps = pmin(adm_hcw_cps / a_pop_hcw, 1),
      adm_hcw_boost = pmin(adm_hcw_boost + (hcw_diff * cov_tot_boost), a_pop_hcw),   
      cov_hcw_boost = pmin(adm_hcw_boost / a_pop_hcw), 1) 
  
  
  # Calculate administration figures capped by older adult population
  timeseries <- timeseries %>%
    mutate(adm_old_a1d_cap = pmin(adm_old_a1d, a_pop_old),
           adm_old_cps_cap = pmin(adm_old_cps, a_pop_old),
           adm_old_boost_cap = pmin(adm_old_boost, a_pop_old))
  
  # Calculate older adult coverage
  timeseries <- timeseries %>%
    mutate(a_pop_old = as.numeric(a_pop_old),
           cov_old_a1d = pmin(adm_old_a1d_cap / a_pop_old, 1),
           cov_old_cps = pmin(adm_old_cps_cap / a_pop_old, 1),
           cov_old_boost = pmin(adm_old_boost_cap / a_pop_old, 1))

  # Calculate per capita funding amount
  # timeseries <- timeseries %>%
  #   mutate(Funds_per_capita = Funding.Amount / a_pop)
  
  
  
  # Ensure current month is included
  # months <- data.frame(NA) %>%
  #   rename(
  #     month_name = NA.
  #   )
  # months$month_name <- as.Date("2023-12-01")

  timeseries <- timeseries %>%
    select(
      a_iso,
      a_region_who,
      a_income_group,
      a_status_covax,
      a_status_who,
      a_pop,
      a_pop_hcw,
      a_pop_old,
      date_intro,
      month_name,
      rec_add,
      rec_cumul,
      adm_tot_td,
      adm_tot_cps,
      adm_tot_cps_add,
      adm_tot_a1d,
      adm_tot_a1d_add,
      adm_tot_boost,
      adm_tot_boost_add,
      cov_tot_cps,
      cov_tot_a1d,
      cov_tot_boost,
      adm_old_a1d_cap,
      adm_old_cps_cap,
      adm_old_boost_cap,
      cov_old_a1d,
      cov_old_cps,
      cov_old_boost,
      adm_hcw_a1d,
      adm_hcw_cps,
      adm_hcw_boost,
      cov_hcw_a1d,
      cov_hcw_cps,
      cov_hcw_boost
    )
  
  # desired_date <- as.Date("2023-12-01")
  # 
  # missing_entries <- timeseries %>%
  #   group_by(a_iso) %>%
  #   summarise(needs_row = !any(month_name == desired_date)) %>%
  #   filter(needs_row) %>%
  #   mutate(month_name = desired_date, value = NA) %>%
  #   select(-needs_row)
  
  timeseries <- timeseries %>%
    group_by(a_iso) %>%
    arrange(month_name) %>%
    fill(a_region_who,
         a_income_group,
         a_status_covax,
         a_status_who,
         a_pop,
         a_pop_hcw,
         a_pop_old,
         date_intro,
         adm_tot_td,
         adm_tot_a1d,
         adm_tot_cps,
         adm_tot_boost,
         cov_tot_cps,
         cov_tot_a1d,
         cov_tot_boost,
         adm_hcw_a1d,
         adm_hcw_cps,
         adm_hcw_boost,
         cov_hcw_a1d,
         cov_hcw_cps,
         cov_hcw_boost,
         adm_old_a1d_cap,
         adm_old_cps_cap,
         adm_old_boost_cap,
         cov_old_a1d,
         cov_old_cps,
         cov_old_boost) %>%
    ungroup()
  
  print(" > Returning...")

  return(timeseries)
  
}