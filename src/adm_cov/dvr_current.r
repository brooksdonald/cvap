load_adm_tot <- function(dvr_data) {
  print(">> Converting dvr_data to dataframe & correcting date formats...")
  adm_tot <- as.data.frame(dvr_data) %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d"))

  print(">> Selecting & renaming relevant administrative data (total population)...")
  adm_tot <- adm_tot %>%
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
      adm_tot_td = total_doses,
      adm_tot_a1d = at_least_one_dose,
      adm_tot_cps = fully_vaccinated,
      adm_tot_boost = persons_booster_add_dose,
      dvr_4wk_td = rolling_4_week_avg_td,
      dvr_4wk_fv = rolling_4_week_avg_fv,
      dvr_4wk_td_max = max_rolling_4_week_avg_td,
      dvr_4wk_td_lm = rolling_4_week_avg_td_lastmonth,
      note_nochange = no_change_from_previous,
      adm_tot_a1d_adj = at_least_one_dose_adj,
      adm_tot_cps_adj = fully_vaccinated_adj
    )
  
  print(">> Done.")
  return(adm_tot)
}

trans_adm_tot <- function(adm_tot, entity_characteristics, date_refresh) {
  print(">> Joining administrative and entity characteristic dataframes...")
  adm_tot <- left_join(adm_tot, entity_characteristics, by = "a_iso")
  
  print(">> Adding relevant date-related fields...")
  adm_tot <- adm_tot %>%
    mutate(a_pop = as.numeric(a_pop),
           adm_date_year = year(adm_date),
           adm_date_month = ifelse(year(adm_date) == 2021, month(adm_date),
                                   ifelse(year(adm_date) == 2022, month(adm_date) + 12,
                                          if_else(year(adm_date) == 2023, month(adm_date) + 24,
                                                  if_else(year(adm_date) == 2024, month(adm_date) + 36,
                                                          NA_real_))))) %>%
    mutate(adm_date_week = if_else(
      year(adm_date) == 2021 | 
        year(adm_date) == 2022 | 
        year(adm_date) == 2023 | 
        year(adm_date) == 2024,
      isoweek(adm_date),
      NA_integer_))

  print(">> Removing pre-2021 entries & applying latest report flag...")
  adm_tot <- adm_tot %>%
      filter(adm_date_year == 2021 | 
               adm_date_year == 2022 | 
               adm_date_year == 2023 |
               adm_date_year == 2024) %>%
    group_by(a_iso) %>%
    mutate(adm_latest = if_else(adm_date == max(adm_date),
                                "Yes", 
                                "No")) %>%
    ungroup()
  
  print(">> Applying date-related flags for subsequent filtering...")
  adm_tot <- adm_tot %>%
    mutate(is_current = ifelse(adm_tot$adm_latest == "Yes" &
                                 (adm_tot$adm_date_week == isoweek(date_refresh)
                                  | adm_tot$adm_date_week == isoweek(date_refresh) - 1), 
                               "Yes",
                               NA)) %>%
    group_by(a_iso, adm_date_week) %>%
    mutate(adm_date_maxweek = if_else(adm_date == max(adm_date),
                                      "Yes",
                                      "No")) %>%
    ungroup() %>%
    group_by(a_iso, adm_date_month) %>%
    mutate(adm_date_eom = if_else(adm_date == max(adm_date),
                                  "Yes",
                                  "No")) %>%
    ungroup()

    print(">> Done.")
    return(adm_tot)
}

create_adm_tot_13jan <- function(adm_tot, entity_characteristics) {
  print(">> Selecting & renaming columns required to recreate 13jan data...")
  adm_tot_13jan <- adm_tot %>%
    select(a_iso,
           adm_date,
           adm_tot_td,
           adm_tot_a1d,
           adm_tot_cps,
           adm_tot_boost) %>%
    rename(
      a_iso = a_iso, 
      date_13jan = adm_date, 
      adm_tot_td_13jan = adm_tot_td, 
      adm_tot_a1d_13jan = adm_tot_a1d, 
      adm_tot_cps_13jan = adm_tot_cps, 
      adm_tot_boost_13jan = adm_tot_boost
    ) %>%
    mutate(date_13jan = as.Date(date_13jan))

  print(">> Selecting iso and date columns from entity detail data...")
  stable_dates <- entity_characteristics %>%
    select(a_iso, date_13jan) %>%
    mutate(date_13jan = as.Date(date_13jan))

  print(">> Joining 13 jan dates to time series frame...")
  adm_tot_13jan <- inner_join(adm_tot_13jan, stable_dates, by = c("a_iso", "date_13jan"))
  
  print(">> Done.")
  return(adm_tot_13jan)
}

create_adm_tot_ts_daily <- function(adm_tot) {
  print(">> Creating long-form, daily administrative (total population) data frame...")
  adm_tot_ts_daily <- adm_tot %>%
      select(a_iso,
             a_name_short,
             a_region_who,
             a_income_group,
             a_status_who,
             a_status_covax,
             a_status_csc,
             a_continent_sub,
             a_pop,
             adm_date,
             adm_tot_td,
             adm_tot_a1d,
             adm_tot_a1d_adj, 
             adm_tot_cps,
             adm_tot_cps_adj,
             adm_tot_boost,
             dvr_4wk_td) %>%
      mutate(dvr_4wk_td_per = dvr_4wk_td / a_pop,
             cov_tot_cps = adm_tot_cps / a_pop,
             cov_tot_cps_theo = (adm_tot_td / 2) / a_pop,
             cov_tot_a1d = adm_tot_a1d / a_pop)

    print(">> Done.")
    return(adm_tot_ts_daily)
}

create_adm_tot_sep21 <- function(adm_tot) {
  print(">> Creating September 2021 administrative (total population) data frame...")
  adm_tot_sep2021 <- adm_tot %>%
    filter(adm_date_eom == "Yes" & 
             adm_date_month == 9 & 
             adm_date_year == 2021)

  print(">> Calculating total population coverage as at September 2021...")
  adm_tot_sep2021 <- helper_calculate_cov_total_fv(adm_tot_sep2021)

  print(">> Adding 10% target achievement status & selecting relevant columns...")
  adm_tot_sep2021 <- adm_tot_sep2021 %>%
    mutate(t10_goalmet_sep = if_else(cov_total_fv >= .1, "Yes", "No")) %>%
    select(a_iso,
           cov_total_fv, 
           t10_goalmet_sep) %>%
    rename(cov_total_fv_30sep = cov_total_fv)
  
  print(">> Done.")
  return(adm_tot_sep2021)
}

create_adm_tot_dec21 <- function(adm_tot) {
  print(">> Creating December 2021 administrative (total population) data frame...")
  adm_tot_dec21 <- adm_tot %>%
    filter(adm_date_eom == "Yes" & 
             adm_date_month == 12 & 
             adm_date_year == 2021)

  print(">> Calculating total population coverage as at December 2021...")
  adm_tot_dec21 <- helper_calculate_cov_total_fv(adm_tot_dec21)

  print(">> Adding 20%/40% target achievement statuses & selecting relevant columns...")
  adm_tot_dec21 <- adm_tot_dec21 %>%
    mutate(t20_goalmet_dec = if_else(cov_total_fv >= .2, "Yes", "No"),
           t40_goalmet_dec = if_else(cov_total_fv >= .4, "Yes", "No")) %>%
    select(a_iso, 
           cov_total_fv, 
           t20_goalmet_dec, 
           t40_goalmet_dec) %>%
    rename(cov_total_fv_31dec = cov_total_fv)

  print(">> Done.")
  return(adm_tot_dec21)
}

create_adm_tot_jun22 <- function(adm_tot) {
  print(">> Creating June 2022 administrative (total population) data frame...")
  adm_tot_jun22 <- adm_tot %>%
    filter(adm_date_eom == "Yes" & 
             adm_date_month == 18 & 
             adm_date_year == 2022)
  
  print(">> Calculating total population coverage as at June 2022...")
  adm_tot_jun22 <- helper_calculate_cov_total_fv(adm_tot_jun22)
  
  print(">> Adding 70% target achievement statuses & selecting relevant columns...")
  adm_tot_jun22 <- adm_tot_jun22 %>%
    mutate(t70_goalmet_jun = if_else(cov_total_fv >= .7, "Yes", "No")) %>%
    select(a_iso, 
           cov_total_fv, 
           t70_goalmet_jun) %>%
    rename(cov_total_fv_30jun = cov_total_fv)
  
  print(">> Done.")
  return(adm_tot_jun22)
}

create_adm_tot_td_mon <- function(adm_tot, current_month) {
  print(">> Creating monthly, administrative (total population) time series data frame...")
  adm_tot_ts_mon <- adm_tot %>%
    filter(adm_date_eom == "Yes") %>%
    select(a_iso,
           a_pop,
           a_pop_hcw,
           a_region_who,
           a_income_group,
           a_status_who,
           a_status_covax,
           a_status_csc,
           adm_date_month,
           adm_tot_td,
           adm_tot_a1d,
           adm_tot_cps,
           adm_tot_boost) %>%
    group_by(a_iso) %>%
    arrange(a_iso, adm_date_month) %>%
    mutate(adm_tot_td_add = adm_tot_td - lag(adm_tot_td, default = first(adm_tot_td)),
           adm_tot_a1d_add = adm_tot_a1d - lag(adm_tot_a1d, default = first(adm_tot_a1d)),
           adm_tot_cps_add = adm_tot_cps - lag(adm_tot_cps, default = first(adm_tot_cps)),
           adm_tot_boost_add = adm_tot_boost - lag(adm_tot_boost, default = first(adm_tot_boost)))

  ## Note: list of months is automatically generated from "2021-01" to month of date_refresh
  adm_tot_ts_mon$adm_date_month_name <- helper_mapping_months( 
    adm_tot_ts_mon$adm_date_month,
    current_month
  )

  print(">> Done.")
  return(adm_tot_ts_mon)
}

absorption_per_country <- function(adm_tot_ts_mon, current_month) {
  print(" >> Adding per country monthly absorption table...")
  
  adm_tot_ts_mon <- select(
    adm_tot_ts_mon,
    c(
      "a_iso",
      "a_pop",
      "a_pop_hcw",
      "a_status_covax",
      "a_status_csc",
      "adm_tot_td",
      "adm_date_month",
      "adm_tot_td_add",
      "adm_tot_cps",
      "adm_tot_cps_add",
      "adm_tot_a1d",
      "adm_tot_a1d_add",
      "adm_tot_boost",
      "adm_tot_boost_add"
    )
  )
  ## Note: list of months is automatically generated from "2021-01" to month of date_refresh
  adm_tot_ts_mon$adm_date_month_name <- helper_mapping_months(
    adm_tot_ts_mon$adm_date_month,
    current_month
  )

  print(" >> Selecting columns needed...")
  adm_tot_ts_mon <- adm_tot_ts_mon %>%
    select(a_iso,
           a_pop,
           a_pop_hcw,
           a_status_covax,
           a_status_csc,
           adm_tot_td,
           adm_tot_td_add,
           adm_date_month_name,
           adm_tot_cps,
           adm_tot_cps_add,
           adm_tot_a1d,
           adm_tot_a1d_add,
           adm_tot_boost,
           adm_tot_boost_add) %>%
    rename(
      iso = a_iso,
      value = adm_tot_td_add,
      month_name = adm_date_month_name
      ) %>%
    mutate(type = "Absorbed")
  
  print(">> Done.")
  return(adm_tot_ts_mon)
}

new_absorption_countries <- function(c_vxrate_eom, current_month) {
  print(" >> Selecting columns from c_vxrate_eom for d_absorption_country_new...")
  d_absorption_country_new <- select(
      c_vxrate_eom,
      c(
        "a_iso",
        "adm_tot_td",
        "adm_date_month"
      )
    )
    print(" >> Renaming d_absorption_country_new columns...")
    colnames(d_absorption_country_new) <- c(
      "iso",
      "absorbed",
      "adm_date_month"
    )

  ## Note: list of months is automatically generated from "2021-01" to month of date_refresh
    d_absorption_country_new$month_name <- helper_mapping_months(
      d_absorption_country_new$adm_date_month,
      current_month
    )
    
    print(">> Done.")
    return(d_absorption_country_new)
}

second_supplies <- function(adm_tot_ts_mon, sup_ts_wide) {
  print(" >> Loading supplies data for second supplies...")
  
  combined_three <- full_join(adm_tot_ts_mon, sup_ts_wide, 
                              by = c("a_iso" = "iso", "adm_date_month_name" = "month_name")) %>%
    arrange(desc(adm_date_month_name), a_iso)

  print(">> Done.")
  return(combined_three)
}

absorption_sum_by_month <- function(c_vxrate_eom, current_month) {
  print(" >> Summarize absorption by grouping by month...")
  ## Summarize absorption by grouping by month

  groupby_and_summarize <- function(c_vxrate) {
    suffix <- substr(deparse(substitute(c_vxrate)), 14, 30)
    return(c_vxrate %>%
      group_by(adm_date_month) %>%
      summarize(!!as.name(paste0("absorption_", suffix)) := sum(adm_tot_td_add))
    )
  }

  ### COVAX participation = AMC
  c_vxrate_eom_amc <- 
    filter(c_vxrate_eom, a_status_covax == "AMC")
  d_absorption_amc <- groupby_and_summarize(c_vxrate_eom_amc)

  #### COVAX participation = AMC91
  c_vxrate_eom_amc91 <-
    filter(c_vxrate_eom, a_status_covax == "AMC" & a_iso != "IND")
  d_absorption_amc91 <- groupby_and_summarize(c_vxrate_eom_amc91)

  #### COVAX participation = India
  c_vxrate_eom_ind <-
    filter(c_vxrate_eom, a_iso == "IND")
  d_absorption_ind <- groupby_and_summarize(c_vxrate_eom_ind)
  
  #### Concerted support status = csc
  c_vxrate_eom_csc <-
    filter(c_vxrate_eom, a_status_csc == "Concerted support country")
  d_absorption_csc <- groupby_and_summarize(c_vxrate_eom_csc)
  
  #### WHO Member States
  c_vxrate_eom_who <-
    filter(c_vxrate_eom, a_status_who == "Member State")
  d_absorption_who <- groupby_and_summarize(c_vxrate_eom_who)

  for (region_appendix in c("EMR", "AFR", "SEAR", "WPR", "EUR", "AMR")) {
    assign(paste0("d_absorption_", tolower(region_appendix)),
      filter(c_vxrate_eom, a_region_who == region_appendix) %>%
        group_by(adm_date_month) %>%
        summarize("absorption_{tolower(region_appendix)}" :=
          sum(adm_tot_td_add)
        )
    )
  }

  #### Additional groupings as needed

  ### Merge groupings monthly absorption data
  d_absorption <-
    left_join(d_absorption_amc,  d_absorption_emr, by = "adm_date_month") %>%
    left_join(., d_absorption_amr, by = "adm_date_month") %>%
    left_join(., d_absorption_afr, by = "adm_date_month") %>%
    left_join(., d_absorption_sear, by = "adm_date_month") %>%
    left_join(., d_absorption_wpr, by = "adm_date_month") %>%
    left_join(., d_absorption_eur, by = "adm_date_month") %>%
    left_join(., d_absorption_amc91, by = "adm_date_month") %>%
    left_join(., d_absorption_csc, by = "adm_date_month") %>%
    left_join(., d_absorption_ind, by = "adm_date_month") %>%
    left_join(., d_absorption_who, by = "adm_date_month")
  
    ## Note: list of months is automatically generated from "2021-01" to month of date_refresh
    d_absorption$adm_date_month_name <- helper_mapping_months(
      d_absorption$adm_date_month,
      current_month
    )
  return(d_absorption)
}

latest_sum_table <- function(adm_tot, adm_tot_13jan) {
  print(" >> Create latest value summary table...")
  c_vxrate_latest <- adm_tot %>%
    filter(adm_date <= as.Date("2023-12-31")) %>%
    group_by(a_iso) %>%
    filter(adm_date == max(adm_date)) %>%
    ungroup() %>%
    left_join(., adm_tot_13jan, by = "a_iso")

  return(c_vxrate_latest)

}
