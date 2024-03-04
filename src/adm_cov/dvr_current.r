load_b_vxrate <- function(dvr_data) {
  print(">> Converting dvr_data to dataframe & correcting date formats...")
  b_vxrate <- as.data.frame(dvr_data)
  b_vxrate$date <- as.Date(b_vxrate$date, format = "%Y-%m-%d")

  print(">> Selecting & renaming relevant current daily vaccination rate data...")
  b_vxrate <- b_vxrate %>%
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
  return(b_vxrate)
}

transform_current_vxrate <- function(
  b_vxrate, entity_characteristics, date_refresh) {
    print(" >> Transforming current vxrate...")
    ## Add entity base data
    b_vxrate <- left_join(b_vxrate, entity_characteristics, by = "a_iso")
    ## Change population field type to numeric
    b_vxrate$a_pop <- as.numeric(b_vxrate$a_pop)
    ## Add year, month, and week numbers
    b_vxrate <- b_vxrate %>%
    mutate(adm_date_year = if_else(
      year(adm_date) == 2021, 2021,
      if_else(year(adm_date) == 2022, 2022,
              if_else(year(adm_date) == 2023, 2023,
      NA_real_))
    )) %>%
    mutate(
      adm_date_month = ifelse(
        year(adm_date) == 2021,
        month(adm_date),
        ifelse(
          year(adm_date) == 2022,
          month(adm_date) + 12,
          if_else(year(adm_date) == 2023,
                  month(adm_date) + 24,
                  NA_real_
        )
      )
    )) %>%
    mutate(adm_date_week = if_else(
      year(adm_date) == 2021 | year(adm_date) == 2022 | year(adm_date) == 2023,
      isoweek(adm_date),
      NA_integer_
      )
    )
    ## Remove pre-2021 entries
    b_vxrate <-
      filter(
        b_vxrate, adm_date_year == 2021 | adm_date_year == 2022 | adm_date_year == 2023
      )

    ## Indicate latest entry per ISO code
    b_vxrate <- b_vxrate %>%
      group_by(a_iso) %>%
        mutate(
          adm_latest = if_else(
            adm_date == max(adm_date),
            "Yes", "No"
          )
        )

    ## Indicate if latest entry is reported within the current or past week
    b_vxrate$adm_is_current <- ifelse(
      b_vxrate$adm_latest == "Yes" &
        (
          b_vxrate$adm_date_week == isoweek(date_refresh)
          | b_vxrate$adm_date_week == isoweek(date_refresh) - 1
        ),
        "Yes",
        NA
      )
    b_vxrate <- b_vxrate %>% 
      group_by(a_iso, adm_date_week) %>%
        mutate (
          adm_date_maxweek = if_else(
            adm_date == max(adm_date),
            "Yes",
            "No"
          )
        )

    ## Indicate if end of month
    b_vxrate <- b_vxrate %>% 
      group_by(a_iso, adm_date_month) %>%
        mutate(
          adm_date_eom = if_else(
            adm_date == max(adm_date),
            "Yes",
            "No"
          )
        )
    
    ## Indicate if last week
    b_vxrate <- b_vxrate %>%
      group_by(a_iso) %>%
      mutate(adm_last_week = if_else(
        adm_date == date_refresh - 14, 
        "Yes", 
        "No"
      ))
    
    ## Indicate if last month
    b_vxrate <- b_vxrate %>%
      group_by(a_iso) %>%
      mutate(adm_last_month = if_else(
        adm_date == date_refresh - 35, 
        "Yes", 
        "No"
      ))
    
    ## Indicate if two month
    b_vxrate <- b_vxrate %>%
      group_by(a_iso) %>%
      mutate(adm_two_month = if_else(
        adm_date == date_refresh - 63, 
        "Yes", 
        "No"
      ))

    ## Indicate most recent month
    b_vxrate <- b_vxrate %>% 
      group_by(a_iso) %>% 
        mutate (
          adm_date_lastmonth = if_else(
            adm_date_week == (
              max(adm_date_week) - 4)
                & adm_date_maxweek == "Yes", "Yes", "No"
              )
            )
    
    print(">> Done.")
    return(b_vxrate)
}

recreate_df <- function(b_vxrate, entity_characteristics) {
  print(">> Selecting & renaming columns required to recreate 13jan data...")
  b_vxrate_data <- b_vxrate %>%
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
    )

  print(">> Changing date format...")
  b_vxrate_data$date_13jan <- as.Date(b_vxrate_data$date_13jan)

  print(">> Selecting iso and date columns from entity detail data...")
  stable_dates <- entity_characteristics %>%
    select(a_iso, date_13jan)
  
  print(">> Changing date format...")
  stable_dates$date_13jan <- as.Date(stable_dates$date_13jan)

  print(">> Joining 13 jan dates to time series frame...")
  recreated_data <- inner_join(b_vxrate_data, stable_dates, by = c("a_iso", "date_13jan"))
  
  print(">> Done.")
  return(recreated_data)
}
 
## Create clean long form subsets and select relevant columns
transform_current_vxrate_pub <- function(b_vxrate) {
  print(" >> Create clean long form subsets for b_vxrate_pub")
  columns <- c(
    "a_iso",
    "a_name_short",
    "a_pop",
    "adm_date",
    "adm_tot_td",
    "adm_tot_a1d",
    "adm_tot_cps",
    "adm_tot_boost",
    "dvr_4wk_td",
    "a_region_who",
    "a_status_covax",
    "a_income_group",
    "a_status_csc",
    "a_continent_sub",
    "a_status_who",
    "adm_tot_a1d_adj", 
    "adm_tot_cps_adj")

    ### Calculate population coverage in long form datasets
    b_vxrate_pub <- b_vxrate %>%
      select(columns) %>%
      mutate(dvr_4wk_td_per = dvr_4wk_td / a_pop,
             cov_tot_cps = adm_tot_cps / a_pop,
             cov_tot_cps_theo = (adm_tot_td / 2) / a_pop,
             cov_tot_a1d = adm_tot_a1d / a_pop)

    print(">> Done.")
    return(b_vxrate_pub)
}

## Create population target deadline tables
### Create end of September 2021 table

transform_sept21_pop_tgt <- function(b_vxrate) {
  print(" >> Create end of September 2021 table population target deadline table...")
  c_vxrate_sept <- 
  filter(b_vxrate,
    adm_date_eom == "Yes" & 
    adm_date_month == 9 & 
    adm_date_year == 2021
  )

  #### Calculate cov_total_fv at end of September 2021
  c_vxrate_sept <- helper_calculate_cov_total_fv(c_vxrate_sept)

  #### Indicate if cov_total_fv is greater than or equal to 10%
  c_vxrate_sept <- c_vxrate_sept %>%
    mutate(t10_goalmet_sep = if_else(cov_total_fv >= .1, "Yes", "No"))

  #### Reduce to a_iso and t10_goalmet_sep
  c_vxrate_sept_t10 <-
    select(c_vxrate_sept, c("a_iso", "cov_total_fv", "t10_goalmet_sep"))
  
  colnames(c_vxrate_sept_t10) <- c("a_iso","cov_total_fv_30sep", "t10_goalmet_sep")

  print(">> Done.")
  return(c_vxrate_sept_t10)
}

transform_dec21_pop_tgt <- function(b_vxrate) {
  print(" Create end of Dec 2021 table population target deadline table...")
  ### Create end of December 2021 table
  c_vxrate_dec <-
    filter(b_vxrate,
      adm_date_eom == "Yes" & 
      adm_date_month == 12 & 
      adm_date_year == 2021
    )

  #### Calculate cov_total_fv at end of December 2021
  c_vxrate_dec <- helper_calculate_cov_total_fv(c_vxrate_dec)

  #### Indicate if cov_total_fv is greater than or equal to 20%
  c_vxrate_dec <- c_vxrate_dec %>%
    mutate(t20_goalmet_dec = if_else(cov_total_fv >= .2, "Yes", "No"))

  #### Indicate if cov_total_fv is greater than or equal to 40%
  c_vxrate_dec <- c_vxrate_dec %>%
    mutate(t40_goalmet_dec = if_else(cov_total_fv >= .4, "Yes", "No"))

  #### Reduce to a_iso, t20_goalmet_dec, t40_goalmet_dec
  c_vxrate_dec_t2040 <-
    select(c_vxrate_dec,
      c("a_iso", "cov_total_fv", "t20_goalmet_dec", "t40_goalmet_dec")) %>%
    rename(cov_total_fv_31dec = cov_total_fv)

  print(">> Done.")
  return(c_vxrate_dec_t2040)
}

transform_jun22_pop_tgt <- function(b_vxrate) {
  print(" >> Create end of June 2022 table population target deadline table...")
  c_vxrate_jun <-
    filter(b_vxrate,
           adm_date_eom == "Yes" & 
             adm_date_month == 18 & 
             adm_date_year == 2022
    )
  
  #### Calculate cov_total_fv at end of June 2022
  c_vxrate_jun <- helper_calculate_cov_total_fv(c_vxrate_jun)
  
  #### Indicate if cov_total_fv is greater than or equal to 70%
  c_vxrate_jun <- c_vxrate_jun %>%
    mutate(t70_goalmet_jun = if_else(cov_total_fv >= .7, "Yes", "No"))
  
  #### Reduce to a_iso and t70_goalmet_jun
  c_vxrate_jun_t70 <-
    select(c_vxrate_jun, c("a_iso", "cov_total_fv", "t70_goalmet_jun")) %>%
    rename(cov_total_fv_30jun = cov_total_fv)
  
  print(">> Done.")
  return(c_vxrate_jun_t70)
  
}

transform_abspt_by_month <- function(b_vxrate, current_month) {
  print(" >> Create absorption by month table")
  ## Create absorption by month table
  c_vxrate_eom <- filter(b_vxrate, adm_date_eom == "Yes")

  ### Select relevant columns
  c_vxrate_eom <- 
    select(
      c_vxrate_eom,
      c(
        "a_iso",
        "a_region_who",
        "a_continent",
        "a_status_covax",
        "a_status_csc",
        "a_status_who",
        "a_income_group",
        "a_status_csc",
        "adm_date_month",
        "adm_tot_td",
        "adm_tot_a1d",
        "adm_tot_cps",
        "adm_tot_boost"
        )
    )

  ### Create temporary previous month data frame to allow calculation
  z_vxrate_eom_temp <- 
    select(c_vxrate_eom, c("a_iso", "adm_date_month", "adm_tot_td", 
                           "adm_tot_a1d","adm_tot_cps", "adm_tot_boost"))
  
  z_vxrate_eom_temp <- z_vxrate_eom_temp %>%
    mutate(adm_date_month = adm_date_month + 1)
    colnames(z_vxrate_eom_temp) <-
      c("a_iso", "adm_date_month", "adm_tot_td_lm", "adm_tot_a1d_lm","adm_tot_cps_lm",
        "adm_tot_boost_lm")

  ### Calculate change by month
  c_vxrate_eom <- 
    left_join(
      c_vxrate_eom,
      z_vxrate_eom_temp,
      by = c("a_iso" = "a_iso", "adm_date_month" = "adm_date_month")
    )

  c_vxrate_eom <- c_vxrate_eom %>%
    mutate(adm_tot_td_absorbed = adm_tot_td - adm_tot_td_lm,
           adm_tot_a1d_change = adm_tot_a1d - adm_tot_a1d_lm,
           adm_tot_cps_change = adm_tot_cps - adm_tot_cps_lm,
           adm_tot_boost_change = adm_tot_boost - adm_tot_boost_lm)

  c_vxrate_eom <- c_vxrate_eom %>%
    mutate(adm_tot_td_absorbed = if_else(
      is.na(adm_tot_td_absorbed),
      adm_tot_td,
      adm_tot_td_absorbed)) %>%
    mutate(adm_tot_a1d_change = if_else(
      is.na(adm_tot_a1d_change),
      adm_tot_a1d,
      adm_tot_a1d_change)) %>%
    mutate(adm_tot_cps_change = if_else(
      is.na(adm_tot_cps_change),
      adm_tot_cps,
      adm_tot_cps_change)) %>%
    mutate(adm_tot_boost_change = if_else(
      is.na(adm_tot_boost_change),
      adm_tot_boost,
      adm_tot_boost_change))

  ## Note: list of months is automatically generated from "2021-01" to month of date_refresh
  c_vxrate_eom$adm_date_month_name <- helper_mapping_months( 
    c_vxrate_eom$adm_date_month,
    current_month
  )

  print(">> Done.")
  return(c_vxrate_eom)
}

# Create per country absorption table
absorption_per_country <- function(c_vxrate_eom, current_month) {
  print(" >> Adding per country monthly absorption table...")
  
  d_absorption_country <- select(
    c_vxrate_eom,
    c(
      "a_iso",
      "a_status_covax",
      "a_status_csc",
      "adm_tot_td",
      "adm_date_month",
      "adm_tot_td_absorbed",
      "adm_tot_cps",
      "adm_tot_cps_change",
      "adm_tot_a1d",
      "adm_tot_a1d_change",
      "adm_tot_boost",
      "adm_tot_boost_change"
    )
  )
  ## Note: list of months is automatically generated from "2021-01" to month of date_refresh
  d_absorption_country$adm_date_month_name <- helper_mapping_months(
    d_absorption_country$adm_date_month,
    current_month
  )

  print(" >> Selecting columns needed...")
  d_absorption_country <- select(
    d_absorption_country,
    c(
      "a_iso",
      "a_status_covax",
      "a_status_csc",
      "adm_tot_td",
      "adm_tot_td_absorbed",
      "adm_date_month_name",
      "adm_tot_cps",
      "adm_tot_cps_change",
      "adm_tot_a1d",
      "adm_tot_a1d_change",
      "adm_tot_boost",
      "adm_tot_boost_change"
    )
  )
  print(" >> Renaming columns...")
  colnames(d_absorption_country) <- c(
    "iso",
    "a_status_covax",
    "a_status_csc",
    "adm_tot_td",
    "value",
    "month_name",
    "adm_tot_cps",
    "adm_tot_cps_change",
    "adm_tot_a1d",
    "adm_tot_a1d_change",
    "adm_tot_boost",
    "adm_tot_boost_change"
  )
  d_absorption_country$a_amc_status <- NA
  d_absorption_country$a_amc_status[d_absorption_country$a_status_covax == "AMC" & d_absorption_country$iso != "IND"] <- "AMC91"  
  d_absorption_country$a_amc_status[d_absorption_country$a_status_covax == "AMC" & d_absorption_country$iso == "IND"] <- "India"  
  d_absorption_country$type <- "Absorbed"
  print(" >> Selecting columns needed from d_absorption_country for d_absorb_red...")
  d_absorb_red <- select(
    d_absorption_country,
    c(
      "iso",
      "month_name",
      "value",
      "adm_tot_td",
      "adm_tot_cps",
      "adm_tot_cps_change",
      "adm_tot_a1d",
      "adm_tot_a1d_change",
      "adm_tot_boost",
      "adm_tot_boost_change"
    )
  )
  print(" >> Renaming columns for d_absorb_red...")
  colnames(d_absorb_red) <- c(
    "iso",
    "month_name",
    "absorbed",
    "adm_tot_td",
    "adm_tot_cps",
    "adm_tot_cps_change",
    "adm_tot_a1d",
    "adm_tot_a1d_change",
    "adm_tot_boost",
    "adm_tot_boost_change"
  )
  datalist <- list("d_absorb_red" = d_absorb_red,
    "d_absorption_country" = d_absorption_country)
  
  print(">> Done.")
  return(datalist)
}

first_supplies <- function(d_absorb_red, d_absorption_country, overall_long, 
                           overall_cumul_long) {
  print(" >> Loading supplies data from supply dataset...")
  b_supply <- overall_long
  print(" >> Selecting columns needed for b_supply_red...")
  b_supply_red <- select(
    b_supply,
    c(
      "iso",
      "month_name",
      "value"
    )
  )
  print(" >> Renaming b_supply_red columns...")
  colnames(b_supply_red) <- c(
    "iso",
    "month_name",
    "received"
  )
  b_supply_add <- overall_cumul_long
  print(" >> Selecting columns needed for b_supply_red...")
  b_supply_add <- select(
    b_supply_add,
    c(
      "iso",
      "month_name",
      "supply"
    )
  )
  b_supply_red <- left_join(b_supply_red, b_supply_add, by = c("iso" = "iso", 
                                                               "month_name" = "month_name"))
  combined <- rbind(d_absorption_country, b_supply)
  datalist <- list("combined" = combined, "b_supply_red" = b_supply_red)
  
  print(">> Done.")
  return(datalist)
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

second_supplies <- function(d_absorption_country_new, combined,
  d_absorb_red, entity_characteristics, b_supply_red, overall_cumul_long) {
  print(" >> Loading supplies data for second supplies...")
  b_supply_second <- overall_cumul_long
  d_absorption_country_new <- select(
    d_absorption_country_new,
    c("iso", "absorbed", "month_name")
  )
  combined_new <- full_join(
    d_absorption_country_new,
    b_supply_second,
    b = c("iso", "month_name")
  )
  combined_new <- combined_new %>%
    mutate(est_stock = if_else(
      is.na(supply) | is.na(absorbed),
      NA_real_,
      pmax(supply - absorbed, 0)))

  d_est_stock <- select(
    combined_new,
    c("iso", "month_name", "est_stock")
  )

  combined_three <- full_join(
    b_supply_red,
    d_absorb_red,
    by = c("iso", "month_name")) %>%
    full_join(., d_est_stock, by = c("iso", "month_name")) %>%
    left_join(., entity_characteristics, by = c("iso" = "a_iso")
  )
  combined_three <- combined_three %>%
    arrange(desc(month_name), iso)
  
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
      summarize(!!as.name(paste0("absorption_", suffix)) := sum(adm_tot_td_absorbed))
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

  #### Continent = Africa
  c_vxrate_eom_africa <-
    filter(c_vxrate_eom, a_continent == "Africa")
  d_absorption_africa <- groupby_and_summarize(c_vxrate_eom_africa)

  for (region_appendix in c("EMR", "AFR", "SEAR", "WPR", "EUR", "AMR")) {
    assign(paste0("d_absorption_", tolower(region_appendix)),
      filter(c_vxrate_eom, a_region_who == region_appendix) %>%
        group_by(adm_date_month) %>%
        summarize("absorption_{tolower(region_appendix)}" :=
          sum(adm_tot_td_absorbed)
        )
    )
  }

  #### Additional groupings as needed

  ### Merge groupings monthly absorption data
  d_absorption <-
    left_join(d_absorption_amc, d_absorption_africa, by = "adm_date_month") %>%
    left_join(., d_absorption_emr, by = "adm_date_month") %>%
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



latest_sum_table <- function(b_vxrate, c_vxrate_latest) {
  print(" >> Create latest value summary table...")
  c_vxrate_latest <- b_vxrate %>%
    filter(adm_date <= as.Date("2023-12-31")) %>%
    filter(adm_date == max(adm_date))

  return(c_vxrate_latest)

}

# TODO make the three next functions DRY
last_week_sum_table <- function(b_vxrate, c_vxrate_latest, c_vxrate_lastweek) {
  print(" >> Create last week value summary table...")
  c_vxrate_lastweek <- filter(b_vxrate, adm_last_week == "Yes")
  
  c_vxrate_lw_temp <- select(c_vxrate_lastweek, c("a_iso", "adm_last_week"))
  c_vxrate_latest_temp <- select(c_vxrate_latest, -adm_last_week)
  c_vxrate_lw_temp <- left_join(c_vxrate_latest_temp, c_vxrate_lw_temp, by = "a_iso")
  c_vxrate_lw_temp <- filter(c_vxrate_lw_temp, is.na(adm_last_week))
  c_vxrate_lw_temp <- c_vxrate_lw_temp %>%
    mutate(adm_last_week = "Yes")
  c_vxrate_lw_temp <- select(c_vxrate_lw_temp, c("a_iso","adm_last_week"))
  c_vxrate_lw_temp <- left_join(c_vxrate_latest_temp, c_vxrate_lw_temp, by = "a_iso")
  c_vxrate_lw_temp <- filter(c_vxrate_lw_temp, adm_last_week == "Yes")
  c_vxrate_lastweek <- rbind(c_vxrate_lastweek, c_vxrate_lw_temp)
  
  ### Remove is_latest column
  c_vxrate_lastweek <- select(c_vxrate_lastweek, -c("adm_last_week"))
  
  return(c_vxrate_lastweek)
  
}

last_month_sum_table <- function(b_vxrate, c_vxrate_latest, c_vxrate_lastmonth) {
  print(" >> Create last month value summary table...")
  c_vxrate_lastmonth <- filter(b_vxrate, adm_last_month == "Yes")
  
  c_vxrate_lm_temp <- select(c_vxrate_lastmonth, c("a_iso", "adm_last_month"))
  c_vxrate_latest_temp <- select(c_vxrate_latest, -adm_last_month)
  c_vxrate_lm_temp <- left_join(c_vxrate_latest_temp, c_vxrate_lm_temp, by = "a_iso")
  c_vxrate_lm_temp <- filter(c_vxrate_lm_temp, is.na(adm_last_month))
  c_vxrate_lm_temp <- c_vxrate_lm_temp %>%
    mutate(adm_last_month = "Yes")
  c_vxrate_lm_temp <- select(c_vxrate_lm_temp, c("a_iso","adm_last_month"))
  c_vxrate_lm_temp <- left_join(c_vxrate_latest_temp, c_vxrate_lm_temp, by = "a_iso")
  c_vxrate_lm_temp <- filter(c_vxrate_lm_temp, adm_last_month == "Yes")
  c_vxrate_lastmonth <- rbind(c_vxrate_lastmonth, c_vxrate_lm_temp)
  
  ### Remove is_latest column
  c_vxrate_lastmonth <- select(c_vxrate_lastmonth, -c("adm_last_month"))
  
  return(c_vxrate_lastmonth)
  
}

two_month_sum_table <- function(b_vxrate, c_vxrate_latest, c_vxrate_twomonth) {
  print(" >> Create two month value summary table...")
  c_vxrate_twomonth <- filter(b_vxrate, adm_two_month == "Yes")
  
  c_vxrate_2m_temp <- select(c_vxrate_twomonth, c("a_iso", "adm_two_month"))
  c_vxrate_latest_temp <- select(c_vxrate_latest, -adm_two_month)
  c_vxrate_2m_temp <- left_join(c_vxrate_latest_temp, c_vxrate_2m_temp, by = "a_iso")
  c_vxrate_2m_temp <- filter(c_vxrate_2m_temp, is.na(adm_two_month))
  c_vxrate_2m_temp <- c_vxrate_2m_temp %>%
    mutate(adm_two_month = "Yes")
  c_vxrate_2m_temp <- select(c_vxrate_2m_temp, c("a_iso","adm_two_month"))
  c_vxrate_2m_temp <- left_join(c_vxrate_latest_temp, c_vxrate_2m_temp, by = "a_iso")
  c_vxrate_2m_temp <- filter(c_vxrate_2m_temp, adm_two_month == "Yes")
  c_vxrate_twomonth <- rbind(c_vxrate_twomonth, c_vxrate_2m_temp)
  
  ### Remove is_latest column
  c_vxrate_twomonth <- select(c_vxrate_twomonth, -c("adm_two_month"))
  
  return(c_vxrate_twomonth)
  
}
