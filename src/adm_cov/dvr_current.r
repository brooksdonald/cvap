
load_dvr_current <- function(dvr_data, adm_api, auto_cleaning) {
  print(" >> Loading current daily vaccination rate data...")
  if (adm_api) {
    print(" >> Loading current daily vaccination rate from API...")
    dvr_current <- as.data.frame(dvr_data)
    dvr_current$date <- as.Date(dvr_current$date, format = "%Y-%m-%d")
    } else {
      print(" >> Loading current daily vaccination rate from Excel...")
      dvr_current <- data.frame(
        read_excel("data/input/base_dvr_current.xlsx",
                   sheet = "data"
                   )
      )
    }
  
  print(" >> Selecting relevant columns...")
  columns <- c(
    "iso_code",
    "date",
    "total_doses",
    "at_least_one_dose",
    "fully_vaccinated",
    "persons_booster_add_dose",
    "rolling_4_week_avg_td",
    "rolling_4_week_avg_fv",
    "max_rolling_4_week_avg_td",
    "rolling_4_week_avg_td_lastmonth",
    "no_change_from_previous"
    )
  
  if (auto_cleaning) {
    print(" >> Auto Cleaning: Appending columns...")
    columns <- append(columns,
                      c("at_least_one_dose_adj", "fully_vaccinated_adj"
                        )
                      )
  }
  dvr_current <- select(
    dvr_current,
    columns
    )
  
  print(" >> Renaming columns...")
  column_names <- c(
    "a_iso",
    "adm_date",
    "adm_td",
    "adm_a1d",
    "adm_fv",
    "adm_booster",
    "dvr_4wk_td",
    "dvr_4wk_fv",
    "dvr_4wk_td_max",
    "dvr_4wk_td_lm",
    "note_nochange"
  )
  
  if (auto_cleaning) {
    print(" >> Auto Cleaning: Renaming columns...")
    column_names <- append(column_names,
                           c("adm_a1d_adj", "adm_fv_adj"))
  }
  
  colnames(dvr_current) <- column_names
  
  print(" >> Function 'load_dvr_current' done")
  return(dvr_current)
}

transform_dvr_current <- function(
    dvr_current, entity_characteristics, refresh_date) {
  print(" >> Transforming current daily vaccination rate data...")
  
  print(" >> Joining entity characteristics base data...")
  dvr_current <- left_join(dvr_current, entity_characteristics, by = "a_iso")
  
  print(" >> Transforming 'a_pop' variable...")
  dvr_current$a_pop <- as.numeric(dvr_current$a_pop)
  
  print(" >> Transforming 'adm_date_year', 'adm_date_month', and 'adm_date_week' variables...")
  dvr_current <- dvr_current %>%
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
  
  print(" >> Filtering out pre-2021 data...")
  dvr_current <- filter(dvr_current, 
                        adm_date_year == 2021 | adm_date_year == 2022 | adm_date_year == 2023
                        )
  
  print(" >> Generating 'adm_latest' variable: Latest entry per ISO code...")
  dvr_current <- dvr_current %>%
    group_by(a_iso) %>%
    mutate(adm_latest = if_else(
      adm_date == max(adm_date),
      "Yes", "No"
      )
      )
  
  print(" >> Generating 'adm_is_current' variable: If latest entry reported is within current or past week...")
  dvr_current$adm_is_current <- ifelse(
    dvr_current$adm_latest == "Yes" & 
      (dvr_current$adm_date_week == isoweek(refresh_date) | 
         dvr_current$adm_date_week == isoweek(refresh_date) - 1),
    "Yes",
    NA
  )
  
  print(" >> Generating 'adm_date_maxweek' variable: Latest entry per ISO Code per week...")
  dvr_current <- dvr_current %>% 
    group_by(a_iso, adm_date_week) %>%
    mutate (
      adm_date_maxweek = if_else(
        adm_date == max(adm_date),
        "Yes",
        "No"
      )
    )
  
  print(" >> Generating 'adm_date_eom' variable: If latest entry reported is from the end of the month...")
  dvr_current <- dvr_current %>%
    group_by(a_iso, adm_date_month) %>%
    mutate(adm_date_eom = if_else(
      adm_date == max(adm_date),
      "Yes",
      "No"
      )
      )
  
  print(" >> Generating 'adm_last_week' variable: If latest entry reported is from last week...")
  dvr_current <- dvr_current %>%
    group_by(a_iso) %>%
    mutate(adm_last_week = if_else(
      adm_date == refresh_date - 14, 
      "Yes", 
      "No"
      )
      )
  
  print(" >> Generating 'adm_last_month' variable: If latest entry reported is from last month...")
  dvr_current <- dvr_current %>%
    group_by(a_iso) %>%
    mutate(adm_last_month = if_else(
      adm_date == refresh_date - 35, 
      "Yes", 
      "No"
    )
    )

  print(" >> Generating 'adm_two_month' variable: If latest entry reported is from two months ago...")
  dvr_current <- dvr_current %>%
    group_by(a_iso) %>%
    mutate(adm_two_month = if_else(
      adm_date == refresh_date - 63, 
      "Yes", 
      "No"
    )
    )
  
  print(" >> Generating 'adm_date_lastmonth' variable: If latest entry reported is from the most recent month...")
  dvr_current <- dvr_current %>% 
    group_by(a_iso) %>% 
    mutate(adm_date_lastmonth = if_else(
      adm_date_week == (
        max(adm_date_week) - 4)
      & adm_date_maxweek == "Yes", 
      "Yes", 
      "No"
      )
      )
  
  print(" >> Function 'transform_dvr_current' done")
  return(dvr_current)
}

transform_dvr_current_pub <- function(dvr_current, auto_cleaning) {
  print(" >> Transforming clean long-form subsets of daily vaccination rate data...")
  columns <- c(
    "a_iso",
    "a_name_short",
    "a_pop",
    "adm_date",
    "adm_td",
    "adm_a1d",
    "adm_fv",
    "adm_booster",
    "dvr_4wk_td",
    "a_who_region",
    "a_covax_status",
    "a_income_group",
    "a_csc_status",
    "a_continent_sub",
    "a_who_status"
  )
  if (auto_cleaning) {
    print(" >> Auto Cleaning: Appending columns...")
    columns <- append(columns,
                      c("adm_a1d_adj", "adm_fv_adj"))
  }
  
  print(" >> Calculating population coverage in long-form data...")
  dvr_current_pub <- dvr_current %>%
    select(columns) %>%
    mutate(dvr_4wk_td_per = dvr_4wk_td / a_pop,
           cov_total_fv = adm_fv / a_pop,
           cov_total_fv_theo = (adm_td / 2) / a_pop,
           cov_total_a1d = adm_a1d / a_pop)
  
  print(" >> Function 'transform_dvr_current_pub' done")
  return(dvr_current_pub)
}

transform_dvr_current_amc <- function(dvr_current) {
  print(" >> Filtering for AMC countries...")
  dvr_current_amc <- filter(dvr_current, a_covax_status == "AMC" | a_iso == "GAB")
  
  print(" >> Selecting relevant columnds...")
  dvr_current_amc <- select(
    dvr_current_amc, c(
      "a_iso",
      "a_name_short",
      "a_pop",
      "adm_date",
      "adm_td",
      "adm_a1d",
      "adm_fv",
      "adm_booster",
      "dvr_4wk_td",
      "a_who_region",
      "a_covax_status",
      "a_income_group",
      "a_csc_status",
      "a_continent_sub"
      )
    )
  
  print(" >> Calculating daily vaccination rate variables for AMC countries...")
  dvr_current_amc <- dvr_current_amc %>%
    mutate(dvr_4wk_td_per = dvr_4wk_td / a_pop) %>%
    mutate(cov_total_fv = adm_fv / a_pop) %>%
    mutate(cov_total_fv_theo = (adm_td / 2) / a_pop)
  
  print(" >> Function 'transform_dvr_current_amc' done")
  return(dvr_current_amc)
}

transform_pop_tgt_sep_t10 <- function(dvr_current) {
  print(" >> Creating end of September 2021 table population target deadline table...")
  dvr_current_sep_2021 <- 
    filter(dvr_current,
           adm_date_eom == "Yes" & adm_date_month == 9 & adm_date_year == 2021
           )
  
  print(" >> Generating 'cov_total_fv' variable at end of September 2021...")
  dvr_current_sep_2021 <- helper_calculate_cov_total_fv(dvr_current_sep_2021)
  
  print(" >> Generating 't10_goalmet_sep' variable: Whether or not total coverage is greater than or equal to 10%...")
  dvr_current_sep_2021 <- dvr_current_sep_2021 %>%
    mutate(t10_goalmet_sep = if_else(cov_total_fv >= .1, "Yes", "No"))
  
  print(" >> Selecting relevant columns...")
  pop_tgt_sep_t10 <- select(
    dvr_current_sep_2021, c(
      "a_iso", 
      "cov_total_fv", 
      "t10_goalmet_sep"
      )
    )
  
  print(" >> Renaming columns...")
  colnames(pop_tgt_sep_t10) <- c(
    "a_iso",
    "cov_total_fv_30sep",
    "t10_goalmet_sep"
    )
  
  print(" >> Function 'transform_pop_tgt_sep_t10' done")
  return(pop_tgt_sep_t10)
}

transform_pop_tgt_dec_t2040 <- function(dvr_current) {
  print(" >> Creating end of December 2021 table population target deadline table...")
  dvr_current_dec_2021 <- 
    filter(dvr_current,
           adm_date_eom == "Yes" & adm_date_month == 12 & adm_date_year == 2021
    )
  
  print(" >> Generating 'cov_total_fv' variable at end of December 2021...")
  dvr_current_dec_2021 <- helper_calculate_cov_total_fv(dvr_current_dec_2021)
  
  print(" >> Generating 't20_goalmet_dec' and 't40_goalmet_dec' variable: Whether or not total coverage is greater than or equal to 20% and/or 40%...")
  dvr_current_dec_t2040 <- dvr_current_dec_2021 %>%
    mutate(t20_goalmet_dec = if_else(cov_total_fv >= .2, "Yes", "No")) %>%
    mutate(t40_goalmet_dec = if_else(cov_total_fv >= .4, "Yes", "No"))
  
  print(" >> Selecting relevant columns...")
  pop_tgt_dec_t2040 <- select(
    dvr_current_dec_t2040, c(
      "a_iso", 
      "cov_total_fv", 
      "t20_goalmet_dec", 
      "t40_goalmet_dec")) %>%
    rename(cov_total_fv_31dec = cov_total_fv)
  
  print(" >> Function 'transform_pop_tgt_dec_t2040' done")
  return(pop_tgt_dec_t2040)
}

transform_pop_tgt_jun_t70 <- function(dvr_current) {
  print(" >> Creating end of June 2022 table population target deadline table...")
  dvr_current_jun_2022 <- 
    filter(dvr_current,
           adm_date_eom == "Yes" & adm_date_month == 18 & adm_date_year == 2022
    )
  
  print(" >> Generating 'cov_total_fv' variable at end of December 2021...")
  dvr_current_jun_2022 <- helper_calculate_cov_total_fv(dvr_current_jun_2022)
  
  print(" >> Generating 't20_goalmet_dec' and 't40_goalmet_dec' variable: Whether or not total coverage is greater than or equal to 20% and/or 40%...")
  dvr_current_jun_t70 <- dvr_current_jun_2022 %>%
    mutate(t70_goalmet_jun = if_else(cov_total_fv >= .7, "Yes", "No"))
  
  print(" >> Selecting relevant columns...")
  pop_tgt_jun_t70 <- select(
    dvr_current_jun_t70, c(
      "a_iso", 
      "cov_total_fv", 
      "t70_goalmet_jun")) %>%
    rename(cov_total_fv_30jun = cov_total_fv)
  
  print(" >> Function 'transform_pop_tgt_jun_t70' done")
  return(pop_tgt_jun_t70)
}

transform_absorption_by_month <- function(b_vxrate, current_month) {
  print(" >> Creating absorption by month table...")
  absorption_by_month <- filter(b_vxrate, adm_date_eom == "Yes")
  
  print(" >> Selecting relevant columns...")
  absorption_by_month <- select(
    absorption_by_month, c(
      "a_iso",
      "a_who_region",
      "a_continent",
      "a_covax_status",
      "a_csc_status",
      "a_who_status",
      "a_income_group",
      "a_csc_status",
      "adm_date_month",
      "adm_td",
      "adm_a1d",
      "adm_fv",
      "adm_booster"
      )
    )
  
  print(" >> Creating temporary previous month data for calculation...")
  absorption_by_month_temp <- select(
    absorption_by_month, c("a_iso", "adm_date_month", "adm_td",
                      "adm_a1d","adm_fv", "adm_booster"))
  
  absorption_by_month_temp <- absorption_by_month_temp %>%
    mutate(adm_date_month = adm_date_month + 1)
  colnames(absorption_by_month_temp) <- c("a_iso", "adm_date_month", "adm_td_lm",
                                     "adm_a1d_lm","adm_fv_lm", "adm_booster_lm")
  
  print(" >> Calculating change since previous month...")
  absorption_by_month <- left_join(absorption_by_month,
                                   absorption_by_month_temp,
                              by = c("a_iso" = "a_iso", "adm_date_month" = "adm_date_month")
                              )
  
  absorption_by_month <- absorption_by_month %>%
    mutate(adm_td_absorbed = adm_td - adm_td_lm,
           adm_a1d_change = adm_a1d - adm_a1d_lm,
           adm_fv_change = adm_fv - adm_fv_lm,
           adm_booster_change = adm_booster - adm_booster_lm)
  
  absorption_by_month <- absorption_by_month %>%
    mutate(adm_td_absorbed = if_else(
      is.na(adm_td_absorbed),
      adm_td,
      adm_td_absorbed)) %>%
    mutate(adm_a1d_change = if_else(
      is.na(adm_a1d_change),
      adm_a1d,
      adm_a1d_change)) %>%
    mutate(adm_fv_change = if_else(
      is.na(adm_fv_change),
      adm_fv,
      adm_fv_change)) %>%
    mutate(adm_booster_change = if_else(
      is.na(adm_booster_change),
      adm_booster,
      adm_booster_change))
  
  ## Note: list of months is automatically generated from "2021-01" to month of refresh_date
  absorption_by_month$adm_date_month_name <- helper_mapping_months(
    absorption_by_month$adm_date_month,
    current_month
  )
  
  print(" >> Function 'transform_absorption_by_month' done")
  return(absorption_by_month)
}

transform_absorption_by_country <- function(absorption_by_month, current_month) {
  print(" >> Creating per country monthly absorption table...")
  absorption_by_country <- select(
    absorption_by_month,
    c(
      "a_iso",
      "a_covax_status",
      "a_csc_status",
      "adm_td",
      "adm_date_month",
      "adm_td_absorbed",
      "adm_fv",
      "adm_fv_change",
      "adm_a1d",
      "adm_a1d_change",
      "adm_booster",
      "adm_booster_change"
    )
  )
  ## Note: list of months is automatically generated from "2021-01" to month of refresh_date
  absorption_by_country$adm_date_month_name <- helper_mapping_months(
    absorption_by_country$adm_date_month,
    current_month
  )
  
  print(" >> Selecting relevant columns...")
  absorption_by_country <- select(
    absorption_by_country, c(
      "a_iso",
      "a_covax_status",
      "a_csc_status",
      "adm_td",
      "adm_td_absorbed",
      "adm_date_month_name",
      "adm_fv",
      "adm_fv_change",
      "adm_a1d",
      "adm_a1d_change",
      "adm_booster",
      "adm_booster_change"
    )
  )
  
  print(" >> Renaming columns...")
  colnames(absorption_by_country) <- c(
    "iso",
    "a_covax_status",
    "a_csc_status",
    "adm_td",
    "value",
    "month_name",
    "adm_fv",
    "adm_fv_change",
    "adm_a1d",
    "adm_a1d_change",
    "adm_booster",
    "adm_booster_change"
  )
  
  print(" >> Cleaning 'a_amc_status' variable...")
  absorption_by_country$a_amc_status <- NA
  absorption_by_country$a_amc_status[absorption_by_country$a_covax_status == "AMC" & absorption_by_country$iso != "IND"] <- "AMC91"  
  absorption_by_country$a_amc_status[absorption_by_country$a_covax_status == "AMC" & absorption_by_country$iso == "IND"] <- "India"  
  
  print(" >> Generating 'type' variable...")
  absorption_by_country$type <- "Absorbed"
  
  print(" >> Creating per country monthly absorption table (reduced)...")
  print(" >> Selecting relevant columns...")
  absorption_by_country_red <- select(
    absorption_by_country, c(
      "iso",
      "month_name",
      "value",
      "adm_td",
      "adm_fv",
      "adm_fv_change",
      "adm_a1d",
      "adm_a1d_change",
      "adm_booster",
      "adm_booster_change"
      )
    )
  
  print(" >> Renaming columns...")
  colnames(absorption_by_country_red) <- c(
    "iso",
    "month_name",
    "absorbed",
    "adm_td",
    "adm_fv",
    "adm_fv_change",
    "adm_a1d",
    "adm_a1d_change",
    "adm_booster",
    "adm_booster_change"
    )
  
  print(" >> Creating list of absorption by country data...")
  datalist <- list("d_absorb_red" = absorption_by_country_red,
                   "d_absorption_country" = absorption_by_country)
  
  print(" >> Function 'transform_absorption_by_country' done")
  return(datalist)
}

absorption_sum_by_month <- function(c_vxrate_eom, current_month) {
  print(" >> Summarize absorption by grouping by month...")
  
  groupby_and_summarize <- function(c_vxrate) {
    suffix <- substr(deparse(substitute(c_vxrate)), 14, 30)
    return(c_vxrate %>%
             group_by(adm_date_month) %>%
             summarize(!!as.name(paste0("absorption_", suffix)) := sum(adm_td_absorbed))
    )
  }
  
  ### COVAX participation = AMC
  c_vxrate_eom_amc <-
    filter(c_vxrate_eom, a_covax_status == "AMC")
  d_absorption_amc <- groupby_and_summarize(c_vxrate_eom_amc)
  
  #### COVAX participation = AMC91
  c_vxrate_eom_amc91 <-
    filter(c_vxrate_eom, a_covax_status == "AMC" & a_iso != "IND")
  d_absorption_amc91 <- groupby_and_summarize(c_vxrate_eom_amc91)
  
  #### COVAX participation = India
  c_vxrate_eom_ind <-
    filter(c_vxrate_eom, a_iso == "IND")
  d_absorption_ind <- groupby_and_summarize(c_vxrate_eom_ind)
  
  #### Concerted support status = csc
  c_vxrate_eom_csc <-
    filter(c_vxrate_eom, a_csc_status == "Concerted support country")
  d_absorption_csc <- groupby_and_summarize(c_vxrate_eom_csc)
  
  #### WHO Member States
  c_vxrate_eom_who <-
    filter(c_vxrate_eom, a_who_status == "Member State")
  d_absorption_who <- groupby_and_summarize(c_vxrate_eom_who)
  
  #### Continent = Africa
  c_vxrate_eom_africa <-
    filter(c_vxrate_eom, a_continent == "Africa")
  d_absorption_africa <- groupby_and_summarize(c_vxrate_eom_africa)
  
  for (region_appendix in c("EMR", "AFR", "SEAR", "WPR", "EUR", "AMR")) {
    assign(paste0("d_absorption_", tolower(region_appendix)),
           filter(c_vxrate_eom, a_who_region == region_appendix) %>%
             group_by(adm_date_month) %>%
             summarize("absorption_{tolower(region_appendix)}" :=
                         sum(adm_td_absorbed)
             )
    )
  }
  
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
  
  ## Note: list of months is automatically generated from "2021-01" to month of refresh_date
  d_absorption$adm_date_month_name <- helper_mapping_months(
    d_absorption$adm_date_month,
    current_month
  )
  
  print(" >> Function 'absorption_sum_by_month' done")  
  return(d_absorption)
}

latest_sum_table <- function(dvr_current_pub, latest_sum_table) {
  print(" >> Creating latest value summary table...")
  latest_sum_table <- filter(dvr_current_pub, adm_latest == "Yes")
  
  print(" >> Removing 'adm_latest' variable...")
  latest_sum_table <- select(latest_sum_table, -c("adm_latest"))
  
  print(" >> Function 'latest_sum_table' done")
  return(latest_sum_table)
}

last_week_sum_table <- function(dvr_current_pub, latest_sum_table, last_week_sum_table) {
  print(" >> Creating last week value summary table...")
  last_week_sum_table <- filter(dvr_current_pub, adm_last_week == "Yes")
  
  last_week_sum_table_temp <- select(last_week_sum_table, c("a_iso", "adm_last_week"))
  latest_sum_table_temp <- select(latest_sum_table, -adm_last_week)
  last_week_sum_table_temp <- left_join(latest_sum_table_temp, last_week_sum_table_temp, by = "a_iso")
  last_week_sum_table_temp <- filter(last_week_sum_table_temp, is.na(adm_last_week))
  last_week_sum_table_temp <- last_week_sum_table_temp %>%
    mutate(adm_last_week = "Yes")
  last_week_sum_table_temp <- select(last_week_sum_table_temp, c("a_iso","adm_last_week"))
  last_week_sum_table_temp <- left_join(latest_sum_table_temp, last_week_sum_table_temp, by = "a_iso")
  last_week_sum_table_temp <- filter(last_week_sum_table_temp, adm_last_week == "Yes")
  last_week_sum_table <- rbind(last_week_sum_table, last_week_sum_table_temp)
  
  print(" >> Removing 'adm_latest' variable...")
  last_week_sum_table <- select(last_week_sum_table, -c("adm_last_week"))
  
  print(" >> Function 'last_week_sum_table' done")
  return(last_week_sum_table)
}

last_month_sum_table <- function(dvr_current_pub, latest_sum_table, last_month_sum_table) {
  print(" >> Creating last month value summary table...")
  last_month_sum_table <- filter(dvr_current_pub, adm_last_month == "Yes")
  
  last_month_sum_table_temp <- select(last_month_sum_table, c("a_iso", "adm_last_month"))
  latest_sum_table_temp <- select(latest_sum_table, -adm_last_month)
  last_month_sum_table_temp <- left_join(latest_sum_table_temp, last_month_sum_table_temp, by = "a_iso")
  last_month_sum_table_temp <- filter(last_month_sum_table_temp, is.na(adm_last_month))
  last_month_sum_table_temp <- last_month_sum_table_temp %>%
    mutate(adm_last_month = "Yes")
  last_month_sum_table_temp <- select(last_month_sum_table_temp, c("a_iso","adm_last_month"))
  last_month_sum_table_temp <- left_join(latest_sum_table_temp, last_month_sum_table_temp, by = "a_iso")
  last_month_sum_table_temp <- filter(last_month_sum_table_temp, adm_last_month == "Yes")
  last_month_sum_table <- rbind(last_month_sum_table, last_month_sum_table_temp)
  
  print(" >> Removing 'adm_last_month' variable...")
  last_month_sum_table <- select(last_month_sum_table, -c("adm_last_month"))
  
  print(" >> Function 'last_month_sum_table' done")  
  return(last_month_sum_table)
}

two_month_sum_table <- function(dvr_current_pub, latest_sum_table, two_month_sum_table) {
  print(" >> Creating two month value summary table...")
  two_month_sum_table <- filter(dvr_current_pub, adm_two_month == "Yes")
  
  two_month_sum_table_temp <- select(two_month_sum_table, c("a_iso", "adm_two_month"))
  latest_sum_table_temp <- select(latest_sum_table, -adm_two_month)
  two_month_sum_table_temp <- left_join(latest_sum_table_temp, two_month_sum_table_temp, by = "a_iso")
  two_month_sum_table_temp <- filter(two_month_sum_table_temp, is.na(adm_two_month))
  two_month_sum_table_temp <- two_month_sum_table_temp %>%
    mutate(adm_two_month = "Yes")
  two_month_sum_table_temp <- select(two_month_sum_table_temp, c("a_iso","adm_two_month"))
  two_month_sum_table_temp <- left_join(latest_sum_table_temp, two_month_sum_table_temp, by = "a_iso")
  two_month_sum_table_temp <- filter(two_month_sum_table_temp, adm_two_month == "Yes")
  two_month_sum_table <- rbind(two_month_sum_table, two_month_sum_table_temp)
  
  print(" >> Removing 'adm_two_month' variable...")
  two_month_sum_table <- select(two_month_sum_table, -c("adm_two_month"))
  
  print(" >> Function 'two_month_sum_table' done")  
  return(two_month_sum_table)
}

new_absorption_countries <- function(absorption_by_month, current_month) {
  print(" >> Selecting relevant columns...")
  new_absorption_countries <- select(
    absorption_by_month, c(
      "a_iso",
      "adm_td",
      "adm_date_month"
      )
    )
  
  print(" >> Renaming columns...")
  colnames(new_absorption_countries) <- c(
    "iso",
    "absorbed",
    "adm_date_month"
    )
  
  new_absorption_countries$month_name <- helper_mapping_months(
    new_absorption_countries$adm_date_month,
    current_month
  )
  
  print(" >> Function 'new_absorption_countries' done")  
  return(new_absorption_countries)
}

first_supplies <- function(d_absorb_red, d_absorption_country, 
                           overall_long, overall_cumul_long) {
  print(" >> Loading data for first supplies...")
  first_supplies <- overall_long
  
  print(" >> Selecting relevant columns...")
  first_supplies_red <- select(
    first_supplies, c(
      "iso",
      "month_name",
      "value"
      )
    )
  
  print(" >> Renaming columns...")
  colnames(first_supplies_red) <- c(
    "iso",
    "month_name",
    "received"
  )
  
  first_supplies_add <- overall_cumul_long
  print(" >> Selecting relevant columns...")
  first_supplies_add <- select(
    first_supplies_add,
    c(
      "iso",
      "month_name",
      "supply"
    )
  )
  
  print(" >> Joining data...")
  first_supplies_red <- left_join(first_supplies_red, 
                                  first_supplies_add, 
                                  by = c("iso" = "iso", "month_name" = "month_name"))
  combined <- rbind(d_absorption_country, first_supplies)
  
  print(" >> Creating list of dataframes...")  
  datalist_first_supplies <- list("combined" = combined, 
                                  "b_supply_red" = first_supplies_red)
  
  print(" >> Function 'first_supplies' done")    
  return(datalist_first_supplies)
}

second_supplies <- function(d_absorption_country_new, combined,
                            d_absorb_red, entity_characteristics, 
                            b_supply_red, overall_cumul_long) {
  print(" >> Loading data for second supplies...")
  second_supplies <- overall_cumul_long
  
  print(" >> Selecting relevant columns...")
  d_absorption_country_new <- select(
    d_absorption_country_new, c(
      "iso", 
      "absorbed", 
      "month_name"
      )
    )
  
  print(" >> Joining data...")
  combined_new <- full_join(
    d_absorption_country_new,
    second_supplies,
    b = c("iso", "month_name")
  )
  combined_new <- combined_new %>%
    mutate(est_stock = if_else(
      is.na(supply) | is.na(absorbed),
      NA_real_,
      pmax(supply - absorbed, 0)))
  
  d_est_stock <- select(
    combined_new, c(
      "iso", 
      "month_name", 
      "est_stock"
      )
    )
  
  print(" >> Joining data...")
  combined_three <- full_join(
    b_supply_red,
    d_absorb_red,
    by = c("iso", "month_name")) %>%
    full_join(., d_est_stock, by = c("iso", "month_name")) %>%
    left_join(., entity_characteristics, by = c("iso" = "a_iso")
    )
  combined_three <- combined_three %>%
    arrange(desc(month_name), iso)
  
  print(" >> Function 'second_supplies' done")  
  return(combined_three)
}

recreate_df <- function(b_vxrate) {
  print(" >> Selecting relevant columns...")
  b_vxrate_data <- b_vxrate %>%
    select(a_iso, 
           adm_date, 
           adm_td, 
           adm_a1d, 
           adm_fv, 
           adm_booster)

  print(" >> Renaming columns...")
  colnames(b_vxrate_data) <- c(
    "iso_code", 
    "date_13jan", 
    "adm_td_13jan", 
    "adm_a1d_13jan", 
    "adm_fv_13jan", 
    "adm_booster_13jan")

  print(" >> Change date format")
  b_vxrate_data$date_13jan <- as.Date(b_vxrate_data$date_13jan)

  print(" >> Loading base_adhoc data...")
  base_adhoc <- data.frame(
    read_excel(
      "data/input/static/base_adhoc.xlsx",
      sheet = "data"
      )
    )
  
  print(" >> Selecting relevant columns...")
  base_adhoc <- base_adhoc %>%
    select(iso, date_13jan)
  
  print(" >> Changing date column to date format...")
  base_adhoc$date_13jan <- as.Date(base_adhoc$date_13jan)
  
  print(" >> Renaming columns...")
  names(base_adhoc)[1] <- "iso_code"

  print(" >> Joining data...")
  recreated_data <- inner_join(b_vxrate_data, 
                               base_adhoc, 
                               by = c("iso_code", "date_13jan"))
  
  print(" >> Renaming columns...")
  names(recreated_data)[1] <- "a_iso"

  print(" >> Function 'recreate_df' done")  
  return(recreated_data)
}
