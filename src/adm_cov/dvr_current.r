
# load current daily vxrate
load_b_vxrate <- function(dvr_data, adm_api, auto_cleaning) {
  if (adm_api) {
    print(" >> Using data from API...")
    b_vxrate <- as.data.frame(dvr_data)
    b_vxrate$date <- as.Date(b_vxrate$date, format = "%Y-%m-%d")
  } else {
    print(" >> Loading current daily vaccination rate from Excel...")
    b_vxrate <-
        data.frame(
            read_excel("data/_input/base_dvr_current.xlsx",
            sheet = "data"
        )
    )
  }
  print(" >> Selecting relevant current daily vaccination rate columns...")
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
    columns <- append(columns,
      c("at_least_one_dose_adj", "fully_vaccinated_adj"))
  }
  b_vxrate <-
    select(
      b_vxrate,
      columns
    )

  print(" >> Renaming current daily vaccination rate columns...")
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
    column_names <- append(column_names,
      c("adm_a1d_adj", "adm_fv_adj"))
  }
  colnames(b_vxrate) <- column_names


  return(b_vxrate)

}

transform_current_vxrate <- function(
  b_vxrate, entity_characteristics, refresh_date) {
    print(" >> Transforming current vxrate...")
    ## Add entity base data
    b_vxrate <- left_join(b_vxrate, entity_characteristics, by = "a_iso")
    ## Change population field type to numeric
    b_vxrate$a_pop <- as.numeric(b_vxrate$a_pop)
    ## Add year, month, and week numbers
    b_vxrate <- b_vxrate %>%
    mutate(adm_date_year = if_else(
      year(adm_date) == 2021,
      2021,
      if_else(year(adm_date) == 2022, 2022,
      NA_real_)
    )) %>%
    mutate(
      adm_date_month = ifelse(
        year(adm_date) == 2021,
        month(adm_date),
        ifelse(
          year(adm_date) == 2022,
          month(adm_date) + 12,
          NA
        )
      )
    ) %>%
    mutate(adm_date_week = if_else(
      year(adm_date) == 2021 | year(adm_date) == 2022,
      isoweek(adm_date),
      NA_integer_
      )
    )
    ## Remove pre-2021 entries
    b_vxrate <-
      filter(
        b_vxrate, adm_date_year == 2021 | adm_date_year == 2022
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
          b_vxrate$adm_date_week == isoweek(refresh_date)
          | b_vxrate$adm_date_week == isoweek(refresh_date) - 1
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
        adm_date == refresh_date - 14, 
        "Yes", 
        "No"
      ))
    
    ## Indicate if last month
    b_vxrate <- b_vxrate %>%
      group_by(a_iso) %>%
      mutate(adm_last_month = if_else(
        adm_date == refresh_date - 35, 
        "Yes", 
        "No"
      ))
    
    ## Indicate if two month
    b_vxrate <- b_vxrate %>%
      group_by(a_iso) %>%
      mutate(adm_two_month = if_else(
        adm_date == refresh_date - 63, 
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
    return(b_vxrate)
}

## Create clean long form subsets and select relevant columns
transform_current_vxrate_pub <- function(b_vxrate, auto_cleaning) {
  print(" >> Create clean long form subsets for b_vxrate_pub")
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
    "a_ifc_status",
    "a_continent_sub"
  )
  if (auto_cleaning) {
    columns <- append(columns,
      c("adm_a1d_adj", "adm_fv_adj"))
  }
  b_vxrate_pub <-
  select(
    b_vxrate,
    columns
    )
    ### Calculate dvr_4wk_td_per in long form subsets
    b_vxrate_pub <- b_vxrate_pub %>%
      mutate(dvr_4wk_td_per = dvr_4wk_td / a_pop)
    
    ### Calculate population coverage in long form datasets
    b_vxrate_pub <- b_vxrate_pub %>%
      mutate(cov_total_fv = adm_fv / a_pop) %>%
      
      mutate(cov_total_fv_theo = (adm_td / 2) / a_pop)

    return(b_vxrate_pub)
}

transform_subset_amc <- function(b_vxrate) {
  print(" >> Create clean long form subsets for b_vxrate_amc")
  b_vxrate_amc <- filter(b_vxrate, a_covax_status == "AMC" | a_iso == "GAB")
  b_vxrate_amc <-
    select(
      b_vxrate_amc,
      c(
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
        "a_ifc_status",
        "a_continent_sub"
      )
    )
    b_vxrate_amc <- b_vxrate_amc %>%
      mutate(dvr_4wk_td_per = dvr_4wk_td / a_pop)
    
    b_vxrate_amc <- b_vxrate_amc %>%
      mutate(cov_total_fv = adm_fv / a_pop) %>%
      
      mutate(cov_total_fv_theo = (adm_td / 2) / a_pop)

    return(b_vxrate_amc)
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
        "a_who_region",
        "a_continent",
        "a_covax_status",
        "a_csc_status",
        "a_income_group",
        "a_csc_status",
        "a_ifc_status",
        "adm_date_month",
        "adm_td"
      )
    )

  ### Create temporary previous month data frame to allow calculation
  z_vxrate_eom_temp <- 
    select(c_vxrate_eom, c("a_iso", "adm_date_month", "adm_td"))
  
  z_vxrate_eom_temp <- z_vxrate_eom_temp %>%
    mutate(adm_date_month = adm_date_month + 1)
    colnames(z_vxrate_eom_temp) <-
      c("a_iso", "adm_date_month", "adm_td_lm")

  ### Calculate change by month
  c_vxrate_eom <- 
    left_join(
      c_vxrate_eom,
      z_vxrate_eom_temp,
      by = c("a_iso" = "a_iso", "adm_date_month" = "adm_date_month")
    )

  c_vxrate_eom <- c_vxrate_eom %>%
    mutate(adm_td_absorbed = adm_td - adm_td_lm)

  c_vxrate_eom <- c_vxrate_eom %>%
    mutate(adm_td_absorbed = if_else(
      is.na(adm_td_absorbed),
      adm_td,
      adm_td_absorbed))

  ## Note: list of months is automatically generated from "2021-01" to month of refresh_date
  c_vxrate_eom$adm_date_month_name <- helper_mapping_months( 
    c_vxrate_eom$adm_date_month,
    current_month
  )

  return(c_vxrate_eom)
}

# Create per country absorption table
absorption_per_country <- function(c_vxrate_eom, current_month) {
  print(" >> Adding per country monthly absorption table...")
  
  d_absorption_country <- select(
    c_vxrate_eom,
    c(
      "a_iso",
      "a_covax_status",
      "a_csc_status",
      "adm_date_month",
      "adm_td_absorbed"
    )
  )
  ## Note: list of months is automatically generated from "2021-01" to month of refresh_date
  d_absorption_country$adm_date_month_name <- helper_mapping_months(
    d_absorption_country$adm_date_month,
    current_month
  )

  print(" >> Selecting columns needed...")
  d_absorption_country <- select(
    d_absorption_country,
    c(
      "a_iso",
      "a_covax_status",
      "a_csc_status",
      "adm_td_absorbed",
      "adm_date_month_name"
    )
  )
  print(" >> Renaming columns...")
  colnames(d_absorption_country) <- c(
    "iso",
    "a_covax_status",
    "a_csc_status",
    "value",
    "month_name"
  )
  d_absorption_country$type <- "Absorbed"
  print(" >> Selecting columns needed from d_absorption_country for d_absorb_red...") #nolint
  d_absorb_red <- select(
    d_absorption_country,
    c(
      "iso",
      "month_name",
      "value"
    )
  )
  print(" >> Renaming columns for d_absorb_red...")
  colnames(d_absorb_red) <- c(
    "iso",
    "month_name",
    "absorbed"
  )
  datalist <- list("d_absorb_red" = d_absorb_red,
    "d_absorption_country" = d_absorption_country)
  return(datalist)
}

first_supplies <- function(d_absorb_red, d_absorption_country) {
  print(" >> Loading supplies data from supply dataset...")
  b_supply <- data.frame(
    read_excel("data/output/supply.xlsx",
    sheet = "data"
    )
  )
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
  combined <- rbind(d_absorption_country, b_supply)
  datalist <- list("combined" = combined, "b_supply_red" = b_supply_red)
  return(datalist)
}

new_absorption_countries <- function(c_vxrate_eom, current_month) {
  print(" >> Selecting columns from c_vxrate_eom for d_absorption_country_new...")
  d_absorption_country_new <- select(
      c_vxrate_eom,
      c(
        "a_iso",
        "adm_td",
        "adm_date_month"
      )
    )
    print(" >> Renaming d_absorption_country_new columns...")
    colnames(d_absorption_country_new) <- c(
      "iso",
      "absorbed",
      "adm_date_month"
    )

  ## Note: list of months is automatically generated from "2021-01" to month of refresh_date
    d_absorption_country_new$month_name <- helper_mapping_months(
      d_absorption_country_new$adm_date_month,
      current_month
    )

    return(d_absorption_country_new)
}

second_supplies <- function(d_absorption_country_new, combined,
  d_absorb_red, entity_characteristics, b_supply_red) {
  print(" >> Loading supplies data for second supplies...")
  b_supply_second <- data.frame(
    read_excel("data/output/supply.xlsx", sheet = "supply")
  )
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
  mutate(est_stock = if_else(is.na(supply) | is.na(absorbed), NA_real_,
  if_else((supply - absorbed) < 0, 0, (supply - absorbed))))
  
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
  return(combined_three)
}

absorption_sum_by_month <- function(c_vxrate_eom, current_month) {
  print(" >> Summarize absorption by grouping by month...")
  ## Summarize absorption by grouping by month

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

  #### Concerted support status = csc
  c_vxrate_eom_csc <-
    filter(c_vxrate_eom, a_csc_status == "Concerted support country")
  d_absorption_csc <- groupby_and_summarize(c_vxrate_eom_csc)

  #### Immediate focus country status = IFC
  c_vxrate_eom_ifc <-
    filter(c_vxrate_eom, a_ifc_status == "Immediate focus")
  d_absorption_ifc <- groupby_and_summarize(c_vxrate_eom_ifc)

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
    left_join(., d_absorption_ifc, by = "adm_date_month")

    ## Note: list of months is automatically generated from "2021-01" to month of refresh_date
    d_absorption$adm_date_month_name <- helper_mapping_months(
      d_absorption$adm_date_month,
      current_month
    )
  return(d_absorption)
}



latest_sum_table <- function(b_vxrate, c_vxrate_latest) {
  print(" >> Create latest value summary table...")
  c_vxrate_latest <- filter(b_vxrate, adm_latest == "Yes")

  ### Remove is_latest column
  c_vxrate_latest <- select(c_vxrate_latest, -c("adm_latest"))

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
