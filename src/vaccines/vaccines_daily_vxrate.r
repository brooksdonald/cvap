
# load current daily vxrate
load_b_vxrate <- function(adm_data) {
  use_api <- readline(prompt = " >> Would you like to use data from the API? \n 'y' uses data from API, 'n' uses data from Excel: ")
  if (use_api %in% c('y', 'Y', 'yes', 'Yes')) {
    print(" >> Using data from API...")
    b_vxrate <- as.data.frame(adm_data)
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
    print(" >> Selecting relevant columns and rename...")
    b_vxrate <-
      select(
        b_vxrate,
        c(
          "iso_code",
          "population",
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
      )

    print(" >> Renaming current vxrate columns...")
    colnames(b_vxrate) <-
      c(
        "a_iso",
        "a_pop",
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

    return(b_vxrate)

}

transform_current_vxrate <- function(
  b_vxrate, entity_characteristics, refresh_date) {
    print(" >> Transforming current vxrate...")
    ## Change population field type to numeric
    b_vxrate$a_pop <- as.numeric(b_vxrate$a_pop)
    ## Add entity base data
    b_vxrate <- left_join(b_vxrate, entity_characteristics, by = "a_iso")
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
transform_current_vxrate_pub <- function(b_vxrate) {
  print(" >> Create clean long form subsets for b_vxrate_pub")
  b_vxrate_pub <-
  select(
    b_vxrate,
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

transform_smooth_timeseries <- function(
  b_vxrate_amc, b_vxrate_pub, refresh_date) {
  print(" >> Transforming smooth time series...")
  d_cov_smooth <- b_vxrate_pub
  d_cov_smooth <- select(
    d_cov_smooth,
    c(
      "a_iso",
      "adm_date",
      "adm_fv"
      )
    )
  z_cov_smooth <- d_cov_smooth
  z_cov_smooth <- z_cov_smooth %>%
  mutate(
    adm_date = adm_date + days(1)
  )
  colnames(z_cov_smooth) <- c(
    "a_iso",
    "adm_date",
    "adm_fv_ld"
  )
  d_cov_smooth <- left_join(
    d_cov_smooth, z_cov_smooth, by = c(
      "a_iso" = "a_iso",
      "adm_date" = "adm_date"
    )
  )
  d_cov_smooth <- d_cov_smooth %>%
  mutate(change = adm_fv - adm_fv_ld)

  d_cov_smooth$adm_fv_smooth <- d_cov_smooth$adm_fv
  d_cov_smooth <- d_cov_smooth %>%
  mutate(adm_fv_smooth = if_else(change < 0, NA_real_, adm_fv_smooth))

  #Generate new receiving table
  #Isolate iso codes
  df_iso <- data.frame(d_cov_smooth$a_iso)
  colnames(df_iso) <- c("iso")

  #Remove duplicates
  df_iso <- df_iso %>% distinct(iso)
  df_iso_2 <- df_iso
  df_iso_2$date <- as.Date("2021-01-01")
  df_iso_2$index <- 0

  #Create date_week matrix
  df_iso_full <- data.frame(df_iso[rep(seq_len(nrow(df_iso)),
    each = as.numeric(refresh_date-as.Date("2021-01-01")-1)), ])
  df_iso_full$date <- NA
  colnames(df_iso_full) <- c("iso", "date")
  df_iso_week <- as.matrix(
    1:(as.numeric(refresh_date - as.Date("2021-01-01")) - 1)
  )
  iso_week_temp <- rep(1:nrow(df_iso_week), df_iso_week[,1], nrow(df_iso_full))
  df_iso_week <- data.frame(df_iso_week[iso_week_temp,])
  df_iso_full <- cbind(df_iso_full, df_iso_week)
  colnames(df_iso_full) <- c("iso", "date", "index")

  df_iso_new <- rbind(df_iso_2, df_iso_full)

  df_iso_new <- df_iso_new %>%
  mutate(date = if_else(is.na(date), as.Date("2021-01-01") + days(index), date))

  df_iso_new <- select(df_iso_new, c("iso", "date"))

  d_cov_smooth_new <- left_join(df_iso_new, d_cov_smooth,
    by = c("iso" = "a_iso", "date" = "adm_date"))
  d_cov_smooth_new <- arrange(d_cov_smooth_new,iso, date)

  d_cov_smooth_new <- d_cov_smooth_new %>%
    group_by(iso) %>% 
    fill(adm_fv_smooth) %>%
    
    mutate(adm_fv_smooth = replace(adm_fv_smooth, is.na(adm_fv_smooth), 0))

  d_cov_smooth_new <- select(
    d_cov_smooth_new, c("iso", "date", "adm_fv_smooth")
  )

  b_vxrate_amc_smooth <- right_join(
    b_vxrate_amc, d_cov_smooth_new, by = c("a_iso" = "iso", "adm_date" = "date")
  )

  b_vxrate_amc_smooth <- arrange(b_vxrate_amc_smooth, a_iso, adm_date)

  b_vxrate_amc_smooth <- b_vxrate_amc_smooth %>%
    group_by(a_iso) %>%
    fill(a_name_short) %>%
    fill(a_pop) %>%
    fill(a_who_region) %>%
    fill(a_covax_status) %>%
    fill(a_income_group) %>%
    fill(a_csc_status) %>%
    fill(a_ifc_status) %>%
    fill(dvr_4wk_td) %>%
    fill(dvr_4wk_td_per)

  b_vxrate_amc_smooth <- b_vxrate_amc_smooth %>%
    mutate(cov_total_fv_smooth = adm_fv_smooth / a_pop)
  return(b_vxrate_amc_smooth)
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
    select(c_vxrate_sept, c("a_iso", "t10_goalmet_sep"))

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
      c("a_iso", "t20_goalmet_dec", "t40_goalmet_dec"
    )
  )
  return(c_vxrate_dec_t2040)

}

transform_abspt_by_month <- function(b_vxrate) {
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

  c_vxrate_eom$adm_date_month_name <- helper_mapping_months(
    c_vxrate_eom$adm_date_month,
    "2022-05"
  )

  return(c_vxrate_eom)
}

# Create per country absorption table
absorption_per_country <- function(c_vxrate_eom) {
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

  d_absorption_country$adm_date_month_name <- helper_mapping_months(
    d_absorption_country$adm_date_month,
    "2022-05"
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
    read_excel("data/_input/static/supply.xlsx",
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

new_absorption_countries <- function(c_vxrate_eom) {
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

    d_absorption_country_new$month_name <- helper_mapping_months(
      d_absorption_country_new$adm_date_month,
      "2022-05"
    )

    return(d_absorption_country_new)
}

second_supplies <- function(d_absorption_country_new, combined,
  d_absorb_red, entity_characteristics, b_supply_red) {
  print(" >> Loading supplies data for second supplies...")
  b_supply_second <- data.frame(
    read_excel("data/_input/static/supply.xlsx", sheet = "supply")
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
    left_join(.,entity_characteristics, by = c("iso" = "a_iso")
  )
  return(combined_three)
}

absorption_sum_by_month <- function(c_vxrate_eom) {
  print(" >> Summarize absorption by grouping by month...")
  ## Summarize absorption by grouping by month

  groupby_and_summarize <- function(c_vxrate) {
    suffix <- substr(deparse(substitute(c_vxrate)), 14, 30)
    return(c_vxrate %>%
      group_by(adm_date_month) %>%
      summarize("absorption_{suffix}" := sum(adm_td_absorbed))
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

    ### Add full date names for visualization
    d_absorption$adm_date_month_name <- helper_mapping_months(
      d_absorption$adm_date_month,
      "2022-05"
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
