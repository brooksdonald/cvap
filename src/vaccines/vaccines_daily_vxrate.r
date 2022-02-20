
# load current daily vxrate
load_b_vxrate <- function {
    print(" >> Loading current daily vaccination rate...")
    b_vxrate <-
        data.frame(
            read_excel("input/base_dvr_current.xlsx",
            sheet = "data"
        )
    )

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

transform_b_vxrate <- function(b_vxrate) {
    print(" >> Transforming current vxrate...")
    
    ## Change population field type to numeric
    b_vxrate$a_pop <- as.numeric(b_vxrate$a_pop)
    
    ## Add entity base data
    ### Import entity_details from entity base data
    source("src/entity/entity_characteristics.r")
    b_vxrate <- left_join(b_vxrate, entity_details, by = "a_iso")
    
    ## Add year, month, and week numbers
    b_vxrate <- b_vxrate %>%
      mutate(adm_date_year = if_else(
        year(adm_date) == 2021,
        2021,
        if_else(year(adm_date) == 2022, 2022,
        NA_real_
        )
      )
    ) %>% 
    
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
        mutate (
          adm_latest = if_else(
            adm_date == max(adm_date), 
            "Yes", "No"
          )
        )

    ## Indicate if latest entry is reported within the current or past week
    b_vxrate$adm_is_current <- ifelse(
      b_vxrate$adm_latest == "Yes" & 
        (
          b_vxrate$adm_date_week == isoweek(Sys.Date()) 
            | b_vxrate$adm_date_week == isoweek(Sys.Date()) - 1
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
        mutate (
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
transform_subset_b_vxrate_pub <- function(b_vxrate) {
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
          "a_income_group"
        )
      )
    
    ### Calculate dvr_4wk_td_per in long form subsets
    b_vxrate_pub <- b_vxrate_pub %>%
      mutate(dvr_4wk_td_per = dvr_4wk_td / a_pop)

    return(b_vxrate_pub)

}

transform_subset_b_vxrate_amc <- function(b_vxrate) {
  print(" >> Create clean long form subsets for b_vxrate_amc")
  b_vxrate_amc <- filter(b_vxrate, a_covax_status == "AMC")
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
        "a_income_group"
      )
    )
    
    b_vxrate_amc <- b_vxrate_amc %>%
      mutate(dvr_4wk_td_per = dvr_4wk_td / a_pop)

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
  c_vxrate_sept <- c_vxrate_sept %>%
    mutate(
      cov_total_fv = if_else(
        adm_a1d == 0 & adm_fv == 0 & adm_booster == 0, ((adm_td / 2) / a_pop),
          if_else(adm_a1d == 0 & adm_fv == 0 & adm_booster != 0, (((adm_td - adm_booster)/ 2) / a_pop),
            if_else(adm_a1d != 0 & adm_fv == 0 & adm_booster == 0, ((adm_td - adm_a1d) / a_pop),
              if_else(adm_a1d != 0 & adm_fv == 0 & adm_booster != 0, ((adm_td - adm_a1d - adm_booster) / a_pop),
                (adm_fv / a_pop)
              )
            )
          )
        )
      )
        
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
  c_vxrate_dec <- c_vxrate_dec %>% 
    mutate(
      cov_total_fv = if_else(
        adm_a1d == 0 & adm_fv == 0 & adm_booster == 0, ((adm_td / 2) / a_pop),
          if_else(adm_a1d == 0 & adm_fv == 0 & adm_booster != 0, (((adm_td - adm_booster)/ 2) / a_pop),
            if_else(adm_a1d != 0 & adm_fv == 0 & adm_booster == 0, ((adm_td - adm_a1d) / a_pop),
              if_else(adm_a1d != 0 & adm_fv == 0 & adm_booster != 0, ((adm_td - adm_a1d - adm_booster) / a_pop),
                (adm_fv / a_pop)
              )
            )
          )
        )
      )

  #### Indicate if cov_total_fv is greater than or equal to 20%
  c_vxrate_dec <- c_vxrate_dec %>%
    mutate(t20_goalmet_dec = if_else(cov_total_fv > .2, "Yes", "No"))

  #### Indicate if cov_total_fv is greater than or equal to 40%
  c_vxrate_dec <- c_vxrate_dec %>%
    mutate(t40_goalmet_dec = if_else(cov_total_fv > .4, "Yes", "No"))

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
        "a_income_group",
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
    mutate(adm_td_absorbed = if_else(is.na(adm_td_absorbed), adm_td, adm_td_absorbed))

  return(c_vxrate_eom)
}

absorption_sum_by_month <- function(c_vxrate_eom) {
  print(" >> Summarize absorption by grouping by month...")
  ## Summarize absorption by grouping by month
  ### COVAX participation = AMC
  c_vxrate_eom_amc <- filter(c_vxrate_eom, a_covax_status == "AMC")
  d_absorption_amc <- c_vxrate_eom_amc %>%
    group_by(adm_date_month) %>%
    summarize(absorption_amc = sum(adm_td_absorbed))

  #### COVAX participation = AMC91
  c_vxrate_eom_amc91 <-
    filter(c_vxrate_eom, a_covax_status == "AMC" & a_iso != "IND")
  
  d_absorption_amc91 <- c_vxrate_eom_amc91 %>%
    group_by(adm_date_month) %>%
    summarize(absorption_amc91 = sum(adm_td_absorbed))

  #### Continent = Africa
  c_vxrate_eom_africa <- filter(c_vxrate_eom, a_continent == "Africa")
  d_absorption_africa <- c_vxrate_eom_africa %>%
    group_by(adm_date_month) %>%
    summarize(absorption_africa = sum(adm_td_absorbed))

  #### WHO region = EMR
  c_vxrate_eom_emr <- filter(c_vxrate_eom, a_who_region == "EMR")
  d_absorption_emr <- c_vxrate_eom_emr %>%
    group_by(adm_date_month) %>%
    summarize(absorption_emr = sum(adm_td_absorbed))

  #### WHO region = AFR
  c_vxrate_eom_afr <- filter(c_vxrate_eom, a_who_region == "AFR")
  d_absorption_afr <- c_vxrate_eom_afr %>%
    group_by(adm_date_month) %>%
    summarize(absorption_afr = sum(adm_td_absorbed))

  #### WHO region = SEAR
  c_vxrate_eom_sear <- filter(c_vxrate_eom, a_who_region == "SEAR")
  d_absorption_sear <- c_vxrate_eom_sear %>%
    group_by(adm_date_month) %>%
    summarize(absorption_sear = sum(adm_td_absorbed))

  #### WHO region = WPR
  c_vxrate_eom_wpr <- filter(c_vxrate_eom, a_who_region == "WPR")
  d_absorption_wpr <- c_vxrate_eom_wpr %>%
    group_by(adm_date_month) %>%
    summarize(absorption_wpr = sum(adm_td_absorbed))

  #### WHO region = EUR
  c_vxrate_eom_eur <- filter(c_vxrate_eom, a_who_region == "EUR")
  d_absorption_eur <- c_vxrate_eom_eur %>%
    group_by(adm_date_month) %>%
    summarize(absorption_eur = sum(adm_td_absorbed))

  #### WHO region = AMR
  c_vxrate_eom_amr <- filter(c_vxrate_eom, a_who_region == "AMR")
  d_absorption_amr <- c_vxrate_eom_amr %>%
    group_by(adm_date_month) %>%
    summarize(absorption_amr = sum(adm_td_absorbed))

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
    left_join(., d_absorption_amc91, by = "adm_date_month")

    ### Add full date names for visualization
    # TODO Refactor this
    d_absorption <- d_absorption %>%
      mutate(adm_date_month_name = if_else(
        adm_date_month == 1,
        "2021-01",
        if_else(
          adm_date_month == 2,
          "2021-02",
          if_else(
            adm_date_month == 3,
            "2021-03",
            if_else(
              adm_date_month == 4,
              "2021-04",
              if_else(
                adm_date_month == 5,
                "2021-05",
                if_else(
                  adm_date_month == 6,
                  "2021-06",
                  if_else(
                    adm_date_month == 7,
                    "2021-07",
                    if_else(
                      adm_date_month == 8,
                      "2021-08",
                      if_else(
                        adm_date_month == 9,
                        "2021-09",
                        if_else(
                          adm_date_month == 10,
                          "2021-10",
                          if_else(
                            adm_date_month == 11,
                            "2021-11",
                            if_else(
                              adm_date_month == 12,
                              "2021-12",
                              if_else(adm_date_month == 13, "2022-01",
                                      NA_character_)
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  
  return(d_absorption)
}

latest_sum_table <- function(b_vxrate) {
  print(" >> Create latest value summary table...")
  c_vxrate_latest <- filter(b_vxrate, adm_latest == "Yes")

  ### Remove is_latest column
  c_vxrate_latest <- select(c_vxrate_latest, -c("adm_latest"))

  return(c_vxrate_latest) 

} 
