
extract_vxrate_details <- function(c_vxrate_latest) {
  print(" >> Remove duplicative base details from latest vxrate summary...")
  c_vxrate_latest_red <-
    select(
      c_vxrate_latest, -c(
        "a_continent",
        "a_who_region",
        "a_income_group",
        "a_covax_status",
        "a_unicef_region",
        "a_name_short",
        "a_name_long",
        "a_who_subregion",
        "a_who_status"
      )
    )

    return(c_vxrate_latest_red)
}

merge_dataframes <- function(entity_characteristics, c_vxrate_latest_red, population_data, uptake_gender_data, b_who_dashboard) {
  # Merge details
  a_data <-
    left_join(entity_characteristics, c_vxrate_latest_red, by = "a_iso") %>%
    left_join(., population_data) %>%
    left_join(., uptake_gender_data)

  # Merge WHO Dashboard data
  a_data <- left_join(a_data, b_who_dashboard, by = c("a_iso" = "iso"))


  return(a_data)
}

transform_vxrate_merge <- function(a_data) {
  # Calculate introduction status
  a_data <- a_data %>%
    mutate(intro_status = if_else(
      is.na(adm_td) | adm_td == 0,
      "No product introduced",
      "Product introduced"
    )
  )
  # Assign population size category
  a_data <- a_data %>%
  mutate(a_pop_cat = if_else(
    a_pop < 1000000,
    "1) <1M",
    if_else(
      a_pop < 10000000,
      "2) 1-10M",
      if_else(
        a_pop < 100000000,
        "3) 10-100M",
        if_else(
          a_pop >= 100000000,
          "4) 100M+",
          NA_character_
          )
        )
      )
    )
  )

  # Calculate theoretical fully vaccinated for non-reporters for current, lm, and 2m
  a_data <- a_data %>%
  mutate(adm_fv_homo = if_else(
    adm_a1d == 0 & adm_fv == 0 & adm_booster == 0, (adm_td / 2),
    if_else(adm_a1d == 0 & adm_fv == 0 & adm_booster != 0, ((adm_td - adm_booster)/ 2),
    if_else(adm_a1d != 0 & adm_fv == 0 & adm_booster == 0, (adm_td - adm_a1d),
    if_else(adm_a1d != 0 & adm_fv == 0 & adm_booster != 0, (adm_td - adm_a1d - adm_booster),
    (adm_fv)))))) %>%
    mutate(adm_fv_lm_homo = if_else(
    adm_a1d_lm == 0 & adm_fv_lm == 0 & adm_booster_lm == 0, (adm_td_lm / 2),
    if_else(adm_a1d_lm == 0 & adm_fv_lm == 0 & adm_booster_lm != 0, ((adm_td_lm - adm_booster_lm)/ 2),
    if_else(adm_a1d_lm != 0 & adm_fv_lm == 0 & adm_booster_lm == 0, (adm_td_lm - adm_a1d_lm),
    if_else(adm_a1d_lm != 0 & adm_fv_lm == 0 & adm_booster_lm != 0, (adm_td_lm - adm_a1d_lm - adm_booster_lm),
    (adm_fv_lm)))))) %>%
    mutate(adm_fv_2m_homo = if_else(
    adm_a1d_2m == 0 & adm_fv_2m == 0, (adm_td_2m / 2),
            if_else(adm_a1d_2m != 0 & adm_fv_2m == 0, (adm_td_2m - adm_a1d_2m),
                            (adm_fv_2m)))) %>%
  
  mutate(adm_a1d_homo = if_else(
    adm_a1d == 0 & adm_fv == 0, (adm_td / 2),
    adm_a1d))

  # Calculate td and fv change from lm and 2m
  a_data <- a_data %>%
    mutate(adm_td_less_1m = adm_td - adm_td_lm) %>%
    
    mutate(adm_td_1m_2m = adm_td_lm - adm_td_2m) %>%
    
    mutate(adm_fv_less_1m = adm_fv_homo - adm_fv_lm_homo) %>%
    
    mutate(adm_fv_1m_2m = adm_fv_lm_homo - adm_fv_2m_homo)


  # Calculate adm_a1d and adm_fv coverage for current, lm, and 2m, including change
  a_data <- a_data %>%
    mutate(cov_total_a1d = adm_a1d / a_pop) %>%
    
    mutate(cov_total_fv = if_else(a_who_region == "EUR", cov_total_fv_per100 / 100,
                                    if_else((adm_fv_homo / a_pop) > 1, 1, (adm_fv_homo / a_pop)))) %>%
    
    mutate(cov_total_fv_lw = adm_fv_lw / a_pop) %>%
    
    mutate(cov_total_fv_lm = if_else((adm_fv_lm_homo / a_pop) > 1, 1, (adm_fv_lm_homo / a_pop))) %>%
    
    mutate(cov_total_fv_2m = if_else((adm_fv_2m_homo / a_pop) > 1, 1, (adm_fv_2m_homo / a_pop))) %>%
    
    mutate(cov_total_fv_less_1m = if_else((cov_total_fv - cov_total_fv_lm) < 0, 0, (cov_total_fv - cov_total_fv_lm)))  %>%
    
    mutate(cov_total_fv_1m_2m = if_else((cov_total_fv_lm - cov_total_fv_2m) < 0, 0, (cov_total_fv_lm - cov_total_fv_2m))) %>%
    
    mutate(cov_total_fv_less_1m_prop = cov_total_fv_less_1m / cov_total_fv)


  # Assign coverage category for current and lw
  a_data <- a_data %>%
    mutate(cov_total_fv_cat = if_else(
      cov_total_fv < 0.01,
      "1) 0-1%",
      if_else(
        cov_total_fv < 0.1,
        "2) 1-10%",
        if_else(
          cov_total_fv < 0.2,
          "3) 10-20%",
          if_else(
            cov_total_fv < 0.4,
            "4) 20-40%",
            if_else(cov_total_fv >= 0.4, "5) 40%+",
                    NA_character_)
          )
        )
      )
    )) %>%
    mutate(cov_total_fv_lw_cat = if_else(
      cov_total_fv_lw < 0.01,
      "1) 0-1%",
      if_else(
        cov_total_fv_lw < 0.1,
        "2) 1-10%",
        if_else(
          cov_total_fv_lw < 0.2,
          "3) 10-20%",
          if_else(
            cov_total_fv_lw < 0.4,
            "4) 20-40%",
            if_else(cov_total_fv_lw >= 0.4, "5) 40%+",
                    NA_character_)
          )
        )
      )
    ))


  # Calculate linear population coverage projection by 30 June 2022
  a_data <- a_data %>%
    mutate(cov_total_fv_atpace_30jun = if_else(((adm_fv_homo + (dvr_4wk_fv * (
      as.numeric(as.Date("2022-06-30") - Sys.Date())
    ))) / a_pop) > 1, 1, ((adm_fv_homo + (dvr_4wk_fv * (
      as.numeric(as.Date("2022-06-30") - Sys.Date())
    ))) / a_pop)))


  # Indicator reporting status for target group-specific uptake data
  a_data <- a_data %>%
    mutate(adm_fv_hcw_repstat = if_else(adm_fv_hcw > 0, "Reporting", NA_character_)) %>%
    
    mutate(adm_fv_60p_repstat = if_else(adm_fv_60p > 0, "Reporting", NA_character_)) %>%
    
    mutate(adm_fv_gen_repstat = if_else(adm_fv_female > 0, "Reporting", NA_character_))


  # Calculate target group coverage figures
  a_data <- a_data %>%
    mutate(adm_fv_hcw_homo = if_else(adm_fv_hcw > a_pop_hcw, a_pop_hcw, adm_fv_hcw)) %>%
    
    mutate(cov_hcw_fv = if_else((adm_fv_hcw_homo / a_pop_hcw) > 1, 1, (adm_fv_hcw_homo / a_pop_hcw))) %>%
    
    mutate(adm_fv_gen = adm_fv_male + adm_fv_female) %>%
    
    mutate(cov_total_male_fv = adm_fv_male / a_pop_male) %>%
    
    mutate(cov_total_fem_fv = adm_fv_female / a_pop_female) %>%
    
    mutate(cov_60p_fv = adm_fv_60p / a_pop_60p)

  # Calculate gender coverage difference in reporting countries
  a_data <- a_data %>%
    mutate(cov_total_gen_diff = cov_total_fem_fv - cov_total_male_fv)
    

  # Calculate 4-week average daily rates as % of pop.
  a_data <- a_data %>%
    mutate(dvr_4wk_td_per = dvr_4wk_td / a_pop) %>%
    
    mutate(dvr_4wk_fv_per = dvr_4wk_fv / a_pop) %>%
    
    mutate(dvr_4wk_td_max_per = dvr_4wk_td_max / a_pop)


  # Assign vaccination rate category
  a_data <- a_data %>%
    mutate(dvr_4wk_td_per_cat = if_else(
      dvr_4wk_td_per < 0.0015,
      "1) Low (< 0.15%*)",
      if_else(
        dvr_4wk_td_per < 0.0035,
        "2) Medium (< 0.35%)",
        if_else(
          dvr_4wk_td_per < 0.0065,
          "3) High (< 0.65%)",
          if_else(dvr_4wk_td_per >= 0.0065, "4) Very high (> 0.65%)",
                  NA_character_)
        )
      )
    ))


  # Calculate (percent) change in 4-week average daily vaccination rate & assign category
  a_data <- a_data %>%
    mutate(dvr_4wk_td_change_lm = dvr_4wk_td - dvr_4wk_td_lm) %>%
    mutate(dvr_4wk_td_change_lm_per = dvr_4wk_td_change_lm / dvr_4wk_td_lm) %>%
    
    mutate(
      dvr_4wk_td_change_lm_per_cat = if_else(
        dvr_4wk_td_change_lm_per <= -0.25,
        "1) < (-25)%",
        if_else(
          dvr_4wk_td_change_lm_per >= 0.25,
          "4) > 25%",
          if_else(
            dvr_4wk_td_change_lm_per <= 0,
            "2) (-25)-0%",
            if_else(dvr_4wk_td_change_lm_per > 0, "3) 0-25%",
            NA_character_)
          )
        )
      )
    ) %>%
    mutate(
      dvr_4wk_td_change_lm_trend = if_else(
        dvr_4wk_td_change_lm_per <= -0.25,
        "Downward",
        if_else(
          dvr_4wk_td_change_lm_per >= 0.25,
          "Upward",
          if_else(
            dvr_4wk_td_change_lm_per < 0.25 &
              dvr_4wk_td_change_lm_per > -0.25,
            "Stable",
            NA_character_
          )
        )
      )
    )

  return(a_data)
}