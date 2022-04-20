
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
        "a_who_status",
        "a_csl_status",
        "a_ifc_status",
        "a_continent_sub",
        "ndvp_mid_target",
        "ndvp_mid_deadline",
        "ndvp_mid_rep_rate",
        "jj_policy"
      )
    )

    return(c_vxrate_latest_red)
}

merge_dataframes <- function(
  entity_characteristics,
  c_vxrate_latest_red,
  population_data,
  uptake_gender_data,
  b_who_dashboard,
  b_smartsheet,
  supply_secured,
  delivery_courses_doses,
  b_dp,
  c_delivery_product,
  b_fin_fund_del_sum
  ) {
    # Renaming iso columns to a_iso before merge
    df_list <- list(
      entity_characteristics,
      c_vxrate_latest_red,
      population_data,
      uptake_gender_data,
      b_who_dashboard,
      b_smartsheet,
      supply_secured,
      delivery_courses_doses,
      b_dp,
      c_delivery_product,
      b_fin_fund_del_sum
    )
    # Merge details
    a_data <- helper_join_dataframe_list(
      df_list,
      join_by = "a_iso"
    )
  return(as.data.frame(a_data))
}

transform_vxrate_merge <- function(a_data) {
  # Set static dates
  print(" >>> Setting static dates")
  refresh_date <- as.Date("2022-03-30")
  t70_deadline <- as.Date("2022-06-30")
  timeto_t70 <<- as.numeric(t70_deadline - refresh_date)
  a_data$refresh_date <- refresh_date

  #Calculate JJ proportion
  print(" >>> Computing JJ doses KPIs")
  a_data <- a_data %>%
      mutate(del_dose_minjj = del_dose_total  - del_dose_jj) %>% 
      mutate(del_dose_jj_prop = if_else(is.na(del_dose_jj), 0,
                                        (del_dose_jj / del_dose_total)))

  # Calculate introduction status
  print(" >>> Computing introduction status...")
  a_data <- a_data %>%
    mutate(intro_status = if_else(
      is.na(adm_td) | adm_td == 0,
      "No product introduced",
      "Product introduced"
    )
  )
  
  a_data <- a_data %>%
    mutate(csl_status_numb = if_else(a_csl_status == "Concerted support country", 1, NA_real_))
  

  # Assign population size category
  # TODO Find a way to include the max value of max(a_data$a_pop_cat))
  # FIXME Don't hard code the max value
  breaks <- c(0, 1000000, 10000000, 100000000, 2000000000)
  tags <- c("1) <1M", "2) 1-10M", "3) 10-100M", "4) 100M+")
  a_data$a_pop_cat <- cut(
    a_data$a_pop,
    breaks = breaks,
    include.lowest = TRUE,
    right = FALSE,
    labels = tags
  )
  
  # Calculate population percentages
  print(" >>> Computing pop percentages...")
  a_data <- a_data %>%
    mutate(a_pop_10 = a_pop * 0.1) %>%
    mutate(a_pop_20 = a_pop * 0.2) %>%
    mutate(a_pop_40 = a_pop * 0.4) %>%
    mutate(a_pop_70 = a_pop * 0.7)
  
  # Calculate population proportions
  print(" >>> Computing pop proportions...")
  a_data <- a_data %>%
    mutate(a_pop_18p_prop = a_pop_18p / a_pop_2021) %>%
    mutate(a_pop_18u_prop = a_pop_18u / a_pop_2021) %>%
    mutate(a_pop_hcw_prop = a_pop_hcw / a_pop_2021) %>%
    mutate(a_pop_60p_prop = a_pop_60p / a_pop_2021) %>%
    mutate(a_pop_12p_prop = a_pop_12p / a_pop_2021) %>%
    mutate(a_pop_12u_prop = a_pop_12u / a_pop_2021)

  # Calculate theoretical fully vaccinated for non-reporters for current, lm, and 2m
  print(" >>> Computing theoreticaally fully vaxxed for non reporters...")
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
  mutate(adm_fv_13jan_homo = if_else(
      adm_a1d_13jan == 0 & adm_fv_13jan == 0 & adm_booster_13jan == 0, (adm_td_13jan / 2),
      if_else(adm_a1d_13jan == 0 & adm_fv_13jan == 0 & adm_booster_13jan != 0, ((adm_td_13jan - adm_booster_13jan)/ 2),
      if_else(adm_a1d_13jan != 0 & adm_fv_13jan == 0 & adm_booster_13jan == 0, (adm_td_13jan - adm_a1d_13jan),
      if_else(adm_a1d_13jan != 0 & adm_fv_13jan == 0 & adm_booster_13jan != 0, (adm_td_13jan - adm_a1d_13jan - adm_booster_13jan),
      (adm_fv_13jan)))))) %>%
  mutate(adm_a1d_homo = if_else(
    adm_a1d == 0 & adm_fv == 0, (adm_td / 2),
    adm_a1d)) %>%
  mutate(adm_td_per = adm_td / a_pop) %>%
  mutate(adm_pv = if_else((adm_a1d - adm_fv) < 0, 0, (adm_a1d - adm_fv)))
  
  # Calculate td and fv change from lm and 2m
  print(" >>> Computing td and fv change from lm and 2m...")
  a_data <- a_data %>%
    mutate(adm_td_less_1m = adm_td - adm_td_lm) %>%
    mutate(adm_td_1m_2m = adm_td_lm - adm_td_2m) %>%
    mutate(adm_fv_less_1m = adm_fv_homo - adm_fv_lm_homo) %>%
    mutate(adm_fv_1m_2m = adm_fv_lm_homo - adm_fv_2m_homo)

  # Calculate adm_a1d and adm_fv coverage for current, lm, and 2m, including change
  print(" >>> Computing adm_a1d and adm_fv coverage...")
  a_data <- a_data %>%
    mutate(cov_total_a1d = adm_a1d / a_pop) %>%
    mutate(cov_total_a1d_adjust = if_else(adm_a1d <= adm_fv, NA_real_, (adm_a1d / a_pop))) %>%
    mutate(cov_total_a1d_13jan = adm_a1d_13jan / a_pop) %>%
    mutate(cov_total_fv = if_else((adm_fv_homo / a_pop) > 1, 1, (adm_fv_homo / a_pop))) %>%
    mutate(cov_total_fv_theo = (adm_td / 2) / a_pop) %>%
    mutate(cov_total_fv_lw = adm_fv_lw / a_pop) %>%
    mutate(cov_total_fv_13jan = adm_fv_13jan_homo / a_pop) %>%
    mutate(cov_total_fv_lm = if_else((adm_fv_lm_homo / a_pop) > 1, 1, (adm_fv_lm_homo / a_pop))) %>%
    mutate(cov_total_fv_2m = if_else((adm_fv_2m_homo / a_pop) > 1, 1, (adm_fv_2m_homo / a_pop))) %>%
    mutate(cov_total_fv_less_1m = if_else((cov_total_fv - cov_total_fv_lm) < 0, 0, (cov_total_fv - cov_total_fv_lm)))  %>%
    mutate(cov_total_fv_1m_2m = if_else((cov_total_fv_lm - cov_total_fv_2m) < 0, 0, (cov_total_fv_lm - cov_total_fv_2m))) %>%
    mutate(cov_total_fv_cur_13jan= if_else((cov_total_fv - cov_total_fv_13jan) < 0, 0, (cov_total_fv - cov_total_fv_13jan))) %>%
    mutate(cov_total_fv_less_1m_prop = cov_total_fv_less_1m / cov_total_fv)


  # Assign coverage category for current and lw
  # FIXME Find how to include max as opposed to 1 in breaks
  print(" >>> Assigning coverage category for current and lw...")
  breaks <- c(0, 0.01, 0.1, 0.2, 0.4, 0.7, 1)
  tags <- c("1) 0-1%", "2) 1-10%", "3) 10-20%", "4) 20-40%", "5) 40%-70%", "6) 70%+")
  a_data$cov_total_fv_cat <- cut(
    a_data$cov_total_fv,
    breaks = breaks,
    include.lowest = TRUE,
    right = FALSE,
    labels = tags
  )
  # FIXME Find how to include max as opposed to 1 in breaks
  breaks <- c(0, 0.01, 0.1, 0.2, 0.4, 0.7, 1)
  tags <- c("1) 0-1%", "2) 1-10%", "3) 10-20%", "4) 20-40%", "5) 40%-70%", "6) 70%+")
  a_data$cov_total_fv_lw_cat <- cut(
    a_data$cov_total_fv_lw,
    breaks = breaks,
    include.lowest = TRUE,
    right = FALSE,
    labels = tags
  )

  # Calculate linear population coverage projection by 30 June 2022
  print(" >>> Computing linear population coverage projection by 30 June 2022...")
  a_data <- a_data %>%
    mutate(cov_total_fv_atpace_30jun = if_else(((adm_fv_homo + (dvr_4wk_fv * (
      timeto_t70
    ))) / a_pop) > 1, 1, ((adm_fv_homo + (dvr_4wk_fv * (
      timeto_t70
    ))) / a_pop)))


  # Indicator reporting status for target group-specific uptake data
  print(" >>> Indicator reporting status for target group-specific uptake data...")
  a_data <- a_data %>%
    mutate(adm_fv_hcw_repstat = if_else(adm_fv_hcw > 0, "Reporting", NA_character_)) %>%
    mutate(adm_fv_60p_repstat = if_else(adm_fv_60p > 0, "Reporting", NA_character_)) %>%
    mutate(adm_fv_gen_repstat = if_else(adm_fv_female > 0, "Reporting", NA_character_))

  # Calculate target group coverage figures
  print(" >>> Computing target group coverage figures...")
    a_data <- a_data %>%
    mutate(adm_a1d_hcw_homo = if_else(adm_a1d_hcw > a_pop_hcw, a_pop_hcw, adm_a1d_hcw)) %>%
    mutate(adm_fv_hcw_homo = if_else(adm_fv_hcw > a_pop_hcw, a_pop_hcw, adm_fv_hcw)) %>%
    mutate(cov_hcw_a1d = if_else((adm_a1d_hcw / a_pop_hcw) > 1, 1, (adm_a1d_hcw / a_pop_hcw))) %>%
    mutate(cov_hcw_fv = if_else((adm_fv_hcw_homo / a_pop_hcw) > 1, 1, (adm_fv_hcw_homo / a_pop_hcw))) %>%
    mutate(adm_fv_male_homo = if_else(adm_fv_male > a_pop_male, a_pop_male, adm_fv_male)) %>%
    mutate(cov_total_male_fv = adm_fv_male / a_pop_male) %>%
    mutate(adm_fv_fem_homo = if_else(adm_fv_female > a_pop_female, a_pop_female, adm_fv_female)) %>%
    mutate(cov_total_fem_fv = adm_fv_female / a_pop_female) %>%
    mutate(adm_fv_60p_homo = if_else(adm_fv_60p > a_pop_60p, a_pop_60p, adm_fv_60p)) %>%
    mutate(cov_60p_a1d = if_else((adm_a1d_60p / a_pop_60p) > 1, 1, (adm_a1d_60p / a_pop_60p))) %>%
    mutate(cov_60p_fv = if_else((adm_fv_60p / a_pop_60p) > 1, 1, (adm_fv_60p / a_pop_60p))) %>%
    mutate(adm_fv_gen = adm_fv_male_homo + adm_fv_fem_homo)
  
  # Calculate gender coverage difference in reporting countries
  print(" >>> Computing gender coverage difference in reporting countries...")
  a_data <- a_data %>%
    mutate(cov_total_gen_diff = cov_total_fem_fv - cov_total_male_fv)
    
    # Coverage categories in target groups
    a_data <- a_data %>%
      mutate(cov_hcw_fv_cat = if_else(
        cov_hcw_fv < 0.1,
        "1) 1-10%",
        if_else(
          cov_hcw_fv < 0.2,
          "2) 10-20%",
          if_else(
            cov_hcw_fv < 0.4,
            "3) 20-40%",
            if_else(
              cov_hcw_fv < 0.7,
              "4) 40-70%",
              if_else(cov_hcw_fv >= 0.7, "5) 70%+",
                      NA_character_)
            )
          )
        )
      )) %>%
      
      mutate(cov_60p_fv_cat = if_else(
        cov_60p_fv < 0.1,
        "1) 1-10%",
        if_else(
          cov_60p_fv < 0.2,
          "2) 10-20%",
          if_else(
            cov_60p_fv < 0.4,
            "3) 20-40%",
            if_else(
              cov_60p_fv < 0.7,
              "4) 40-70%",
              if_else(cov_60p_fv >= 0.7, "5) 70%+",
                      NA_character_)
            )
          )
        )
      )
    )
    
  # Calculate 4-week average daily rates as % of pop.
  print(" >>> Computing 4-week average daily rates as % of pop...")
  a_data <- a_data %>%
    mutate(dvr_4wk_fv = if_else(dvr_4wk_fv < 0, 0, dvr_4wk_fv)) %>%
    mutate(dvr_4wk_td_per = dvr_4wk_td / a_pop) %>%
    mutate(dvr_4wk_fv_per = dvr_4wk_fv / a_pop) %>%
    mutate(dvr_4wk_td_max_per = dvr_4wk_td_max / a_pop)

  # Assign vaccination rate category
  print(" >>> Assigning vaccination rate category...")
  breaks <- c(0, 0.0015, 0.0035, 0.0065, 1)
  tags <- c("1) Low (< 0.15%*)", "2) Medium (< 0.35%)", "3) High (< 0.65%)", "4) Very high (> 0.65%)") #nolint 
  a_data$dvr_4wk_td_per_cat <- cut(
    a_data$dvr_4wk_td_per,
    breaks = breaks,
    include.lowest = TRUE,
    right = FALSE,
    labels = tags
  )

  # Calculate (percent) change in 4-week average daily vaccination rate & assign category
  print(" >>> Computing % change in 4-week average daily vxrate & assign category...")
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

 # breaks <- c(-1, -0.25, 0.25, 0, 1)
    # tags <- c("1) < (-25)%", "4) > 25%", "2) (-25)-0%", "3) 0-25%") #nolint 
    # a_data$dvr_4wk_td_change_lm_per_cat <- cut(
    #   a_data$dvr_4wk_td_change_lm_per,
    #   breaks = breaks,
    #   include.lowest = TRUE,
    #   right = FALSE,
    #   labels = tags
    # )

 # a_data$dvr_4wk_td_change_lm_trend <- c(-1, -0.25, 0.25, 1)
    # tags <- c("Downward", "Upward", "Stable") #nolint 
    # group_tags <- cut(
    #   a_data$dvr_4wk_td_change_lm_per,
    #   breaks = breaks,
    #   include.lowest = TRUE,
    #   right = FALSE,
    #   labels = tags
    # )