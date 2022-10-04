
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
        "a_csc_status",
        "a_ifc_status",
        "a_gavi_status",
        "a_continent_sub",
        "ndvp_mid_target",
        "ndvp_mid_deadline",
        "ndvp_mid_rep_rate",
        "jj_policy",
        "older_def",
        "older_source",
        "a_pop",
        "a_income_group_vis",
        "expiry_risk",
        "ss_target",
        "ss_deadline",
        "country_source"
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

transform_vxrate_merge <- function(a_data, refresh_date, t70_deadline) {
  # Set static dates
  print(" >>> Setting static dates")
  timeto_t70 <- as.numeric(t70_deadline - refresh_date)
  a_data$a_refresh_date <- refresh_date

  #Calculate JJ proportion
  print(" >>> Computing JJ doses KPIs")
  a_data <- a_data %>%
      mutate(del_dose_minjj = del_dose_total  - del_dose_jj) %>% 
      mutate(del_dose_jj_prop = if_else(
        is.na(del_dose_jj),
        0,
        del_dose_jj / del_dose_total))

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
    mutate(csc_status_numb = if_else(
      a_csc_status == "Concerted support country",
      1,
      NA_real_))

  # Assign population size category
  breaks <- c(0, 1000000, 10000000, 100000000,
    max(a_data$a_pop, na.rm = TRUE) + 1)
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
  
  # Prepare older population value
  a_data <- a_data %>%
    mutate(a_pop_older = if_else(
      is.na(older_def), a_pop_60p,
      if_else(older_def == "45 and older", a_pop_45p,
      if_else(older_def == "50 and older", a_pop_50p,
              if_else(older_def == "55 and older", a_pop_55p,
                      if_else(older_def == "60 and older", a_pop_60p,
                              if_else(older_def == "65 and older", a_pop_65p,
                                      if_else(older_def == "70 and older", a_pop_70p,
                                      a_pop_60p))
              )
      )
    ))))

  # Calculate theoretical fully vaccinated for non-reporters for current, lm, and 2m
  print(" >>> Computing theoreticaally fully vaxxed for non reporters...")
  a_data <- a_data %>%
  mutate(adm_fv_homo = if_else(
    adm_a1d == 0 & adm_fv == 0 & adm_booster == 0,
    adm_td / 2,
      if_else(
        adm_a1d == 0 & adm_fv == 0 & adm_booster != 0,
        (adm_td - adm_booster)/ 2,
        if_else(
          adm_a1d != 0 & adm_fv == 0 & adm_booster == 0,
          adm_td - adm_a1d,
          if_else(
            adm_a1d != 0 & adm_fv == 0 & adm_booster != 0,
            adm_td - adm_a1d - adm_booster,
            adm_fv))))) %>%
  mutate(adm_fv_lm_homo = if_else(
    adm_a1d_lm == 0 & adm_fv_lm == 0 & adm_booster_lm == 0,
    adm_td_lm / 2,
    if_else(
      adm_a1d_lm == 0 & adm_fv_lm == 0 & adm_booster_lm != 0,
      (adm_td_lm - adm_booster_lm)/ 2,
      if_else(
        adm_a1d_lm != 0 & adm_fv_lm == 0 & adm_booster_lm == 0,
        adm_td_lm - adm_a1d_lm,
        if_else(
          adm_a1d_lm != 0 & adm_fv_lm == 0 & adm_booster_lm != 0,
          adm_td_lm - adm_a1d_lm - adm_booster_lm,
          adm_fv_lm))))) %>%
  mutate(adm_fv_2m_homo = if_else(
    adm_a1d_2m == 0 & adm_fv_2m == 0,
    adm_td_2m / 2,
    if_else(
      adm_a1d_2m != 0 & adm_fv_2m == 0,
      adm_td_2m - adm_a1d_2m,
      adm_fv_2m))) %>%
  mutate(adm_fv_13jan_homo = if_else(
      adm_a1d_13jan == 0 & adm_fv_13jan == 0 & adm_booster_13jan == 0,
      adm_td_13jan / 2,
      if_else(
        adm_a1d_13jan == 0 & adm_fv_13jan == 0 & adm_booster_13jan != 0,
        (adm_td_13jan - adm_booster_13jan)/ 2,
        if_else(
          adm_a1d_13jan != 0 & adm_fv_13jan == 0 & adm_booster_13jan == 0,
          adm_td_13jan - adm_a1d_13jan,
          if_else(
            adm_a1d_13jan != 0 & adm_fv_13jan == 0 & adm_booster_13jan != 0,
            adm_td_13jan - adm_a1d_13jan - adm_booster_13jan,
            adm_fv_13jan))))) %>%
  mutate(adm_a1d_homo = if_else(
    adm_a1d == 0 & adm_fv == 0,
    adm_td / 2,
    adm_a1d)) %>%
  mutate(adm_td_per = adm_td / a_pop) %>%
  mutate(adm_pv = pmax(0, adm_a1d - adm_fv))
  
  # Calculate td and fv change from lm and 2m
  print(" >>> Computing td and fv change from lm and 2m...")
  a_data <- a_data %>%
    mutate(adm_td_less_1m = adm_td - adm_td_lm) %>%
    mutate(adm_td_1m_2m = adm_td_lm - adm_td_2m) %>%
    mutate(adm_td_1m_13jan = adm_td_lm - adm_td_13jan) %>%
    mutate(adm_fv_less_1m = adm_fv_homo - adm_fv_lm_homo) %>%
    mutate(adm_fv_1m_2m = adm_fv_lm_homo - adm_fv_2m_homo)

  # Calculate adm_a1d and adm_fv coverage for current, lm, and 2m, including change
  print(" >>> Computing adm_a1d and adm_fv coverage...")
  a_data <- a_data %>%
    mutate(cov_total_a1d = adm_a1d / a_pop) %>%
    mutate(cov_total_a1d_adjust = if_else(
      adm_a1d <= adm_fv,
      NA_real_,
      adm_a1d / a_pop)) %>%
    mutate(cov_total_a1d_13jan = adm_a1d_13jan / a_pop) %>%
    mutate(cov_total_fv = pmin(1, adm_fv_homo / a_pop)) %>%
    mutate(cov_total_fv_theo = (adm_td / 2) / a_pop) %>%
    mutate(cov_total_fv_lw = adm_fv_lw / a_pop) %>%
    mutate(cov_total_fv_13jan = adm_fv_13jan_homo / a_pop) %>%
    mutate(cov_total_fv_lm = pmin(1, adm_fv_lm_homo / a_pop)) %>%
    mutate(cov_total_fv_2m = pmin(1, adm_fv_2m_homo / a_pop)) %>%
    mutate(cov_total_fv_less_1m = pmax(0, cov_total_fv - cov_total_fv_lm))  %>%
    mutate(cov_total_fv_1m_2m = pmax(0, cov_total_fv_lm - cov_total_fv_2m)) %>%
    mutate(cov_total_fv_cur_13jan = pmax(0, cov_total_fv - cov_total_fv_13jan)) %>%
    mutate(cov_total_fv_less_1m_prop = cov_total_fv_less_1m / cov_total_fv) %>%
    mutate(cov_total_fv_1m_13jan = pmax(cov_total_fv_lm - cov_total_fv_13jan, 0))
  
  # Correct GRL and SJM
  a_data$cov_total_fv[a_data$a_iso == "GRL"] <-
    a_data$cov_total_fv[a_data$a_iso == "DNK"]
  
  a_data$cov_total_fv[a_data$a_iso == "SJM"] <-
    a_data$cov_total_fv[a_data$a_iso == "NOR"]


  # Assign coverage category for current and lw
  print(" >>> Assigning coverage category for current and lw...")
  breaks <- c(0, 0.01, 0.1, 0.2, 0.4, 0.7, Inf)
  tags <- c("1) 0-1%", "2) 1-10%", "3) 10-20%",
    "4) 20-40%", "5) 40-70%", "6) 70%+")
  a_data$cov_total_fv_cat <- cut(
    a_data$cov_total_fv,
    breaks = breaks,
    include.lowest = TRUE,
    right = FALSE,
    labels = tags
  )

  breaks <- c(0, 0.01, 0.1, 0.2, 0.4, 0.7, Inf)
  tags <- c("1) 0-1%", "2) 1-10%", "3) 10-20%",
    "4) 20-40%", "5) 40-70%", "6) 70%+")
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
    mutate(cov_total_fv_atpace_31dec = pmin(
      1,
      (adm_fv_homo + (dvr_4wk_fv * timeto_t70)) / a_pop))


  # Indicator reporting status for target group-specific uptake data
  print(" >>> Indicator reporting status for target group-specific uptake data...")
  a_data <- a_data %>%
    mutate(adm_fv_hcw_repstat = if_else(
      is.na(adm_fv_hcw),
      "Not reporting",
      if_else(
        adm_fv_hcw > 0,
        "Reporting",
        "Not reporting"))) %>%
    mutate(adm_fv_60p_repstat = if_else(
      is.na(adm_fv_60p),
      "Not reporting",
      if_else(
        adm_fv_60p > 0,
        "Reporting",
        "Not reporting"))) %>%
    mutate(adm_fv_gen_repstat = if_else(
      is.na(adm_fv_female) | is.na(adm_fv_male),
      "Not reporting",
      if_else(
        adm_fv_female > 0,
        "Reporting",
        "Not reporting")))

  # Converting Ingested data from API to numeric values
  a_data$adm_fv_male <- as.numeric(a_data$adm_fv_male)
  a_data$adm_fv_female <- as.numeric(a_data$adm_fv_female)
  a_data$adm_a1d_hcw <- as.numeric(a_data$adm_a1d_hcw)
  a_data$adm_fv_hcw <- as.numeric(a_data$adm_fv_hcw)
  a_data$adm_fv_60p <- as.numeric(a_data$adm_fv_60p)
  
  a_data$adm_fv_hcw_repstat[a_data$a_iso == "GRL"] <-
    a_data$adm_fv_hcw_repstat[a_data$a_iso == "DNK"]
  a_data$adm_fv_hcw_repstat[a_data$a_iso == "SJM"] <-
    a_data$adm_fv_hcw_repstat[a_data$a_iso == "NOR"]
  
  a_data$adm_fv_60p_repstat[a_data$a_iso == "GRL"] <-
    a_data$adm_fv_60p_repstat[a_data$a_iso == "DNK"]
  a_data$adm_fv_60p_repstat[a_data$a_iso == "SJM"] <-
    a_data$adm_fv_60p_repstat[a_data$a_iso == "NOR"]
  
  a_data$adm_fv_gen_repstat[a_data$a_iso == "GRL"] <-
    a_data$adm_fv_gen_repstat[a_data$a_iso == "DNK"]
  a_data$adm_fv_gen_repstat[a_data$a_iso == "SJM"] <-
    a_data$adm_fv_gen_repstat[a_data$a_iso == "NOR"]
  
  # Healthcare worker
  a_data <- a_data %>%
    mutate(hcw_flag = if_else(
      a_pop_hcw > adm_target_hcw,
      "Yes",
      NA_character_)) %>%
    mutate(hcw_diff = pmax(a_pop_hcw - adm_target_hcw, 0, na.rm = TRUE))

  # Calculate target group coverage figures
  print(" >>> Computing target group coverage figures...")
  a_data$adm_fv_male <- as.double(a_data$adm_fv_male)
  a_data <- a_data %>%
    mutate(adm_fv_male_homo = pmin(
      adm_fv_male,
      a_pop_male)) %>%
    mutate(cov_total_male_fv = adm_fv_male / a_pop_male) %>%
    mutate(adm_fv_fem_homo = pmin(
      adm_fv_female,
      a_pop_female)) %>%
    mutate(cov_total_fem_fv = adm_fv_female / a_pop_female) %>%
    mutate(adm_fv_gen = adm_fv_male_homo + adm_fv_fem_homo) %>%
    mutate(adm_booster_fem_homo = pmin(a_pop_female, adm_booster_female)) %>%
    mutate(adm_booster_male_homo = pmin(a_pop_male, adm_booster_male)) %>%
    mutate(cov_total_booster_fem = adm_booster_fem_homo / a_pop_female) %>%
    mutate(cov_total_booster_male = adm_booster_male_homo / a_pop_male) %>%
    mutate(adm_booster_gen_status = if_else(
      is.na(adm_fv_male) | is.na(adm_fv_female) | adm_fv_male == 0 | adm_fv_female == 0,
      "Not reporting on gender-disaggregated uptake",
      if_else(
        is.na(cov_total_booster_fem) & (is.na(adm_fv_female) == FALSE | is.na(adm_fv_male) == FALSE),
        "Reporting on gender-disaggregated uptake, but not boosters",
        if_else(
          adm_booster_female > 0,
          "Reporting on gender-disaggregated boosters",
          "Reporting on gender-disaggregated uptake, but not boosters"))))

  # Calculate healthcare workers coverage
  a_data <- a_data %>%
    mutate(adm_a1d_hcw_homo = pmin(
      adm_a1d_hcw,
      a_pop_hcw)) %>%
    mutate(adm_fv_hcw_homo = pmin(
      adm_fv_hcw,
      a_pop_hcw)) %>%
    mutate(adm_booster_hcw_homo = pmin(
      a_pop_hcw,
      adm_booster_hcw)) %>%
    mutate(adm_fv_hcw_adjust =
      pmin(adm_fv_hcw + (hcw_diff * cov_total_fv), a_pop_hcw)) %>%
    mutate(cov_hcw_a1d = if_else(
      is.na(hcw_flag),
      pmin(
        adm_a1d_hcw / a_pop_hcw,
        1),
        pmin(
          (adm_a1d_hcw + (hcw_diff * cov_total_a1d)) / a_pop_hcw,
          1)
    )) %>%
    mutate(cov_hcw_a1d_adjust = if_else(
      adm_a1d_hcw <= adm_fv_hcw,
      NA_real_,
      cov_hcw_a1d)) %>%
    mutate(cov_hcw_fv =
      pmin(
        (adm_fv_hcw + if_else(
          is.na(hcw_flag),
          0,
          hcw_diff * cov_total_fv
        )) / a_pop_hcw,
        1
      )
    ) %>%
    mutate(cov_hcw_booster =
      pmin(
        1,
        adm_booster_hcw / a_pop_hcw)) %>%
    mutate(adm_booster_hcw_status = if_else(
      is.na(adm_fv_hcw) | adm_fv_hcw == 0,
      "3) Not reporting on HCW uptake",
      if_else(
        is.na(cov_hcw_booster) & is.na(adm_fv_hcw) == FALSE,
        "2) Reporting on HCW uptake, but not boosters",
        if_else(
          adm_booster_hcw > 0,
          "1) Reporting on HCW boosters",
          "2) Reporting on HCW uptake, but not boosters")))) %>%
    mutate(cov_hcw_booster_cat = if_else(
      is.na(adm_fv_hcw) | adm_fv_hcw == 0,
      "0) Not reporting on HCW uptake",
      if_else(
        is.na(cov_hcw_booster) & is.na(adm_fv_hcw) == FALSE,
        "1) Not reporting on HCW boosters",
        if_else(
          cov_hcw_booster > .5,
          "5) >50%",
          if_else(
            cov_hcw_booster > .25,
            "4) 25-49.9%",
            if_else(
              cov_hcw_booster > .1,
              "3) 10-24.9%",
              if_else(
                cov_hcw_booster > 0,
                "2) 0-9.9%",
                if_else(
                  cov_hcw_booster == 0,
                  "1) Not reporting on HCW boosters",
                  NA_character_)))))))) %>%
    
    mutate(cov_hcw_a1d_fv = if_else(cov_hcw_fv == 0 | is.na(cov_hcw_fv),
                                    cov_hcw_a1d,
                                    cov_hcw_a1d - cov_hcw_fv),
           cov_hcw_fv_booster = if_else(cov_hcw_booster == 0 | is.na(cov_hcw_booster),
                                        cov_hcw_fv,
                                        cov_hcw_fv - cov_hcw_booster))

  # Calculating older adults coverage groups
  a_data <- a_data %>%
    mutate(adm_fv_60p_homo = pmin(
      a_pop_older, adm_fv_60p),
      adm_a1d_60p_homo = pmin(
        a_pop_older, adm_a1d_60p)) %>%
    mutate(cov_60p_a1d = pmin(
      adm_a1d_60p / a_pop_older, 1)) %>%
    mutate(cov_60p_a1d_adjust = if_else(
      adm_a1d_60p <= adm_fv_60p,
      NA_real_,
      cov_60p_a1d)) %>%
    mutate(cov_60p_fv = pmin(
      adm_fv_60p / a_pop_older, 1)) %>%
    mutate(cov_60p_booster = pmin(
      1, adm_booster_60p / a_pop_older)) %>%
    mutate(adm_booster_60p_status = if_else(
      is.na(adm_fv_60p) | adm_fv_60p == 0,
      "3) Not reporting on 60+ uptake",
      if_else(is.na(cov_60p_booster) & is.na(adm_fv_60p) == FALSE, 
        "2) Reporting on 60+ uptake, but not boosters",
        if_else(
          adm_booster_60p > 0,
          "1) Reporting on 60+ boosters",
          "2) Reporting on 60+ uptake, but not boosters")))) %>%
    mutate(cov_60p_booster_cat = if_else(
      is.na(adm_fv_60p) | adm_fv_60p == 0,
      "0) Not reporting on older adult uptake",
      if_else(
        is.na(cov_60p_booster) & is.na(adm_fv_60p) == FALSE,
        "1) Not reporting on older adult boosters",
        # TODO add cut() function here
        if_else(
          cov_60p_booster > .5,
          "5) >50%",
          if_else(
            cov_60p_booster > .25,
            "4) 25-49.9%",
            if_else(
              cov_60p_booster > .1,
              "3) 10-24.9%",
              if_else(
                cov_60p_booster > 0,
                "2) 0-9.9%",
                if_else(
                  cov_60p_booster == 0,
                  "1) Not reporting on older adult boosters",
                  NA_character_)))))))) %>%
    
    mutate(cov_60p_a1d_fv = if_else(cov_60p_fv == 0 | is.na(cov_60p_fv),
                                    cov_60p_a1d,
                                    cov_60p_a1d - cov_60p_fv),
           cov_60p_fv_booster = if_else(cov_60p_booster == 0 | is.na(cov_60p_booster),
                                        cov_60p_fv,
                                        cov_60p_fv - cov_60p_booster))

  a_data$cov_hcw_fv[a_data$a_iso == "GRL"] <-
    a_data$cov_hcw_fv[a_data$a_iso == "DNK"]
  a_data$cov_hcw_fv[a_data$a_iso == "SJM"] <-
    a_data$cov_hcw_fv[a_data$a_iso == "NOR"]

  a_data$cov_60p_fv[a_data$a_iso == "GRL"] <-
    a_data$cov_60p_fv[a_data$a_iso == "DNK"]
  a_data$cov_60p_fv[a_data$a_iso == "SJM"] <-
    a_data$cov_60p_fv[a_data$a_iso == "NOR"]

  # Calculate gender coverage difference in reporting countries
  print(" >>> Computing gender coverage difference in reporting countries...")
  a_data <- a_data %>%
    mutate(cov_total_gen_diff = cov_total_fem_fv - cov_total_male_fv)
  
  # Coverage categories in target groups

  a_data$cov_hcw_fv_cat <- cut(
    a_data$cov_hcw_fv,
    breaks = c(-Inf, 0.1, 0.2, 0.4, 0.7, Inf),
    labels = c("1) 0-10%", "2) 10-20%", "3) 20-40%", "4) 40-70%", "5) 70%+"),
    include.lowest = TRUE,
    right = FALSE
  )

  a_data$cov_60p_fv_cat <- cut(
    a_data$cov_60p_fv,
    breaks = c(-Inf, 0.1, 0.2, 0.4, 0.7, Inf),
    labels = c("1) 0-10%", "2) 10-20%", "3) 20-40%", "4) 40-70%", "5) 70%+"),
    include.lowest = TRUE,
    right = FALSE
  )

  # Calculate 4-week average daily rates as % of pop.
  print(" >>> Computing 4-week average daily rates as % of pop...")
  a_data <- a_data %>%
    mutate(dvr_4wk_fv = pmax(0, dvr_4wk_fv)) %>%
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
    mutate(dvr_4wk_td_change_lm_per = if_else(
      is.infinite(dvr_4wk_td_change_lm / dvr_4wk_td_lm),
      1,
      dvr_4wk_td_change_lm / dvr_4wk_td_lm))

  breaks <- c(-Inf, -0.25, 0, 0.25, Inf)
  tags <- c("1) < (-25)%", "2) (-25)-0%", "3) 0-25%", "4) > 25%")
  a_data$dvr_4wk_td_change_lm_per_cat <- cut(
    a_data$dvr_4wk_td_change_lm_per,
    breaks = breaks,
    labels = tags,
    include.lowest = TRUE,
    right = TRUE
  )

  a_data <- a_data %>%
    mutate(dvr_4wk_td_change_lm_per_cat = replace_na(
      dvr_4wk_td_change_lm_per_cat,
      tags[2]
    ))

  # Calculate coverage difference between HCWs and total in reporting countries
  print(" >>> Computing coverage difference between HCWs and total in reporting countries...")
  a_data <- a_data %>%
    mutate(
      cov_total_hcw_diff = ifelse(
      adm_fv_hcw_repstat == "Reporting",
      cov_hcw_fv - cov_total_fv,
      NA
      ))
  
  # Calculate coverage difference between 60 plus and total in reporting countries
  print(" >>> Computing coverage difference between HCWs and total in reporting countries...")
  a_data <- a_data %>%
    mutate(
      cov_total_60p_diff = ifelse(
        adm_fv_60p_repstat == "Reporting",
        cov_60p_fv - cov_total_fv,
        NA
      ))
  
  # Categorize comparison of coverage between HCWs and total
  breaks <- c(-Inf, 0, Inf)
  tags <- c("AMC participants with complete primary series coverage of healthcare workers lesser than total", "AMC participants with complete primary series coverage ofhealthcare workers greater than total")
  a_data$cov_total_hcw_com <- cut(
    a_data$cov_total_hcw_diff,
    breaks = breaks,
    labels = tags,
    include.lowest = FALSE,
    right = TRUE
  )
  
  # Categorize comparison of coverage between 60 plus and total
  breaks <- c(-Inf, 0, Inf)
  tags <- c("AMC participants with complete primary series coverage of older adults lesser than total", "AMC participants with complete primary series coverage of older adults greater than total")
  a_data$cov_total_60p_com <- cut(
    a_data$cov_total_60p_diff,
    breaks = breaks,
    labels = tags,
    include.lowest = FALSE,
    right = TRUE
  )
  
  breaks <- c(-Inf, -0.25, 0.25, Inf)
  tags <- c("Downward", "Stable", "Upward")
  a_data$dvr_4wk_td_change_lm_trend <- cut(
    a_data$dvr_4wk_td_change_lm_per,
    breaks = breaks,
    labels = tags,
    include.lowest = FALSE,
    right = TRUE
  )

  a_data <- a_data %>%
    mutate(dvr_4wk_td_change_lm_trend = replace_na(
      dvr_4wk_td_change_lm_trend,
      tags[2]
    ))


  datalist <- list("a_data" = a_data,
    "timeto_t70" = timeto_t70)
  return(datalist)
}