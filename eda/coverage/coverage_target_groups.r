target_group_ten <- function(a_data) {
  print(" >>> Getting 10% target progress against coverage targets...")
  # Progress against coverage targets
  ## 10% target
  a_data <- left_join(a_data, c_vxrate_sept_t10, by = "a_iso")

  a_data <- a_data %>%
    mutate(t10_goalmet_sep = if_else(a_iso == "BDI", "No", t10_goalmet_sep)) %>%
    mutate(t10_goalmet_after = if_else(cov_total_fv >= 0.1, "Yes", "No")) %>%
    mutate(t10_notmet = if_else(cov_total_fv < 0.1, "Yes", "No")) %>%
    mutate(t10_timeto = round(if_else(
      ((adm_pv + ((a_pop_10 - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td) < 0, 0,
      if_else(is.infinite(((adm_pv + ((a_pop_10 - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td)), NA_real_,
              ((adm_pv + ((a_pop_10 - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td))))) %>%
    mutate(t10_rate_needed_30jun = if_else(
      (((a_pop_10 - adm_fv_homo) / timeto_t70) * 2) < 0, 0,
      (((a_pop_10 - adm_fv_homo) / timeto_t70) * 2))) %>%
    mutate(t10_scaleup_30jun = if_else(is.infinite(round((t10_rate_needed_30jun / dvr_4wk_td),2)), 0, 
                                       round((t10_rate_needed_30jun / dvr_4wk_td), 2))) %>%
    mutate(t10_status = if_else(
      t10_goalmet_sep == "Yes",
      "1) Goal met by deadline",
      if_else(
        t10_goalmet_after == "Yes",
        "2) Goal met after deadline",
        if_else(t10_notmet == "Yes", "3) Goal not yet met",
                NA_character_)
      )
    ))

    return(a_data)

}

target_group_twenty_forty <- function(a_data) {
  ## 20% - 40% target
  a_data <- left_join(a_data, c_vxrate_dec_t2040, by = "a_iso")

  a_data <- a_data %>%
    mutate(t20_goalmet_after = if_else(cov_total_fv >= 0.2, "Yes", "No")) %>%
    mutate(t20_notmet = if_else(cov_total_fv < 0.2, "Yes", "No")) %>%
    mutate(t20_timeto = round(if_else(
      ((adm_pv + ((a_pop_20 - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td) < 0, 0,
      if_else(is.infinite(((adm_pv + ((a_pop_20 - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td)), NA_real_,
              ((adm_pv + ((a_pop_20 - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td))))) %>%
    mutate(t20_rate_needed_30jun = if_else(
      (((a_pop_20 - adm_fv_homo) / timeto_t70) * 2) < 0, 0,
      (((a_pop_20 - adm_fv_homo) / timeto_t70) * 2))) %>%
    mutate(t20_scaleup_30jun = if_else(is.infinite(round((t20_rate_needed_30jun / dvr_4wk_td), 2)), 0, 
                                       round((t20_rate_needed_30jun / dvr_4wk_td), 2))) %>%
    mutate(t20_status = if_else(
      t20_goalmet_dec == "Yes" |
        (is.na(t20_goalmet_dec) & adm_td > 0),
      "1) Goal met by deadline",
      if_else(
        t20_goalmet_after == "Yes",
        "2) Goal met after deadline",
        if_else(t20_notmet == "Yes", "3) Goal not yet met",
                NA_character_)
      )
    )) %>%
    mutate(t40_goalmet_dec = if_else((is.na(t40_goalmet_dec) & cov_total_fv < 0.4), "No", 
                                     t40_goalmet_dec)) %>%
    mutate(t40_goalmet_after = if_else(cov_total_fv >= 0.4, "Yes", "No")) %>%
    mutate(t40_notmet = if_else(cov_total_fv < 0.4, "Yes", "No")) %>%
    mutate(t40_timeto = round(if_else(
      ((adm_pv + ((a_pop_40 - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td) < 0, 0,
      if_else(is.infinite(((adm_pv + ((a_pop_40 - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td)), NA_real_,
              ((adm_pv + ((a_pop_40 - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td))))) %>%
    mutate(t40_rate_needed_30jun = if_else(
      (((a_pop_40 - adm_fv_homo) / timeto_t70) * 2) < 0, 0,
      (((a_pop_40 - adm_fv_homo) / timeto_t70) * 2))) %>%
    mutate(t40_scaleup_30jun = if_else(is.infinite(round((t40_rate_needed_30jun / dvr_4wk_td),2)), 0, 
                                       round((t40_rate_needed_30jun / dvr_4wk_td),2))) %>%
    mutate(t40_status = if_else(
      t40_goalmet_dec == "Yes" |
        (is.na(t40_goalmet_dec) & adm_td > 0),
      "1) Goal met by deadline",
      if_else(
        t40_goalmet_after == "Yes",
        "2) Goal met after deadline",
        if_else(t40_notmet == "Yes", "3) Goal not yet met",
                NA_character_)
      )
    )) %>%
    mutate(t40_jun_willmeet = if_else(cov_total_fv_atpace_30jun >= 0.4, "Yes", "No"))
  return(a_data)
}

target_group_seventy <- function(a_data) {
  ## 70% target
  a_data <- a_data %>%
  mutate(t70_goalmet = if_else(cov_total_fv >= 0.7, "Yes", NA_character_)) %>%
  mutate(t70_willmeet = if_else(cov_total_fv_atpace_30jun >= 0.7, "Yes", NA_character_))
  
  a_data <- a_data %>%
  mutate(t70_ontrack = if_else(t70_willmeet == "Yes" & is.na(t70_goalmet), "Yes", NA_character_)) %>%
  mutate(t70_offtrack = if_else(is.na(t70_goalmet) & is.na(t70_willmeet), "Yes", NA_character_)) %>%
  mutate(t70_timeto = round(if_else(
    ((adm_pv + ((a_pop_70 - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td) < 0, 0,
    if_else(is.infinite(((adm_pv + ((a_pop_70 - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td)), NA_real_,
            ((adm_pv + ((a_pop_70 - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td)))))
  a_data <- a_data %>%
  mutate(t70_status = if_else(
    cov_total_fv >= 0.7,
    "Goal met",
    if_else(
      cov_total_fv_atpace_30jun >= 0.7 & is.na(t70_goalmet),
      "On track",
      if_else(is.na(t70_goalmet) & is.na(t70_willmeet), "Off track",
      NA_character_)
    )
  )) %>%
  mutate(t70_status_num = if_else(
    cov_total_fv >= 0.7,
    1,
    if_else(
      cov_total_fv_atpace_30jun >= 0.7 & is.na(t70_goalmet),
      2,
      if_else(is.na(t70_goalmet) & is.na(t70_willmeet), 3,
      NA_real_)
    )
  )) %>%
  mutate(t70_rate_needed = if_else(
    ((a_pop_70 - adm_fv_homo) / timeto_t70) < 0, 0,
    ((a_pop_70 - adm_fv_homo) / timeto_t70))) %>%
    mutate(t70_rate_needed_dose = if_else(is.na(jj_policy),
    if_else(
    ((adm_pv + ((a_pop_70 - adm_pv - adm_fv_homo) * 2)) / timeto_t70) < 0, 0,
    ((adm_pv + ((a_pop_70 - adm_pv - adm_fv_homo) * 2)) / timeto_t70)
    ),
    if_else(jj_policy == "One dose",
    if_else(
    ((adm_pv + ((a_pop_70 - adm_pv - adm_fv_homo) * 2 * (1 - del_dose_jj_prop)) + ((a_pop_70 - adm_pv - adm_fv_homo) * del_dose_jj_prop)) / timeto_t70) < 0, 0,
    ((adm_pv + ((a_pop_70 - adm_pv - adm_fv_homo) * 2 * (1 - del_dose_jj_prop)) + ((a_pop_70 - adm_pv - adm_fv_homo) * del_dose_jj_prop)) / timeto_t70) 
    ),
    NA_real_)
    )) %>%
    
  mutate(t70_scaleup = if_else(is.infinite(round((t70_rate_needed / dvr_4wk_fv),2)), NA_real_, 
                               round((t70_rate_needed / dvr_4wk_fv), 2))) %>%
  mutate(t70_scaleup_dose = if_else(is.infinite(round((t70_rate_needed_dose / dvr_4wk_td),2)), 0, 
                                    round((t70_rate_needed_dose / dvr_4wk_td),2))) %>%
    
  mutate(t70_rate_needed_dose_per = t70_rate_needed_dose / a_pop)
  
  #FIXME Min and max values hard coded. Automate this.
  breaks <- c(-1, 0, 2, 5, 10, 1000000)
  tags <- c("1) Goal met", "2) <2x", "3) 3-5x", "4) 5-10x", "5) >10x")
  a_data$t70_scaleup_cat <- cut(
    a_data$t70_scaleup_dose,
    breaks = breaks,
    include.lowest = TRUE,
    right = TRUE,
    labels = tags
  )
  
  a_data <- a_data %>%
  mutate(t70_scaleup_cat_kpi = if_else(
    t70_scaleup_dose < 2,
    "High",
    if_else(
      t70_scaleup_dose < 10,
      "Medium",
      if_else(
        t70_scaleup_dose >= 10,
        "Low",
        NA_character_)
    )
  )
  )
  
  return(a_data)
}

booster_doses <- function(a_data) {
  # Booster and additional doses
  a_data <- a_data %>%
    mutate(cov_total_booster = adm_booster / a_pop) %>%
    mutate(
      booster_status = if_else(
        adm_booster > 0,
        "Administering booster/additional doses",
        "Not reporting on booster/additional dose administration"
      )
    )%>%
  
  #FIXME Make this DRY
  mutate(cov_total_booster_cat = if_else(
    cov_total_booster > .1,
    "4) >10%",
    if_else(
      cov_total_booster > .05,
      "3) 5-9.9%",
      if_else(
        cov_total_booster > .01,
        "2) 1-4.9%",
        if_else(
          cov_total_booster > 0,
          "1) 0-0.9%",
          if_else(
            is.na(cov_total_booster) |
              cov_total_booster == 0,
            "0) Not reporting",
            NA_character_
          )
        )
      )
    )
  ))

  # #FIXME Min and max values hard coded. Automate this.
  # breaks <- c(-1, 0, .01, .05, .1, .2)
  # tags <- c("4) >10%", "3) 5-9.9%", "2) 1-4.9%", "1) 0-0.9%", "0) Not reporting")
  # a_data$cov_total_booster_cat <- cut(
  #   a_data$cov_total_booster,
  #   breaks = breaks,
  #   right = FALSE,
  #   labels = tags
  # )
  
  # Calculate coverage differences
  a_data <- a_data %>%
    mutate(cov_total_a1d_fv = if_else(cov_total_a1d < cov_total_fv, 0, (cov_total_a1d - cov_total_fv))) %>%
    
    mutate(cov_total_fv_booster = if_else(cov_total_fv < cov_total_booster, 0, (cov_total_fv - cov_total_booster)))
  
  return(a_data)
}