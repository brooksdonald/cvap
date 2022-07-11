target_ten <- function(a_data) {
  print(" >>> Calculating progress against 10% coverage target...")

  a_data <- left_join(a_data, c_vxrate_sept_t10, by = "a_iso")

  a_data <- a_data %>%
    mutate(t10_goalmet_sep = if_else(a_iso == "BDI", "No", t10_goalmet_sep)) %>%
    mutate(t10_goalmet_after = if_else(cov_total_fv >= 0.1, "Yes", "No")) %>%
    mutate(t10_notmet = if_else(cov_total_fv < 0.1, "Yes", "No")) %>%
    mutate(t10_timeto = round(if_else(
      ((adm_pv + ((a_pop_10 - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td) < 0, 0,
      if_else(is.infinite(((adm_pv + ((a_pop_10 - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td)), NA_real_,
              ((adm_pv + ((a_pop_10 - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td))))) %>%
    mutate(t10_rate_needed_31dec = if_else(
      (((a_pop_10 - adm_fv_homo) / timeto_t70) * 2) < 0, 0,
      (((a_pop_10 - adm_fv_homo) / timeto_t70) * 2))) %>%
    mutate(t10_scaleup_31dec = if_else(is.infinite(round((t10_rate_needed_31dec / dvr_4wk_td),2)), 0, 
                                       round((t10_rate_needed_31dec / dvr_4wk_td), 2))) %>%
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

target_twenty_forty <- function(a_data) {
  print(" >>> Calculating progress against 20% & 40% coverage targets...")
  
  a_data <- left_join(a_data, c_vxrate_dec_t2040, by = "a_iso")

  a_data <- a_data %>%
    mutate(t20_goalmet_after = if_else(cov_total_fv >= 0.2, "Yes", "No")) %>%
    mutate(t20_notmet = if_else(cov_total_fv < 0.2, "Yes", "No")) %>%
    mutate(t20_timeto = round(if_else(
      ((adm_pv + ((a_pop_20 - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td) < 0, 0,
      if_else(is.infinite(((adm_pv + ((a_pop_20 - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td)), NA_real_,
              ((adm_pv + ((a_pop_20 - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td))))) %>%
    mutate(t20_rate_needed_31dec = if_else(
      (((a_pop_20 - adm_fv_homo) / timeto_t70) * 2) < 0, 0,
      (((a_pop_20 - adm_fv_homo) / timeto_t70) * 2))) %>%
    mutate(t20_scaleup_31dec = if_else(is.infinite(round((t20_rate_needed_31dec / dvr_4wk_td), 2)), 0, 
                                       round((t20_rate_needed_31dec / dvr_4wk_td), 2))) %>%
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
    mutate(t40_rate_needed_31dec = if_else(
      (((a_pop_40 - adm_fv_homo) / timeto_t70) * 2) < 0, 0,
      (((a_pop_40 - adm_fv_homo) / timeto_t70) * 2))) %>%
    mutate(t40_scaleup_31dec = if_else(is.infinite(round((t40_rate_needed_31dec / dvr_4wk_td),2)), 0, 
                                       round((t40_rate_needed_31dec / dvr_4wk_td),2))) %>%
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
    mutate(t40_jun_willmeet = if_else(cov_total_fv_atpace_31dec >= 0.4, "Yes", "No"))
  return(a_data)
}

target_seventy <- function(a_data) {
  print(" >>> Calculating progress against 70% coverage target...")

  a_data <- left_join(a_data, c_vxrate_jun_t70, by = "a_iso")
  
  a_data <- a_data %>%
    mutate(t70_goalmet_after = if_else(cov_total_fv >= 0.7, "Yes", "No")) %>%
    mutate(t70_notmet = if_else(cov_total_fv < 0.7, "Yes", "No")) %>%
    mutate(t70_timeto = round(if_else(
      ((adm_pv + ((a_pop_70 - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td) < 0, 0,
      if_else(is.infinite(((adm_pv + ((a_pop_70 - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td)), NA_real_,
              ((adm_pv + ((a_pop_70 - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td))))) %>%
    mutate(t70_rate_needed_31dec = if_else(
      (((a_pop_70 - adm_fv_homo) / timeto_t70) * 2) < 0, 0,
      (((a_pop_70 - adm_fv_homo) / timeto_t70) * 2))) %>%
    mutate(t70_scaleup_31dec = if_else(is.infinite(round((t70_rate_needed_31dec / dvr_4wk_td),2)), 0, 
                                       round((t70_rate_needed_31dec / dvr_4wk_td), 2))) %>%
    
    mutate(t70_status = if_else(
      t70_goalmet_jun == "Yes",
      "Goal met by deadline",
      if_else(
        t70_goalmet_after == "Yes",
        "Goal met after deadline",
        if_else(t70_notmet == "Yes", "Goal not yet met",
                NA_character_)
      )
    )) %>%
    
    mutate(t70_status_vis = if_else(
      t70_goalmet_jun == "Yes",
      "1) Goal met by deadline",
      if_else(
        t70_goalmet_after == "Yes",
        "2) Goal met after deadline",
        if_else(t70_notmet == "Yes", "3) Goal not yet met",
                NA_character_)
      )
    ))
  
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