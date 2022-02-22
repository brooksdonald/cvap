

target_group_ten <- function(a_data) {
  # Progress against coverage targets
  ## 10% target
  a_data <- left_join(a_data, c_vxrate_sept_t10, by = "a_iso")

  a_data <- a_data %>%
    mutate(t10_goalmet_sep = if_else(a_iso == "BDI", "No", t10_goalmet_sep)) %>%
    
    mutate(t10_goalmet_after = if_else(cov_total_fv >= 0.1, "Yes", "No")) %>%
    
    mutate(t10_notmet = if_else(cov_total_fv < 0.1, "Yes", "No")) %>%
    
    mutate(t10_timeto = round(if_else((((
      0.1 * a_pop
    ) - adm_fv_homo) / (dvr_4wk_fv)) < 0 , 0,
    if_else(is.infinite((((0.1 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
    )), NA_real_,
    (((0.1 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
    ))))) %>%
    
    mutate(t10_status = if_else(
      t10_goalmet_sep == "Yes" ,
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
  ## 20%/40% target
  a_data <- left_join(a_data, c_vxrate_dec_t2040, by = "a_iso")

  a_data <- a_data %>%
    mutate(t20_goalmet_after = if_else(cov_total_fv >= 0.2, "Yes", "No")) %>%
    
    mutate(t20_notmet = if_else(cov_total_fv < 0.2, "Yes", "No")) %>%
    
    mutate(t20_timeto = round(if_else((((
      0.2 * a_pop
    ) - adm_fv_homo) / (dvr_4wk_fv)) < 0 , 0,
    if_else(is.infinite((((0.2 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
    )), NA_real_,
    (((0.2 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
    ))))) %>%
    
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
    
    mutate(t40_goalmet_after = if_else(cov_total_fv >= 0.4, "Yes", "No")) %>%
    
    mutate(t40_notmet = if_else(cov_total_fv < 0.4, "Yes", "No")) %>%
    
    mutate(t40_timeto = round(if_else((((
      0.4 * a_pop
    ) - adm_fv_homo) / (dvr_4wk_fv)) < 0 , 0,
    if_else(is.infinite((((0.4 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
    )), NA_real_,
    (((0.4 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
    ))))) %>%
    
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
    
    
    mutate(t40_jun_peratpace = (adm_fv_homo + (dvr_4wk_fv * (
      as.numeric(as.Date("2022-06-30") - Sys.Date())
    ))) / a_pop) %>%
    
    mutate(t40_jun_willmeet = if_else(t40_jun_peratpace >= 0.4, "Yes", "No"))

  return(a_data) 
}

target_group_seventy <- function(a_data) {
  ## 70% target
  a_data <- a_data %>%
    mutate(t70_goalmet = if_else(cov_total_fv >= 0.7, "Yes", "No")) %>%
    
    mutate(t70_willmeet = if_else(cov_total_fv_atpace_30jun >= 0.7, "Yes", "No")) %>%
    
    mutate(t70_ontrack = if_else(t70_goalmet != "Yes" &
                                  t70_willmeet == "Yes", "Yes", "No")) %>%
    
    mutate(t70_offtrack = if_else(t70_goalmet != "Yes" &
                                    t70_willmeet != "Yes", "Yes", "No")) %>%
    
    mutate(t70_timeto = round(if_else((((
      0.7 * a_pop
    ) - adm_fv_homo) / (dvr_4wk_fv)) < 0 , 0,
    if_else(is.infinite((((0.7 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
    )), NA_real_,
    (((0.7 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
    ))))) %>%
    
    mutate(t70_status = if_else(
      t70_goalmet == "Yes",
      "Goal met",
      if_else(
        t70_ontrack == "Yes",
        "On track",
        if_else(t70_offtrack == "Yes", "Off track",
                NA_character_)
      )
    )) %>%
    
    mutate(t70_status_num = if_else(
      t70_goalmet == "Yes",
      1,
      if_else(
        t70_ontrack == "Yes",
        2,
        if_else(t70_offtrack == "Yes", 3,
                NA_real_)
      )
    )) %>%
    
    mutate(t70_rate_needed = if_else(
      (((a_pop * 0.7) - adm_fv_homo) / as.numeric(as.Date("2022-06-30") - as.Date("2022-01-26"))) < 0, 0,
      (((a_pop * 0.7) - adm_fv_homo) / as.numeric(as.Date("2022-06-30") - as.Date("2022-01-26"))))) %>%
    
    mutate(t70_scaleup = round((t70_rate_needed / dvr_4wk_fv),2)) %>%
    
    mutate(t70_scaleup_cat = if_else(
      t70_scaleup == 0,
      "1) Goal met",
      if_else(
        t70_scaleup <= 2,
        "2) <2x",
        if_else(
          t70_scaleup <= 5,
          "3) 3-5x",
          if_else(
            t70_scaleup <= 10,
            "4) 5-10x",
            if_else(t70_scaleup > 10, "5) >10x",
                    NA_character_)
          )
        )
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
    ) %>%
    
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

  return(a_data) 
}






