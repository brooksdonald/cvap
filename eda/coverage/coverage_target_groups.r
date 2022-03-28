target_group_ten <- function(a_data) {
  # Progress against coverage targets
  ## 10% target
  # TODO Implement join helper function
  a_data <- left_join(a_data, c_vxrate_sept_t10, by = "a_iso")

  a_data <- a_data %>%
    mutate(t10_goalmet_sep = if_else(a_iso == "BDI", "No", t10_goalmet_sep)) %>%
    mutate(t10_goalmet_after = if_else(cov_total_fv >= 0.1, "Yes", "No")) %>%
    mutate(t10_notmet = if_else(cov_total_fv < 0.1, "Yes", "No")) %>%
    mutate(t10_timeto = round(if_else((((
      0.1 * a_pop
    ) - adm_fv_homo) / (dvr_4wk_fv)) < 0, 0,
    if_else(is.infinite((((0.1 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
    )), NA_real_,
    (((0.1 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
    ))))) %>%

    mutate(t10_rate_needed_30jun = if_else(
    (((a_pop * 0.1) - adm_fv_homo) / as.numeric(as.Date("2022-06-30") - as.Date("2022-02-02"))) < 0, 0,
    (((a_pop * 0.1) - adm_fv_homo) / as.numeric(as.Date("2022-06-30") - as.Date("2022-02-02"))))) %>%
    mutate(t10_scaleup_30jun = if_else(is.infinite(round((t10_rate_needed_30jun / dvr_4wk_fv),2)), NA_real_, 
                                     round((t10_rate_needed_30jun / dvr_4wk_fv),2))) %>%
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
    mutate(t20_timeto = round(if_else((((
      0.2 * a_pop
    ) - adm_fv_homo) / (dvr_4wk_fv)) < 0 , 0,
    if_else(is.infinite((((0.2 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
    )), NA_real_,
    (((0.2 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
    ))))) %>%

    mutate(t20_rate_needed_30jun = if_else(
    (((a_pop * 0.2) - adm_fv_homo) / as.numeric(as.Date("2022-06-30") - as.Date("2022-02-02"))) < 0, 0,
    (((a_pop * 0.2) - adm_fv_homo) / as.numeric(as.Date("2022-06-30") - as.Date("2022-02-02"))))) %>%
    mutate(t20_scaleup_30jun = if_else(is.infinite(round((t20_rate_needed_30jun / dvr_4wk_fv),2)), NA_real_, 
                                        round((t20_rate_needed_30jun / dvr_4wk_fv),2))) %>%
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

    mutate(t40_rate_needed_30jun = if_else(
    (((a_pop * 0.4) - adm_fv_homo) / as.numeric(as.Date("2022-06-30") - as.Date("2022-02-02"))) < 0, 0,
    (((a_pop * 0.4) - adm_fv_homo) / as.numeric(as.Date("2022-06-30") - as.Date("2022-02-02"))))) %>%
    mutate(t40_scaleup_30jun = if_else(is.infinite(round((t40_rate_needed_30jun / dvr_4wk_fv),2)), NA_real_, 
                                round((t40_rate_needed_30jun / dvr_4wk_fv),2))) %>%
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
  mutate(t70_timeto = round(if_else((((
    0.7 * a_pop
  ) - adm_fv_homo) / (dvr_4wk_fv)) < 0 , 0,
  if_else(is.infinite((((0.7 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
  )), NA_real_,
  (((0.7 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
  )))))
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
    (((a_pop * 0.7) - adm_fv_homo) / as.numeric(as.Date("2022-06-30") - as.Date("2022-02-02"))) < 0, 0,
    (((a_pop * 0.7) - adm_fv_homo) / as.numeric(as.Date("2022-06-30") - as.Date("2022-02-02"))))) %>%
  mutate(t70_scaleup = if_else(is.infinite(round((t70_rate_needed / dvr_4wk_fv),2)), NA_real_, 
                               round((t70_rate_needed / dvr_4wk_fv), 2)))
  #FIXME Min and max values hard coded. Automate this.
  breaks <- c(-1, 0, 2, 5, 10, 100)
  tags <- c("1) Goal met", "2) <2x", "3) 3-5x", "4) 5-10x", "5) >10x")
  a_data$t70_scaleup_cat <- cut(
    a_data$t70_scaleup,
    breaks = breaks,
    include.lowest = TRUE,
    right = FALSE,
    labels = tags
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
    ) %>%
  
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
  # breaks <- c(.1, .05, .01, 0)
  # tags <- c("4) >10%", "3) 5-9.9%", "2) 1-4.9%", "1) 0-0.9%", "0) Not reporting")
  # a_data$cov_total_booster_cat <- cut(
  #   a_data$cov_total_booster,
  #   breaks = breaks,
  #   right = FALSE,
  #   labels = tags
  # )
  return(a_data)
}