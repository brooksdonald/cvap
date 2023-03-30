
course_progress <- function(a_data, entity_characteristics, refresh_date, timeto_t70) {
  print(" >> Homogenizing country coverage targets...")
  a_data <- a_data %>%
    mutate(dp_deadline = as.Date(dp_deadline)) %>%
    mutate(ss_deadline = as.Date(ss_deadline))

  a_data <- a_data %>%
    mutate(ndvp_target = if_else(is.na(dp_target) == FALSE,
                                 dp_target,
                                 ss_target)) %>%
    mutate(ndvp_target_source = if_else(is.na(dp_target) == FALSE,
                                        "DP",
                                        "SS")) %>%
    mutate(ndvp_deadline = if_else(ndvp_target_source == "DP",
                                   dp_deadline,
                                   ss_deadline))

  a_data <- a_data %>%
  mutate(ndvp_deadline = as.Date(ndvp_deadline)) %>%
  mutate(timeto_ndvp = as.numeric(ndvp_deadline - refresh_date)) %>%
  mutate(a_pop_ndvp = a_pop * ndvp_target) %>%
  mutate(a_pop_ndvp_mid = a_pop * ndvp_mid_target)

  print(" >> Calculating progress against country coverage targets...")
  a_data <- a_data %>%
  mutate(ndvp_goalmet = if_else(
      cov_total_fv >= ndvp_target, 
      "Yes", 
      "No")) %>%
  mutate(ndvp_target_active = if_else(
      as.Date(ndvp_deadline) < refresh_date,
      NA_real_,
      ndvp_target)) %>%
  mutate(ndvp_rem =
      pmax(ndvp_target - cov_total_fv, 0)) %>%
  mutate(ndvp_peratpace = 
      ((adm_fv_homo + (dvr_4wk_fv * 
          as.numeric(as.Date(ndvp_deadline) - refresh_date)
      )) / a_pop)) %>%
  mutate(ndvp_pertogo = 
      pmax(ndvp_target - ndvp_peratpace, 0)) %>%
  mutate(ndvp_willmeet = if_else(
      ndvp_peratpace >= ndvp_target,
      "Yes",
      "No")) %>%
  mutate(ndvp_timeto = round(if_else(is.infinite(
      ((adm_pv + ((a_pop_ndvp - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td)),
      NA_real_,
      pmax((adm_pv + ((a_pop_ndvp - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td,
          0)))) %>%
  mutate(ndvp_ontrack = if_else(
      ndvp_peratpace >= ndvp_target & cov_total_fv <= ndvp_target,
      "Yes",
      "No")) %>%
  mutate(ndvp_offtrack = if_else(
      ndvp_goalmet != "Yes" & ndvp_willmeet != "Yes",
          "Yes",
          "No")) %>%
  mutate(ndvp_status = if_else(
      is.na(ndvp_target) | ndvp_target == 0,
      "Not captured",
      if_else(
          ndvp_goalmet == "Yes",
          "Goal met",
          if_else(
              is.na(ndvp_deadline),
              "No timeline",
              if_else(
                  ndvp_deadline < refresh_date,
                  "Deadline past",
                  if_else(
                      ndvp_ontrack == "Yes",
                      "On track",
                      if_else(ndvp_offtrack == "Yes", "Off track",
                              NA_character_)
                  )
              )
          )
      )
  )) %>%
  mutate(ndvp_status_num = if_else(
      is.na(ndvp_target) | ndvp_target == 0,
      1,
      if_else(
          ndvp_goalmet == "Yes",
          2,
          if_else(
              is.na(ndvp_deadline),
              3,
              if_else(
                  ndvp_deadline < refresh_date,
                  4,
                  if_else(
                      ndvp_ontrack == "Yes",
                      5,
                      if_else(ndvp_offtrack == "Yes", 6,
                              NA_real_)
                  )
              )
          )
      )
  )) %>%
  mutate(ndvp_rate_needed =
      pmax((((a_pop * ndvp_target) - adm_fv_homo) /
          as.numeric(ndvp_deadline - refresh_date)),
      0)
  ) %>%
  mutate(ndvp_rate_needed_dose =
      pmax(((adm_pv + (
          pmax(a_pop_ndvp - adm_pv - adm_fv_homo,
              0) *
          (2 - if_else(
              is.na(jj_policy),
              0,
              if_else(
                  jj_policy == "One dose",
                  del_dose_jj_prop,
                  NA_real_)))))
              / timeto_ndvp),
          0)
      ) %>%
  mutate(ndvp_scaleup = if_else(
      is.infinite(round((ndvp_rate_needed / dvr_4wk_fv), 2)),
      NA_real_,
      round(ndvp_rate_needed / dvr_4wk_fv), 2)) %>%
  mutate(ndvp_scaleup_dose = if_else(
      is.infinite(round((ndvp_rate_needed_dose / dvr_4wk_td), 2)),
      NA_real_,
      round(ndvp_rate_needed_dose / dvr_4wk_td), 2)) #%>%

  a_data$ndvp_scaleup_cat <- as.character(cut(
      a_data$ndvp_scaleup_dose,
      breaks = c(-Inf, 2, 5, 10, Inf),
      labels = c("3) <2x", "4) 3-5x", "5) 5-10x", "6) >10x"),
      include.lowest = TRUE,
      right = TRUE
  ))

  a_data$ndvp_scaleup_cat[a_data$ndvp_target == 0 | is.na(a_data$ndvp_target)] <- "1) Not captured"
  a_data$ndvp_scaleup_cat[a_data$ndvp_status == "Goal met"] <- "2) Goal met"

  print(" >> Calculating progress against mid year targets...")
  a_data <- a_data %>%
  mutate(ndvp_mid_rem = pmax(
      ndvp_mid_target - cov_total_fv,
      0)) %>%
  mutate(ndvp_mid_peratpace =
      ((adm_fv_homo + (dvr_4wk_fv * timeto_t70)) / a_pop)) %>%
  mutate(ndvp_mid_pertogo = pmax(
      ndvp_mid_target - ndvp_mid_peratpace,
      0))  %>%
  mutate(ndvp_mid_rate_needed =
      pmax(((a_pop * ndvp_mid_target) - adm_fv_homo) / timeto_t70,
      0)) %>%
  mutate(ndvp_mid_rate_needed_dose =
      pmax(((adm_pv + (
          pmax(a_pop_ndvp_mid - adm_pv - adm_fv_homo,
              0) *
          (2 - if_else(
              is.na(jj_policy),
              0,
              if_else(
                  jj_policy == "One dose",
                  del_dose_jj_prop,
                  NA_real_)))))
              / timeto_t70),
          0)) %>%
  mutate(ndvp_mid_scaleup = if_else(
      is.infinite(round((ndvp_mid_rate_needed / dvr_4wk_fv), 2)),
      NA_real_,
      round(ndvp_mid_rate_needed / dvr_4wk_fv), 2)) %>%
  mutate(ndvp_mid_scaleup_dose = if_else(
      is.infinite(round((ndvp_mid_rate_needed_dose / dvr_4wk_td), 2)),
      NA_real_,
      round(ndvp_mid_rate_needed_dose / dvr_4wk_td), 2)) %>%
  mutate(ndvp_mid_rate_needed_homo = if_else(
      is.na(ndvp_mid_rep_rate), 
      ndvp_mid_rate_needed_dose, ndvp_mid_rep_rate)) %>%
  mutate(ndvp_scaleup_dose_homo = if_else(
      is.infinite(round((ndvp_mid_rate_needed_homo / dvr_4wk_td), 2)), 
      NA_real_,
      round(ndvp_mid_rate_needed_homo / dvr_4wk_td), 2)) %>%
    mutate(ndvp_goalmet_calc = if_else(
      ndvp_goalmet == "Yes",
      "Yes",
      "No"))
    
  a_data$ndvp_scaleup_cat_kpi <- cut(
      a_data$ndvp_mid_scaleup_dose,
      breaks = c(-Inf, 2, 10, Inf),
      labels = c("High", "Medium", "Low"),
      include.lowest = TRUE,
      right = FALSE
  )
  
  print(" >> Function 'course_progress' done")
  return(a_data)
}

course_add_notes <- function(a_data, refresh_date) {
  print(" >> Adding course notes...")
  a_data <- a_data %>%
  mutate(note_highcov = if_else(
    cov_total_fv > 0.5,
    "High fully vaccinated coverage",
    "No")) %>%
  mutate(note_recent_rollout = if_else(
    intro_date > (refresh_date - 60),
    "Recent rollout", 
    "No")) %>%
  mutate(note_reporting_date = if_else(
    adm_date < (refresh_date - 10),
    "Likely reporting issue",
    NA_character_)) %>%
  mutate(note_drivers_auto = if_else(
    (note_reporting_date == "Likely reporting issue") | (note_nochange == 1) | 
      (dvr_4wk_td_per < 0), "Likely reporting issue",
    if_else(
      note_highcov == "High fully vaccinated coverage",
      "High fully vaccinated coverage",
      if_else(
        note_recent_rollout == "Recent rollout",
        "Recent rollout",
        NA_character_
        )
      )
    )
    ) %>%
  mutate(note_supplyconstraint = if_else(
      rem_cour_del_per < 0.05 & pu_used_per > 0.8,
      1,
      0)) %>%
  mutate(note_supplyneed = if_else(
      rem_cour_del_per < 0.05 & pu_used_per > 0.8,
      "Yes",
      "No"))

  print(" >> Function 'course_add_notes' done")
  return(a_data)
}