
target_group_ten <- function(a_data, timeto_t70, c_vxrate_sept_t10, deadline_suffix) {
  print(" >> Calculating coverage progress against 10% coverage target...")
  a_data <- left_join(a_data, c_vxrate_sept_t10, by = "a_iso")
  
  a_data <- a_data %>%
    mutate(t10_goalmet_sep = if_else(a_iso == "BDI", "No", t10_goalmet_sep)) %>%
    mutate(t10_goalmet_after = if_else(cov_total_fv >= 0.1, "Yes", "No")) %>%
    mutate(t10_notmet = if_else(cov_total_fv < 0.1, "Yes", "No")) %>%
    helper_goal_target_groups(10, timeto_t70, deadline_suffix)
  
  print(" >> Function 'target_group_ten' done")
  return(a_data)
}


target_group_twenty_forty <- function(a_data, timeto_t70, c_vxrate_dec_t2040, deadline_suffix) {
  print(" >> Calculating coverage progress against 20% and 40% coverage targets...")
  a_data <- left_join(a_data, c_vxrate_dec_t2040, by = "a_iso")
  
  a_data <- a_data %>%
    mutate(t20_goalmet_after = if_else(cov_total_fv >= 0.2, "Yes", "No")) %>%
    mutate(t20_notmet = if_else(cov_total_fv < 0.2, "Yes", "No")) %>%
    helper_goal_target_groups(20, timeto_t70, deadline_suffix)
  
  a_data <- a_data %>%
    mutate(t40_goalmet_dec = if_else(
      is.na(t40_goalmet_dec) & cov_total_fv < 0.4,
      "No", 
      t40_goalmet_dec)) %>%
    mutate(t40_goalmet_after = if_else(cov_total_fv >= 0.4, "Yes", "No")) %>%
    mutate(t40_notmet = if_else(cov_total_fv < 0.4, "Yes", "No")) %>%
    ## If not, at least automate the date
    helper_goal_target_groups(40, timeto_t70, deadline_suffix) %>%
    mutate(t40_jun_willmeet = if_else(
      !!as.name(paste0("cov_total_fv_atpace", deadline_suffix)) >= 0.4,
      "Yes",
      "No"))
  
  print(" >> Function 'target_group_twenty_forty' done")
  return(a_data)
}


target_group_seventy <- function(a_data, timeto_t70, c_vxrate_jun_t70, deadline_suffix) {
  print(" >> Calculating coverage progress against 70% coverage target...")
  a_data <- left_join(a_data, c_vxrate_jun_t70, by = "a_iso")
  
  a_data <- a_data %>%
    mutate(t70_goalmet_jun = if_else(a_iso == "SDN", "No", 
                                     if_else(a_iso == "KIR", "No",
                                             if_else(a_iso == "TUV", "No",
                                                     if_else(a_iso == "UKR", "No",
                                                             if_else(a_iso == "BIH", "No",
                                                                     if_else(a_iso == "SMR", "No",
                                                             t70_goalmet_jun))))))) %>%
    mutate(t70_goalmet_after = if_else(cov_total_fv >= 0.7,"Yes","No")) %>%
    mutate(t70_notmet = if_else(cov_total_fv < 0.7,"Yes","No")) %>%
    helper_goal_target_groups(70, timeto_t70, deadline_suffix)
  
  print(" >> Function 'target_group_seventy' done")
  return(a_data)
}


booster_doses <- function(a_data) {
  print(" >> Calculating booster and additional doses progress...")
  a_data <- a_data %>%
    mutate(cov_total_booster = adm_booster / a_pop) %>%
    mutate(
      booster_status = if_else(
        adm_booster > 0,
        "Administering booster/additional doses",
        "Not reporting on booster/additional dose administration"
      )
    )
  
  breaks <- c(-Inf, 0, .05, .1, Inf)
  tags <- c("0) Not reporting", "1) 0-4.9%", 
            "2) 5-9.9%", "3) >10%")
  a_data$cov_total_booster_cat <- cut(
    a_data$cov_total_booster,
    breaks = breaks,
    right = TRUE,
    labels = tags,
    include.lowest = TRUE
  )
  
  a_data %>%
    mutate(cov_total_booster_cat =
             case_when(cov_total_booster < 0 ~ NA_character_
             )
    )
  
  a_data <- a_data %>%
    mutate(cov_total_a1d_fv = if_else(
      cov_total_a1d < cov_total_fv,
      0,
      cov_total_a1d - cov_total_fv)) %>%
    mutate(cov_total_fv_booster = if_else(
      cov_total_fv < cov_total_booster,
      0,
      cov_total_fv - cov_total_booster))
  
  print(" >> Function 'booster_doses' done")
  return(a_data)
}
