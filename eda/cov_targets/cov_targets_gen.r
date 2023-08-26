target_group_ten <- function(a_data, c_vxrate_sept_t10) {
  print(" >>> Getting 10% target progress against coverage targets...")
  # Progress against coverage targets
  ## 10% target
  a_data <- left_join(a_data, c_vxrate_sept_t10, by = "a_iso")

  a_data <- a_data %>%
    mutate(t10_goalmet_sep = if_else(a_iso == "BDI", "No", t10_goalmet_sep)) %>%
    mutate(t10_goalmet_after = if_else(cov_total_fv >= 0.1, "Yes", "No")) %>%
    mutate(t10_notmet = if_else(cov_total_fv < 0.1, "Yes", "No")) %>%
    helper_goal_target_groups(10)
  return(a_data)

}

target_group_twenty_forty <- function(a_data, c_vxrate_dec_t2040) {
  ## 20% - 40% target
  a_data <- left_join(a_data, c_vxrate_dec_t2040, by = "a_iso")

  a_data <- a_data %>%
    mutate(t20_goalmet_after = if_else(cov_total_fv >= 0.2, "Yes", "No")) %>%
    mutate(t20_notmet = if_else(cov_total_fv < 0.2, "Yes", "No")) %>%
    helper_goal_target_groups(20)

  a_data <- a_data %>%
    mutate(t40_goalmet_dec = if_else(
      is.na(t40_goalmet_dec) & cov_total_fv < 0.4,
      "No", 
      t40_goalmet_dec)) %>%
    mutate(t40_goalmet_after = if_else(cov_total_fv >= 0.4, "Yes", "No")) %>%
    mutate(t40_notmet = if_else(cov_total_fv < 0.4, "Yes", "No")) %>%
    ## If not, at least automate the date
    helper_goal_target_groups(40)
    # mutate(t40_jun_willmeet = if_else(
    #   !!as.name(paste0("cov_total_fv_atpace", deadline_suffix)) >= 0.4,
    #   "Yes",
    #   "No"))
  return(a_data)
}

target_group_seventy <- function(a_data, c_vxrate_jun_t70) {
  print(" >>> Calculating progress against 70% coverage target...")

  a_data <- left_join(a_data, c_vxrate_jun_t70, by = "a_iso")
  
  a_data <- a_data %>%
    mutate(t70_goalmet_jun = if_else(a_iso == "SDN", "No", 
                                     if_else(a_iso == "KIR", "No",
                                             if_else(a_iso == "TUV", "No",
                                                     if_else(a_iso == "UKR", "No",
                                     t70_goalmet_jun))))) %>%
    mutate(t70_goalmet_after = if_else(cov_total_fv >= 0.7,"Yes","No")) %>%
    mutate(t70_notmet = if_else(cov_total_fv < 0.7,"Yes","No")) %>%
    helper_goal_target_groups(70)

  return(a_data)
}

booster_doses <- function(a_data) {
  # Booster and additional doses
  a_data <- a_data %>%
    mutate(cov_total_booster = adm_tot_boost / a_pop,
           adm_status_boost = if_else(adm_tot_boost > 0 | pol_boost == "Yes",
                                    "Yes", 
                                    if_else(is.na(adm_tot_boost) & pol_boost == "No",
                                            "No", "No")))

  breaks <- c(-Inf, 0, .01, .05, .1, Inf)
  tags <- c("0) Not reporting", "1) 0-0.9%",
    "2) 1-4.9%", "3) 5-9.9%", "4) >10%")
  a_data$cov_total_booster_cat <- cut(
    a_data$cov_total_booster,
    breaks = breaks,
    right = TRUE,
    labels = tags,
    include.lowest = TRUE
  )

  a_data %>%
    mutate(cov_total_booster_cat =
      case_when(
        cov_total_booster < 0 ~ NA_character_
    ))

  # Calculate coverage differences
  a_data <- a_data %>%
    mutate(cov_total_a1d_fv = if_else(
      cov_total_a1d < cov_total_fv,
      0,
      cov_total_a1d - cov_total_fv)) %>%
    mutate(cov_total_fv_booster = if_else(
      cov_total_fv < cov_total_booster,
      0,
      cov_total_fv - cov_total_booster))

  return(a_data)
}