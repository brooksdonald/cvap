
sov_analyse <- function(a_data) {
  print(" >> Calculating sov report variables...")
  a_data <- a_data %>%
    mutate(adm_a1d_hcw_calc = a_pop_hcw - adm_fv_hcw_adjust) %>%
    mutate(adm_a1d_60p_calc = a_pop_older - adm_fv_60p_homo) %>%
    # mutate(booster_status_adjust = ifelse(is.na(booster_status),
    #                                       "Not reporting on booster/additional dose administration",
    #                                       booster_status)) %>%
    mutate(t10_status_adj = ifelse(t10_status == "1) Goal met by deadline" | t10_status == "2) Goal met after deadline",
                                   "Goal met",
                                   ifelse(t10_status == "3) Goal not yet met" | is.na(t10_status),
                                          "Goal not met",
                                          NA))) %>%
    mutate(t40_status_adj = ifelse(t40_status == "1) Goal met by deadline" | t40_status == "2) Goal met after deadline",
                                   "Goal met",
                                   ifelse(t40_status == "3) Goal not yet met" | is.na(t40_status),
                                          "Goal not met",
                                          NA))) %>%
    mutate(t70_status_adj = ifelse((t70_status == "Goal met by deadline" | t70_status == "Goal met after deadline") & cov_total_fv >= .7,
                                   "Goal met",
                                   ifelse(t70_status == "Goal not yet met" | is.na(t70_status), 
                                          "Goal not met",
                                          NA)))
    # mutate(booster_status_adjust = ifelse(adm_booster > 0 | pol_boost == "Yes",
    #                                       "Administering booster/additional doses",
    #                                       booster_status_adjust))
  
  # a_data <- a_data %>%
  #   mutate(booster_status_adjust = ifelse(is.na(booster_status_adjust) & a_status_who == "Member State",
  #                                         "Administering booster/additional doses",
  #                                         booster_status_adjust)) %>%
  # mutate(a_income_group_ind = ifelse(a_income_group_vis == "2) LMIC",
  #                                    ifelse(a_iso == "IND" | a_iso == "IDN", "2) LMIC - India & Indonesia", "2) LMIC excl. India & Indonesia"),
  #                                    a_income_group_vis))
    
  print(" >> Function 'sov_analyse' done")
  return(a_data)  
}
