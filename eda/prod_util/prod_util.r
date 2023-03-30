
analyse_prod_util <- function(a_data, refresh_date) {
  print(" >> Calculating remaining doses, absolute and % pop...")
  a_data <- a_data %>%
    mutate(pu_del_rem = pmax(
      (if_else(is.na(del_dose_total) | del_dose_total == 0,
               NA_real_,
               ((del_dose_total * 0.9) - adm_td))),
      0)) %>%
    mutate(pu_del_rem_wast = pmax(
      (if_else(is.na(del_dose_total) | del_dose_total == 0,
              NA_real_,
              (del_dose_total - adm_td))),
      0)) %>%
    mutate(pu_del_rem_wast_lm = pmax(
      (if_else(is.na(del_dose_total_lm) | del_dose_total_lm == 0,
              NA_real_,
              (del_dose_total_lm - adm_td_lm))),
      0)) %>%

    mutate(del_dose_wast_per = del_dose_wast / a_pop) %>%
    mutate(del_dose_total_per = del_dose_total / a_pop) %>%
    mutate(pu_del_rem_per = pu_del_rem / a_pop) %>%
    mutate(pu_del_rem_wast_per = pu_del_rem_wast / a_pop) %>%
    mutate(pu_del_rem_timeto = if_else(is.infinite(pu_del_rem / dvr_4wk_td),
                                       NA_real_,
                                       pu_del_rem / dvr_4wk_td)) %>%
    mutate(pu_del_rem_timeto_date = as.Date(refresh_date + pu_del_rem_timeto))

  print(" >> Calculating percent of doses received utilized...")
  a_data <- a_data %>%
    mutate(pu_del_rem_prop = if_else(pu_del_rem > 0,
                                     (pu_del_rem / del_dose_total),
                                     if_else(is.na(pu_del_rem),
                                             NA_real_,
                                             if_else(pu_del_rem <= 0, 
                                                     0,
                                                     NA_real_)
                                             )
                                     )
           ) %>%
  mutate(pu_used_per = 1 - pu_del_rem_prop)

  print(" >> Assigning percent utilization categories...")
  breaks <- c(0, 0.25, 0.5, 0.75, 1)
  tags <- c("0) <25%", "1) 25-49%", "2) 50-74%", "3) 75-100%")
  a_data$pu_used_per_cat <- cut(a_data$pu_used_per,
                                breaks = breaks,
                                include.lowest = TRUE,
                                right = FALSE,
                                labels = tags
                                )
  
  print(" >> Calculating supply secured not yet delivered, supply received not yet administered...")
  a_data <- a_data %>%
    mutate(rem_cour_del = pmax(
      del_cour_total - del_cour_wast - adm_fv_homo -
        (((adm_a1d_homo - adm_fv_homo) + adm_booster)  * 0.5),
      0)) %>%
    mutate(rem_cour_del_wast = pmax(
      del_cour_total - adm_fv_homo -
        (((adm_a1d_homo - adm_fv_homo) + adm_booster)  * 0.5), 
      0)) %>%
    mutate(rem_cour_del_per = rem_cour_del / a_pop) %>%
    mutate(rem_cour_del_wast_per = rem_cour_del_wast / a_pop) %>%
    mutate(rem_cour_del_prop = rem_cour_del / del_cour_total)
  
  print(" >> Calculating if courses received & administered sufficient to reach targets...")
  a_data <- a_data %>%
    mutate(t20_suff_del = if_else(del_cour_total_per >= 0.2, 
                                  "Yes", 
                                  "No")
           ) %>%
    mutate(t40_suff_del = if_else(del_cour_total_per >= 0.4, 
                                  "Yes", 
                                  "No")
           ) %>%
    mutate(t70_suff_del = if_else(del_cour_total_per >= 0.7,
                                  "Yes",
                                  "No")
           )

  print(" >> Calculating absolute courses needed to reach targets...")
  a_data <- a_data %>%
    mutate(t20_cour_req = round((a_pop * 0.2) * 1.1)) %>%
    mutate(t40_cour_req = round((a_pop * 0.4) * 1.1)) %>%
    mutate(t70_cour_req = round((a_pop * 0.7) * 1.1))

  print(" >> Calculating remaining secured, received, and admnistered courses required for targets...")
  a_data <- a_data %>%
    mutate(t20_cour_need_del = round(pmax(t20_cour_req - del_cour_total, 0))) %>%
    mutate(t20_cour_need_adm = round(pmax(t20_cour_req - adm_fv_homo, 0))) %>%
    mutate(t40_cour_need_del = round(pmax(t40_cour_req - del_cour_total, 0))) %>%
    mutate(t40_cour_need_adm = round(pmax(t40_cour_req - adm_fv_homo, 0))) %>%
    mutate(t70_cour_need_del = round(pmax(t70_cour_req - del_cour_total, 0))) %>%
    mutate(t70_cour_need_adm = round(pmax(t70_cour_req - adm_fv_homo, 0)))

  print(" >> Function 'analyse_prod_util' done")
  return(a_data)
}
