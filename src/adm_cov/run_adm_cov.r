run_adm_cov <- function(entity_characteristics, date_refresh, dvr_data, 
                        sup_ts_wide) {
  
  print("> Loading src/adm_cov module scripts...")  
  source("src/adm_cov/dvr_current.r")

  print("> Setting current month in %Y-%m format...")  
  current_month <- substr(date_refresh, 1, 7)
  
  print("> Transformating current administrative and coverage data...")
  adm_tot <- load_adm_tot(dvr_data)
  adm_tot <- trans_adm_tot(adm_tot, entity_characteristics, date_refresh)
  adm_tot_ts_daily <- create_adm_tot_ts_daily(adm_tot)
  adm_tot_13jan <- create_adm_tot_13jan(adm_tot, entity_characteristics)
  adm_tot_sep21 <- create_adm_tot_sep21(adm_tot)
  adm_tot_dec21 <- create_adm_tot_dec21(adm_tot)
  adm_tot_jun22 <- create_adm_tot_jun22(adm_tot)
  
  adm_tot_ts_mon <- create_adm_tot_td_mon(adm_tot, current_month)
  d_absorption <- absorption_sum_by_month(adm_tot_ts_mon, current_month)
  c_vxrate_latest <- latest_sum_table(adm_tot, adm_tot_13jan)
  # c_vxrate_latest <- merge_with_summary(c_vxrate_latest, adm_tot_13jan)
  combined_three <- second_supplies(adm_tot_ts_mon, sup_ts_wide)
    
  # datalist1 <- absorption_per_country(adm_tot_ts_mon, current_month)
  # d_absorption_country_new <- new_absorption_countries(adm_tot_ts_mon, current_month)
  # combined <- datalist2$combined
  
  print("> Done.")
  return(environment())
}
