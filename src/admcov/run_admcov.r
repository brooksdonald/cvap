run_admcov <- function(entity_characteristics,
                       date_refresh,
                       dvr_data) {
  print("> Loading src/admcov module scripts...")
  source("src/admcov/admcov_current.r")
  source("src/admcov/admcov_prev.r")
  
  print("> Setting current month from date_refresh...")
  current_month <- substr(date_refresh, 1, 7)
  print("> Done.")
  
  print("> Preparing current adm / dvr data...")
  adm_dvr <- load_admdvr(dvr_data)
  print("> Done.")
  
  print("> Transforming adm / dvr data...")
  adm_dvr <- transform_admdvr(adm_dvr, date_refresh)
  print("> Done.")
  
  print("> Preparing subsets of adm / dvr data...")
  adm_dvr_latest <- subset_latest(adm_dvr)
  adm_dvr_sep21 <- subset_sep21(adm_dvr)
  adm_dvr_dec21 <- subset_dec21(adm_dvr)
  adm_dvr_jun22 <- subset_jun22(adm_dvr)
  adm_dvr_amc <- subset_amc(adm_dvr, entity_characteristics)
  adm_dvr_13jan <- subset_13jan(adm_dvr, entity_characteristics)
  print("> Done.")
  
  print("> Preparing adm / dvr timeseries data...")
  datalist <- transform_ts_adm(adm_dvr, current_month)
  adm_ts_long <- datalist$adm_ts_long
  adm_ts_wide <- datalist$adm_ts_wide
  print("> Done.")
  
  print("> Preparing histroical data points...")
  adm_dvr_lw <- load_admdvr_lw(adm_dvr)
  adm_dvr_lm <- load_admdvr_lm(adm_dvr)
  adm_dvr_2m <- load_admdvr_2m(adm_dvr)
  print("> Done.")
  
  print("> Merging current with histroical data...")
  adm_dvr_latest <- merge_with_summary(adm_dvr_latest, adm_dvr_lw, adm_dvr_lm, adm_dvr_2m)
  print("> Done.")
  
  # datalist3 <- transform_lw_data(b_vxrate_lw_sum, adm_dvr_latest)
  # b_vxrate_change_lw <- datalist3$b_vxrate_change_lw
  # adm_dvr_latest <- datalist3$adm_dvr_latest
  # print(" > Done.")
  # 
  # print(" > Last month's data")
  # b_vxrate_lm_sum <- load_lm_data(c_vxrate_lastmonth)
  # adm_dvr_latest <-
  #   merge_with_summary(adm_dvr_latest, b_vxrate_lm_sum)
  # print(" > Done.")
  # 
  # print(" > Last 2 months data")
  # b_vxrate_2m_sum <- load_l2m_data(c_vxrate_twomonth)
  # adm_dvr_latest <-
  #   merge_with_summary(adm_dvr_latest, b_vxrate_2m_sum)
  
  print(" > Done.")
  return(environment())
}