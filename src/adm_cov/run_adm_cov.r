
run_adm_cov <- function(entity_characteristics, refresh_date, dvr_data, 
                        adm_api, auto_cleaning, refresh_supply_timeseries) {
  source("src/adm_cov/dvr_current.r")
  source("src/adm_cov/dvr_prev.r")
  source("src/adm_cov/supply_timeseries.r")
  source("src/adm_cov/finance_timeseries.r")
  print(" > Starting local environment for vaccinations...")
  
  path_to_timeseries_data <- "data/input/test/"
  
  print(" > Extracting '%Y-%m' format from current month...")
  current_month <- substr(refresh_date, 1, 7)
  
  print(" > Loading current daily vaccination rate data...")
  b_vxrate <- load_dvr_current(dvr_data, adm_api, auto_cleaning)
  print(" > Done.")
  
  print(" > Transforming current daily vaccination rate data...")
  b_vxrate <- transform_dvr_current(
    b_vxrate,
    entity_characteristics,
    refresh_date)
  print(" > Done.")
  
  print(" > Transforming current daily vaccination rate data...")
  b_vxrate_pub <- transform_dvr_current_pub(b_vxrate, auto_cleaning)
  b_vxrate_amc <- transform_dvr_current_amc(b_vxrate)
  print(" > Done.")
  
  c_vxrate_sept_t10 <- transform_pop_tgt_sep_t10(b_vxrate)
  c_vxrate_dec_t2040 <- transform_pop_tgt_dec_t2040(b_vxrate)
  c_vxrate_jun_t70 <- transform_pop_tgt_jun_t70(b_vxrate)
  c_vxrate_eom <- transform_absorption_by_month(b_vxrate, current_month)
  d_absorption <- absorption_sum_by_month(c_vxrate_eom, current_month)
  
  c_vxrate_latest <- latest_sum_table(b_vxrate, c_vxrate_latest)
  c_vxrate_lastweek <- last_week_sum_table(b_vxrate, c_vxrate_latest, c_vxrate_lastweek)
  c_vxrate_lastmonth <- last_month_sum_table(b_vxrate, c_vxrate_latest, c_vxrate_lastmonth)
  c_vxrate_twomonth <- two_month_sum_table(b_vxrate, c_vxrate_latest, c_vxrate_twomonth)
  
  datalist_absorption_country <- transform_absorption_by_country(c_vxrate_eom,current_month)
  d_absorb_red <- datalist_absorption_country$d_absorb_red
  d_absorption_country_new <- new_absorption_countries(
    c_vxrate_eom,
    current_month
  )
  
  if (refresh_supply_timeseries) {
    print(" > Supply timeseries")
    sec_overall_long <- load_secured_expected(
      path_to_timeseries_data
    )
    overall_cumul_long <- load_supply_received(
      path_to_timeseries_data
    )
    overall_long <- transform_supply_received(
      overall_cumul_long
    )
    admin_red <- load_administration(
      d_absorption_country_new,
      entity_characteristics
    )
    export_supply_xlsx(
      sec_overall_long,
      overall_long,
      overall_cumul_long,
      admin_red
    )
    print(" > Done. Exported to data/input/interim/supply.xlsx")
  } else {
    print(" > Importing supply timeseries from data/input/interim/supply.xlsx")
    overall_cumul_long <- load_supply_cum_xlsx()
    overall_long <- load_supply_month_xlsx()
    print(" > Done.")
  }
  
  datalist_first_supplies <- first_supplies(
    d_absorb_red,
    datalist_absorption_country$d_absorption_country,
    overall_long,
    overall_cumul_long
  )
  combined <- datalist_first_supplies$combined
  combined_three <- second_supplies(
    d_absorption_country_new,
    combined,
    d_absorb_red,
    entity_characteristics,
    datalist_first_supplies$b_supply_red,
    overall_cumul_long
  )
  print(" > Done.")
  
  print(" > Importing finance timeseries data...")
  overall_fin_cumul_long <- import_finance_data()
  # overall_fin_long <- transform_fin_data(overall_fin_cumul_long) # Uncomment to include net per month
  print(" > Done.")
  
  print(" > Loading vaccination data (last week)...")
  b_vxrate_lw_sum <- load_lw_data(c_vxrate_lastweek)
  print(" > Done.")
  
  print(" > Transforming vaccination data (last week)...")
  datalist3 <- transform_lw_data(b_vxrate_lw_sum, c_vxrate_latest)
  b_vxrate_change_lw <- datalist3$b_vxrate_change_lw
  c_vxrate_latest <- datalist3$c_vxrate_latest
  print(" > Done.")
  
  print(" > Loading vaccination data (last month)...")
  b_vxrate_lm_sum <- load_lm_data(c_vxrate_lastmonth)
  print(" > Done.")
  
  print(" > Loading vaccination data (last month)...")
  c_vxrate_latest <- merge_with_summary(c_vxrate_latest, b_vxrate_lm_sum)
  print(" > Done.")
  
  print(" > Loading vaccination data (last month)...")
  b_vxrate_2m_sum <- load_l2m_data(c_vxrate_twomonth)
  print(" > Done.")
  
  print(" > Transforming vaccination data (last month)...")
  c_vxrate_latest <- merge_with_summary(c_vxrate_latest, b_vxrate_2m_sum)
  print(" > Done.")
  
  print(" > Loading vaccination data (week of 13 January)...")
  b_vxrate_13jan <- recreate_df(b_vxrate)
  print(" > Done.")
  
  print(" > Loading vaccination data (week of 13 January)...")
  c_vxrate_latest <- merge_with_summary(c_vxrate_latest, b_vxrate_13jan)
  print(" > Done.")
  
  print(" > Returning to global environment. ")
  return(environment())
}
