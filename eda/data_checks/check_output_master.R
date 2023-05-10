
output_master_checks <- function(refresh_date, a_data, a_data_lm, d_absorption, 
                                 combined, d_absorption_country_new, timeseries, 
                                 b_vxrate_pub, supply_received_by_product, 
                                 base_one_budget_tracker, base_one_budget_cds) {
  print(" >>> Checking output master data")
 
  output_master_checks <- data.frame(refresh_date = c(refresh_date)
  )

  print(" >>> Checking 'a_data'")
  output_master_checks$a_data_check_duplicates <- sum(duplicated(a_data$a_iso))

  print(" >>> Checking 'a_data_lm'")
  output_master_checks$a_data_lm_change_check_duplicates <- sum(duplicated(a_data_lm$a_iso))
  
  a_data$a_data_lm_check_exists <- as.integer(!a_data$a_iso %in% a_data_lm$a_iso)
  output_master_checks$a_data_lm_check_exists <- nrow(a_data[a_data$a_data_lm_check_exists == 1, ])
  
  print(" >>> Checking 'd_absorption'")
  output_master_checks$d_absorption_check_duplicates <- sum(d_absorption[c('adm_date_month_name')][duplicated(d_absorption[c('adm_date_month_name')]),])
  
  refresh_date_yearmonth <- substring(refresh_date,1,7)
  output_master_checks$d_absorption_check_exists <- if_else(refresh_date_yearmonth %in% d_absorption$adm_date_month_name,
                                                            "Okay", "Not Okay")
  
  print(" >>> Checking 'combined'")
  output_master_checks$combined_check_duplicates <- sum(adm_cov_env$combined[c('iso', 'month_name', 'type')][duplicated(adm_cov_env$combined[c('iso', 'month_name', 'type')]),])

  refresh_date_yearmonth <- substring(refresh_date,1,7)
  output_master_checks$combined_check_exists <- if_else(refresh_date_yearmonth %in% combined$month_name,
                                                            "Okay", "Not Okay")

  print(" >>> Checking 'd_absorption_country_new'")
  output_master_checks$d_absorption_country_new_check_duplicates <- sum(d_absorption_country_new[c('iso', 'month_name')][duplicated(d_absorption_country_new[c('iso', 'month_name')]),])  
  
  refresh_date_yearmonth <- substring(refresh_date,1,7)
  output_master_checks$d_absorption_country_new_check_exists <- if_else(refresh_date_yearmonth %in% d_absorption_country_new$month_name,
                                                        "Okay", "Not Okay")
  
  print(" >>> Checking 'timeseries'")
  output_master_checks$timeseries_check_duplicates <- nrow(eda_adm_cov_env$timeseries[c('iso', 'month_name')][duplicated(eda_adm_cov_env$timeseries[c('iso', 'month_name')]),])

  refresh_date_yearmonth <- substring(refresh_date,1,7)
  output_master_checks$timeseries_check_exists <- if_else(refresh_date_yearmonth %in% timeseries$month_name,
                                                                        "Okay", "Not Okay")
  
  print(" >>> Checking 'b_vxrate_pub'")
  output_master_checks$b_vxrate_pub_check_duplicates <- nrow(b_vxrate_pub[c('a_iso', 'adm_date')][duplicated(b_vxrate_pub[c('a_iso', 'adm_date')]),])

  refresh_date_yearmonth <- substring(refresh_date,1,7)
  output_master_checks$b_vxrate_pub_check_exists <- if_else(sum(str_detect(b_vxrate_pub$adm_date, refresh_date_yearmonth)) > 0,
                                                          "Okay", "Not Okay")
  
  print(" >>> Checking 'supply_received_by_product'")
  output_master_checks$supply_received_by_product_check_duplicates <- nrow(supply_received_by_product[c('a_iso', 'product')][duplicated(supply_received_by_product[c('a_iso', 'product')]),])

  print(" >>> Checking 'base_one_budget_tracker'")
  output_master_checks$base_one_budget_tracker_check_duplicates <- nrow(base_one_budget_tracker[c('a_name_long')][duplicated(base_one_budget_tracker[c('a_name_long')]),])

  print(" >>> Checking 'base_one_budget_cds'")
  output_master_checks$base_one_budget_cds_check_duplicates <- nrow(base_one_budget_cds[c('a_name_long')][duplicated(base_one_budget_cds[c('a_name_long')]),])

  print(" >> Finished 'output_master_checks' function...")
  return(output_master_checks)
}

# data <- data %>% 
#   mutate(month_name = as.Date(month_name, format = "%YYYY-%mm"))
# data$rank[order.month_name] <- 1:nrow(data)
# 
# order.scores <- order(dat$score)
# dat1 <- dat[order.scores,]
# dat1$rank <- rank(dat1$score)

# data <- data %>% 
#   mutate(month_name = as.Date(month_name, format = "%d/%m/%Y"))
