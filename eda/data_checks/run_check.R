
run_check <- function(refresh_date, a_data, a_data_lm, d_absorption, combined, 
                      d_absorption_country_new, timeseries, b_vxrate_pub, 
                      supply_received_by_product, base_one_budget_tracker,
                      base_one_budget_cds) {
  print(" > Starting local environment for data quality checks...")
  source("eda/data_checks/check.r")
  source("eda/data_checks/check_output_master.r")
  
  print(" >> Checking data...")
  a_data <- merge_data_checks(a_data, a_data_lm)
  print(" > Done.")
  
  
  print(" >> Checking data...")
  a_data <- data_checks(a_data)
  print(" > Done.")
  
  print(" >> Checking data...")
  output_master_checks <- output_master_checks(refresh_date, a_data, a_data_lm, 
                                               d_absorption, combined, 
                                               d_absorption_country_new, 
                                               timeseries, b_vxrate_pub, 
                                               supply_received_by_product, 
                                               base_one_budget_tracker,
                                               base_one_budget_cds)
  print(" > Done.")
  
  print(" >> Checking data...")
  b_vxrate_pub_check <- b_vxrate_pub_checks(b_vxrate_pub)
  print(" > Done.")
  
  
  
  
  print(" > Returning to local environment.")
  
  return(environment())
}
