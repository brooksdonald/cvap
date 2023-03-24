
run_check <- function(a_data, a_data_lm) {
  print(" > Starting local environment for data quality checks...")
  source("eda/data_checks/check.r")

  
  print(" >> Checking data...")
  a_data <- merge_data_checks(a_data, a_data_lm)
  print(" > Done.")
  
  print(" >> Checking data...")
  a_data <- data_checks(a_data)
  print(" > Done.")

  print(" > Returning to local environment.")
  return(environment())
}