
run_last_month <- function(a_data, a_data_lm) {
  source("eda/last_month/last_month.r")
  print(" > Starting local environment for last month output data eda module...")
  
  print(" > Transforming last month output data...")
  a_data_lm_change <- transform_last_month(a_data, a_data_lm)
  print(" > Done.")
  
  print(" > Returning to local environment.")
  return(environment())
}
