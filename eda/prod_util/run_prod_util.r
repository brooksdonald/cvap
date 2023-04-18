
run_eda_prod_util <- function(a_data, refresh_date, timeto_t70) {
  source("eda/prod_util/prod_util.r")
  print(" > Starting local environment for product utilization eda module...")
  
  print(" > Analysing product utilization data...")
  a_data <- analyse_prod_util(a_data, refresh_date)
  print(" > Done.")
  
  print(" > Returning to local environment.")
  return(environment())
}
