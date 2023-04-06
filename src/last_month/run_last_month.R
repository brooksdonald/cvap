
run_last_month <- function() {
  source("src/last_month/last_month.r")
  print(" > Starting local environment for last month output data module...")
  
  print(" > Loading last month output data...")
  a_data_lm <- load_last_month()
  print(" > Done.")

  print(" > Returning to local environment.")
  return(environment())
}
