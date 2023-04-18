
run_sov <- function(a_data) {
  source("eda/sov/sov.r")
  print(" > Starting local environment for sov base data fixes module...")

  print(" >> Transforming data...")
  a_data <- sov_analyse(a_data)
  print(" > Done.")
  
  print(" > Returning to local environment.")
  return(environment())
}
