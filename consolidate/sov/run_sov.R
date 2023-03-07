
run_sov <- function(a_data) {
  print(" > Starting local environment for sov base data fixes...")
  source("consolidate/sov/sov.r")
  
  print(" >> Transforming data...")
  a_data <- sov(a_data)
  print(" > Done.")
  
  print(" > Returning to local environment.")
  return(environment())
}