
run_eda_pin <- function(a_data) {
  source("eda/pin/pin.r")
  print(" > Starting local environment for pin eda module...")
  
  print(" > Analysing people in need data...")
  a_data <- pin_analyse(a_data)
  print(" > Done.")
  
  print(" > Returning to local environment.")
  return(environment())
}
