run_financing <- function(a_data, env = .GlobalEnv) {
  source("eda/financing/financing_overview.r")
  
  print(" > Starting local environment for financing summary")
  
  print(" > Calculating financing overview...")
  a_data <- financing_overview(a_data)
  print(" > Done.")
  
  print(" > Loading financing summary data back to global environment...") 
  env$a_data <- a_data
  print(" > Ok.")
  
  return(environment())
  
}
run_financing(a_data)