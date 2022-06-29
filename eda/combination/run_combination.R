run_combination <- function(a_data, env = .GlobalEnv) {
  source("eda/combination/combination.r")
  print(" > Starting local environment for supply & administation summary")
  print(" > Overlaying supply and administration data...")
  a_data <- supply_admin_summary(a_data)
  print(" > Loading supply & administration data back to global environment...") 
  print(" > Done.")
  return(environment())
}