run_combination <- function(a_data, env = .GlobalEnv) {
  source("eda/combination/combination.r")
  print(" > Starting local environment for supply & administation summary")
  print(" > Overlaying supply and administration data...")
  datalist <- supply_admin_summary(a_data)
  a_data <- datalist$a_data
  z_temp <- datalist$z_temp
  z_temp_lm <- datalist$z_temp_lm
  z_secview_long <- datalist$z_secview_long
  print(" > Done.")
  print(" > Loading supply & administration data back to global environment...") 
  env$a_data <- a_data
  env$z_temp <- z_temp
  env$z_temp_lm <- z_temp_lm
  env$z_secview_long <- z_secview_long
  print(" > Ok.")
  return(environment())
}