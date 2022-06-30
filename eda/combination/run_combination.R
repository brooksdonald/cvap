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

  return(environment())
}