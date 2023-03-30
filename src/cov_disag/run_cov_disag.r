
run_cov_disag <- function(headers, refresh_api) {
  source("src/cov_disag/cov_disag.r")
  source("src/cov_disag/cov_disag_long.r")
  print(" > Starting local environment for cov_disag...")

  print(" > Loading population uptake...")
  datalist_temp <- load_population_uptake(headers, refresh_api)
  print(" > Done.")
  
  print(" > Transforming population uptake...")
  uptake_gender_data <- transform_population_uptake(
    as.data.frame(datalist_temp$uptake_gender),
    as.data.frame(datalist_temp$uptake_groups)
  )
  print(" > Done.")
  
  print(" > Creating timeseries data...")
  target_hcwold <- create_hrg_timeseries(headers, refresh_api)
  print(" > Done.")
  
  print(" > Returning to global environment. ")
  return(environment())
}