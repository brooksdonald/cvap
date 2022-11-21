
run_cov_disag <- function(headers, refresh_api) {
    source("src/cov_disag/cov_disag.r")
    source("src/cov_disag/cov_disag_long.r")
    
    print(" > Loading coverage disaggregation data...")
    population_uptake <- load_population_uptake(headers, refresh_api)
    print(" > Done.")
    
    print(" > Transforming coverage disaggregation data...")
    uptake_gender_data <- transform_population_uptake(
      as.data.frame(population_uptake$uptake_gender),
      as.data.frame(population_uptake$uptake_group)
      )
    print(" > Done.")
    
    print(" > Loading coverage disaggregation timeseries data...")
    target_hcwold <- create_hrg_timeseries()
    print(" > Done.")
    
    print(" > Returning to global environment.")
    return(environment())
}