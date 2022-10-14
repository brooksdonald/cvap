# rows 600 - 978

run_cov_disag <- function(headers, refresh_api) {
    source("src/cov_disag/cov_disag.r")
    source("src/cov_disag/cov_disag_long.r")

    print(" > Starting local environment for cov_disag")

    print(" > Population uptake...")
    datalist <- load_population_uptake(headers, refresh_api)
    uptake_gender_data <- transform_population_uptake(
        as.data.frame(datalist$uptake_gender),
        as.data.frame(datalist$uptake_groups)
    )
    print(" > Done.")
    
    target_hcwold <- create_hrg_timeseries()

    print(" > Returning to global environment. ")
    return(environment())
}