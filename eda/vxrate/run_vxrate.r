# rows 1723 - 1972

run_vxrate <- function(entity_characteristics, population_data, uptake_gender_data, c_vxrate_latest, env = .GlobalEnv) {
    source("eda/vxrate/vxrate_consolidate.r")

    print(" > Starting local environment for coverage tartget groups...")

    print(" >  Extracting consolidated vxrate summary...")
    c_vxrate_latest_red <- extract_vxrate_details(c_vxrate_latest)
    print(" > Done.")

    print(" > Obtaining WHO Dashboard...")
    b_who_dashboard <- load_who_dashboard()
    print(" > Done.")

    print(" > Merging dataframes...")
    print(" > Done.")

    print(" > Calculating merged data")
    a_data <- transform_vxrate_merge(a_data)
    print(" > Done.")

    print(" > Loading consolidated vxrate data back to global environment...")
    env$c_vxrate_latest_red <- c_vxrate_latest_red
    env$a_data <- a_data
    print(" > Ok.")

    return(environment())
}