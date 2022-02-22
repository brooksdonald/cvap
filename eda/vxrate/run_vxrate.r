# rows 1723 - 1972

source("eda/vxrate/vxrate_consolidate.r")

run_vxrate <- function(env = .GlobalEnv) {
    print(" > Starting local environment for coverage tartget groups...")
    print(" >  Extracting consolidated vxrate summary...")
    c_vxrate_latest_red <- extract_vxrate_details()
    print(" > Done.")

    print(" > Merging dataframes")
    a_data <- merge_dataframes(entity_characteristics, c_vxrate_latest_red, population_data, uptake_gender_data, b_who_dashboard)
    print(" > Done.")

    print(" > Calculating merged data")
    a_data <- transform_vxrate_merge(a_data)
    print(" > Done.")

    print(" > Loading consolidated summary data back to global environment...")
    env$c_vxrate_latest_red <- c_vxrate_latest_red
    env$a_data <- a_data

    return(environment())


}

run_vxrate()