# rows 1723 - 1972

source("eda/vxrate/vxrate_consolidate.r")
source("src/population/run_population.r")
source("src/population/population_base.r")
source("src/base/base_smartsheet.r")
source("src/vaccines/vaccines_daily_vxrate.r")
source("src/vaccines/vaccines_monthly_vxrate.r")

run_vxrate <- function(env = .GlobalEnv) {
    print(" > Starting local environment for coverage tartget groups...")
    print(" > Obtaining entity-characteristics details")
    ec <- load_entity_chars()
    print(" > Done.")

    print(" >  Extracting consolidated vxrate summary...")
    c_vxrate_latest_red <- extract_vxrate_details(c_vxrate_latest)
    print(" > Done.")

    print(" > Obtaining data on population...")
    population_hcw <- load_population_hcw()

    population_hcw <- transform_population_hcw(
        population_hcw
    )
    population_base <- load_base_population()
    population_data <- transform_base_population(
        population_base, population_hcw
    )
    datalist <- load_population_uptake()
    uptake_gender_data <- transform_population_uptake(
        as.data.frame(datalist[1]),
        as.data.frame(datalist[2])
    )
    print(" > Done.")

    print(" > Obtaining WHO Dashboard...")
    b_who_dashboard <- load_who_dashboard()
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
