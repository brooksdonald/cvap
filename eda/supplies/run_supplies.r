# rows 2169 - 2254

source("src/supply/supply_secured.r")
source("src/supply/supply_received.r")
source("eda/supplies/supplies_consolidate.r")


run_eda_supplies <- function(env = .GlobalEnv) {
    print(" > Starting local environment for supplies eda")
    print(" > Supplies eda...")
    a_data <- merge_a_data_details(a_data)
    print(" > Done.")

    print(" > Loading data back to global environment...")
    env$a_data <- a_data

    return(environment())
}

run_eda_supplies()
