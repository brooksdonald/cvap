
source("eda/coverage/coverage_target_groups.r")
source("eda/vxrate/vxrate_consolidate.r")

run_coverage <- function(env = .GlobalEnv) {
    print(" > Starting local environment for eda coverage module...")

    # print(" > Starting local enviroment for coverage...")
    # a_data <- load_data()

    print(" > 10% target...")
    a_data <- target_group_ten(a_data)
    print(" > Done.")

    print(" > 20% and 40% target...")
    a_data <- target_group_twenty_forty(a_data)
    print(" > Done.")

    print(" > 70% target...")
    a_data <- target_group_seventy(a_data)
    print(" > Done.")

    print(" > booster doses...")
    a_data <- booster_doses(a_data)
    print(" > Done.")

    print(" > Loading eda coverage data back to global environment...")
    env$a_data <- a_data
    env$a_data <- a_data
    env$a_data <- a_data
    env$a_data <- a_data

    return(environment())

}

run_coverage()