# rows 1973 - 2167

run_coverage <- function(a_data, env = .GlobalEnv) {
    source("eda/coverage/coverage_target_groups.r")
    source("eda/vxrate/vxrate_consolidate.r")

    print(" > Starting local environment for eda coverage module...")

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
  
    return(environment())

}
