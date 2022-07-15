# rows 1973 - 2167

run_coverage <- function(a_data, timeto_t70, c_vxrate_sept_t10, c_vxrate_dec_t2040, c_vxrate_jun_t70) {
    source("eda/coverage/coverage_target_groups.r")

    print(" > Starting local environment for eda coverage module...")

    print(" > Defining the suffix of the variables...")
    deadline_suffix <- "31dec"
    # TODO add automation
    print(" > Done.")

    print(" > 10% target...")
    a_data <- target_group_ten(a_data, timeto_t70, c_vxrate_sept_t10, deadline_suffix)
    print(" > Done.")

    print(" > 20% and 40% target...")
    a_data <- target_group_twenty_forty(a_data, timeto_t70, c_vxrate_dec_t2040, deadline_suffix)
    print(" > Done.")

    print(" > 70% target...")
    a_data <- target_group_seventy(a_data, timeto_t70, c_vxrate_jun_t7, deadline_suffix)
    print(" > Done.")

    print(" > booster doses...")
    a_data <- booster_doses(a_data)
    print(" > Done.")
  
    return(environment())

}