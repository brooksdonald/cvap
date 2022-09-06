# rows 1973 - 2167

run_cov_targets <- function(a_data, timeto_t70, c_vxrate_sept_t10, c_vxrate_dec_t2040, c_vxrate_jun_t70, t70_deadline, entity_characteristics) {
    source("eda/cov_targets/cov_targets_gen.r")
    source("eda/cov_targets/cov_targets_cs.r")

    print(" > Starting local environment for eda coverage module...")

    print(" > Defining the suffix of the variables...")
    deadline_suffix <- "_31dec"
    # the following line automatically generates the suffix from `t70_deadline`. 
    # uncomment if needed.
    # deadline_suffix <- paste0("_",tolower(strftime(t70_deadline, format = "%d%b")))
    print(" > Done.")

    print(" > 10% target...")
    a_data <- target_group_ten(a_data, timeto_t70, c_vxrate_sept_t10, deadline_suffix)
    print(" > Done.")

    print(" > 20% and 40% target...")
    a_data <- target_group_twenty_forty(a_data, timeto_t70, c_vxrate_dec_t2040, deadline_suffix)
    print(" > Done.")

    print(" > 70% target...")
    a_data <- target_group_seventy(a_data, timeto_t70, c_vxrate_jun_t70, deadline_suffix)
    print(" > Done.")

    print(" > booster doses...")
    a_data <- booster_doses(a_data)
    print(" > Done.")

    print(" > Calculating progress against country coverage targets...")
    a_data <- course_progress(a_data, entity_characteristics, refresh_date, timeto_t70)
    print(" > Done.")

    print(" > Adding additional notes...")
    a_data <- course_add_notes(a_data, refresh_date)
    print(" > Done.")
  
    return(environment())

}