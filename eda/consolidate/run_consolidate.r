# rows 2698 - 3280

run_consolidate <- function(a_data, env = .GlobalEnv) {
    source("eda/consolidate/consolidate_base_file.r")
    source("eda/consolidate/consolidate_vxrate.r")

    print(" > Starting local environment for consolidation summary")
    
    print(" > Consolidating base file...")
    condense_list <- load_base_condense_file(a_data)
    print(" > Done.")

    print(" > Consolidating Vaccination rate...")
    vrcat_list <- vxrate(condense_list)
    print(" > Done.")

    print(" > Loading eda consolidation data back to global environment...") 
    env$c_condense <- c_condense
    env$e_vrcat_all <- e_vrcat_all
    env$e_trend_all <- e_trend_all

    return(environment())
}

run_consolidate(a_data)

# run_consolidate(a_data, c_condense_amc_exc, c_condense_africa, c_condense_amr, c_condense_emr)




