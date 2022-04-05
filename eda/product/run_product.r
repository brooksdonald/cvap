# rows 2258 - 2569

run_product <- function(a_data, b_smartsheet, b_csl, env = .GlobalEnv) {
    source("eda/product/product_utilization.r")

    print(" > Starting local environment for product utilization eda")
    
    print(" > dose utilization ...")
    a_data <- dose_utilization(a_data)
    print(" > Done.")

    print(" > Calculating supply secured not yet delivered, supply received not yet administered...")
    a_data <- supply_pending(a_data)
    print(" > Done.")

    print(" > Calculating proportions of courses of total and course sufficiency...")
    a_data <- course_sufficiency(a_data)
    print(" > Done.")

    print(" > Calculating progress against country coverage targets...")
    a_data <- course_progress(a_data, b_smartsheet)
    print(" > Done.")

    print(" > Adding additional notes...")
    a_data <- course_add_notes(a_data, b_csl)
    print(" > Done.")

    print(" > Filtering AMC covax status...")
    a_data_amc <- amc_covax_status(a_data)
    print(" > Done.")

    print(" > Filtering HIC income group...")
    a_data_hic <- hic_income_group(a_data)
    print(" > Done.")

    print(" > Loading eda product utilization data back to global environment...") 
    env$a_data <- a_data
    env$a_data_amc <- a_data_amc
    env$a_data_hic <- a_data_hic

    return(environment())

}
run_product(a_data, b_smartsheet, b_csl)
