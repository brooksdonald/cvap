# rows 2258 - 2569

run_prod_util <- function(a_data, refresh_date, timeto_t70) {
    source("eda/prod_util/product_utilization.r")

    print(" > Starting local environment for product utilization eda")
    
    print(" > dose utilization ...")
    a_data <- dose_utilization(a_data, refresh_date)
    print(" > Done.")

    print(" > Calculating supply secured not yet delivered, supply received not yet administered...")
    a_data <- supply_pending(a_data)
    print(" > Done.")

    print(" > Calculating proportions of courses of total and course sufficiency...")
    a_data <- course_sufficiency(a_data, refresh_date)
    print(" > Done.")

    return(environment())
}