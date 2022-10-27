# Rows 1648 - 1719

run_last_month <- function() {
    source("consolidate/last_month/last_month.r")
    print(" > Starting local environment for last month base data...")
    base_data_lm <- load_last_month_data()
    print(" > Done.")

    
    base_data_lm_change <- transform_last_month_data(base_data_lm)
    
    
    
    print(" > Returning to local environment.")

    
    return(environment())
}