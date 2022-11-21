
run_last_month <- function() {
    print(" > Starting local environment for last month base data...")
    source("src/last_month/last_month.r")

    print(" > Loading last month base data...")
    base_data_lm <- load_last_month_data()
    print(" > Done.")

    print(" > Returning to local environment.")
    return(environment())
}