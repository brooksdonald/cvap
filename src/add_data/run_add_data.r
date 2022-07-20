# Rows 1648 - 1719

run_add_data <- function(refresh_api) {
    source("src/add_data/add_data.r")

    print(" > Starting local environment for base data...")

    datalist <- load_base_data(refresh_api)
    b_smartsheet <- transform_base_smartsheet(datalist$b_smartsheet)
    b_who_dashboard <- datalist$b_who_dashboard
    print(" > Done.")

    print(" > Returning to local environment.")

    return(environment())
}