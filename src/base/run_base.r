# Rows 1648 - 1719

run_base <- function(env = .GlobalEnv) {
    source("src/base/base_smartsheet.r")

    print(" > Starting local environment for base data...")

    print(" > Base smartsheet...")
    b_smartsheet <- load_base_smartsheet()
    b_smartsheet <- transform_base_smartsheet(
        b_smartsheet
    )
    print(" > Done.")

    print(" > WHO dashboard...")
    b_who_dashboard <- load_who_dashboard()
    print(" > Done.")

    print(" > Returning to local environment. ")

    return(environment())
}