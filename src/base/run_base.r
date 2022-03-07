# Rows 1648 - 1719


run_base <- function(env = new.env()) {
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

    print(" > Concerted support list...")
    b_csl <- load_conc_supp_list()
    print(" > Done.")

    print(" > Returning to local environment. ")

    print(" > Loading data back to global environment...")
    env$b_smartsheet <- b_smartsheet
    env$b_who_dashboard <- b_who_dashboard
    env$b_csl <- b_csl

    return(environment())

}