# Rows 1648 - 1719

run_base <- function(env = .GlobalEnv) {
    source("src/base/base_smartsheet.r")

    print(" > Starting local environment for base data...")

    datalist <- load_base_data()
    b_smartsheet <- transform_base_data(as.data.frame(datalist[1]))
    b_who_dashboard <- as.data.frame(datalist[2])
    print(" > Done.")

    print(" > Returning to local environment. ")

    print(" > Loading data back to global environment...")
    env$b_smartsheet <- b_smartsheet
    env$b_who_dashboard <- b_who_dashboard
    return(environment())
}