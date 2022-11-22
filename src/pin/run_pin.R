
run_pin <- function(refresh_api) {
    print(" > Starting local environment for people in need data...")
    source("src/pin/pin.r")

    print(" > Loading people in need data...")
    population_pin <- load_pin_data()
    print(" > Done.")

    print(" > Returning to local environment.")

    return(environment())
}