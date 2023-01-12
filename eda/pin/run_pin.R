
run_eda_pin <- function(a_data, population_pin) {
    print(" > Starting local environment for pin eda")
    source("eda/pin/pin.r")
# 
#     print(" > Calculating people in need variables...")
#     temp_population_pin <- calculate_pin(population_pin)
#     print(" > Done.")

    print(" > Consolidating people in need data...")
    a_data <- merge_pin(a_data)
    print(" > Done.")

    return(environment())
}