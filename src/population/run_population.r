
source("src/population/base_population.r")
source("src/population/base_population.r")

run_population <- function(local = new.env()) {
    print(" > Starting local environment for base population") 
    print(" > base population...")
    c_pop_disag <- load_base_population()
    local$c_pop_disag <- transform_base_population(
        c_pop_disag
    )
    print(" > Done. ")
    print(" base_hcw...")
    population_hcw <- load_base_hcw()
    local$population_hcw <- transform_base_hcw(
        population_hcw
    )
    print(" Done ")

    print(" > Returning to llocal environment. ")

    print(" > Loaading data back to global environment ")
    .GlobalEnv$population_hcw <- local$population_hcw
    .GlobalEnv$c_pop_disag <- local$c_pop_disag
    rm(list = ls())
    
}