# rows 600 - 978

run_population <- function(env = .GlobalEnv) {
    source("src/population/population_base.r")
    source("src/population/population_hcw.r")
    source("src/population/population_uptake.r")

    print(" > Starting local environment for base population")

    print(" > Population healthcare workers...")
    population_hcw <- load_population_hcw()
    population_hcw <- transform_population_hcw(
        population_hcw
    )
    print(" > Done.")

    print(" > Base population...")
    population_base <- load_base_population()
    population_data <- transform_base_population(
        population_base, population_hcw
    )
    print(" > Done.")

    print(" > Population uptake...")
    datalist <- load_population_uptake()
    uptake_gender_data <- transform_population_uptake(
        as.data.frame(datalist[1]),
        as.data.frame(datalist[2])
    )
    print(" > Done.")

    print(" > Returning to global environment. ")
    return(environment())
}