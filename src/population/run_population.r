# rows 600 - 978

library("readxl")
library("writexl")
library("countrycode")
library("dplyr")
library("lubridate")
library("tidyr")
library("data.table")
library("here")


source("src/population/population_base.r")
source("src/population/population_hcw.r")
source("src/population/population_uptake.r")


run_population <- function(env = new.env()) {
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

    print(" > Returning to local environment. ")

    print(" > Loading data back to global environment...")
    env$population_data <- population_data
    env$uptake_gender_data <- uptake_gender_data

    return(environment())
}

run_population(env = .GlobalEnv)

