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

run_population <- function(local = new.env()) {
    print(" > Starting local environment for base population")

    print(" > Population healthcare workers...")
    population_hcw <- load_population_hcw()
    population_hcw <- transform_population_hcw(
        population_hcw
    )
    print(" > Done.")

    print(" > Base population...")
    population_base <- load_base_population()
    local$population_data <- transform_base_population(
        population_base, population_hcw
    )
    print(" > Done. ")

    print(" > Returning to local environment. ")

    print(" > Loading data back to global environment...")
    .GlobalEnv$population_data <- local$population_data
    rm(list = ls())
}

run_population()