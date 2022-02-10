# rows 985 - 1650


library("readxl")
library("writexl")
library("countrycode")
library("dplyr")
library("lubridate")
library("tidyr")
library("data.table")
library("here")


source("src/supply/supply_forecast.r")
source("src/supply/supply_secured.r")


run_supply <- function(local = new.env()) {
    print(" > Starting local environment for supply secured")
    print(" > Secured supply...")
    supply_secured <- extract_supply_secured()
    local$supply_secured <- transform_supply_secured(
        supply_secured,
        dataset_date = "2020-11-20"
    )
    print(" > Done.")
    print(" > Forecasted supply...")
    supply_forecast <- extract_supply_forecast()
    local$supply_forecast_total <- transform_supply_forecast(supply_forecast)
    print(" > Done.")

    print(" > Returning local environment.")

    print(" > Loading data back to global environment...")
    .GlobalEnv$supply_forecast_total <- local$supply_forecast_total
    .GlobalEnv$supply_secured <- local$supply_secured
    rm(list = ls())
}