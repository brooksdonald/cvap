# rows 985 - 1650

# TODO Check all warning messages coming from supplies module
run_supply <- function(env = .GlobalEnv) {
    print(" > Starting local environment for supply module...")

    source("src/supply/supply_forecast.r", TRUE)
    source("src/supply/supply_secured.r", TRUE)
    source("src/supply/supply_received.r", TRUE)

    print(" > Secured supply...")

    supply_secured <- extract_supply_secured()
    # FIXME hardcoded date
    supply_secured <- transform_supply_secured(
        supply_secured,
        dataset_date = "2022-01-27"
    )
    print(" > Done.")

    print(" > Forecasted supply...")
    supply_forecast <- extract_supply_forecast()
    supply_forecast_total <- transform_supply_forecast(supply_forecast)
    print(" > Done.")

    print(" > Received supply...")
    supply_received <- load_sup_rec()
    supply_received_doses <- transform_sup_rec_doses(supply_received)
    supply_received_produ <- transform_sup_rec_product(supply_received)
    delivery_courses_doses <- eda_sup_rec_courses(
        supply_received, supply_received_doses
    )
    print(" > Done.")


    print(" > Loading supply data back to global environment...")
    env$supply_forecast_total <- supply_forecast_total
    env$supply_secured <- supply_secured
    env$delivery_courses_doses <- delivery_courses_doses
    env$dsupply_received_product <- supply_received_produ
    env$supply_received_doses <- supply_received_doses
    print(" > OK.")

    return(environment())
}