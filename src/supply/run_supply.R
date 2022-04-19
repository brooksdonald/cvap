run_supply <- function(env = .GlobalEnv) {
    print(" > Starting local environment for supply module...")

    source("src/supply/supply_secured.r", TRUE)
    source("src/supply/supply_received.r", TRUE)

    print(" > Secured supply...")
    supply_secured <- extract_supply_secured()
    c_sec_cour_lm <- extract_sup_sec_lm()

    # FIXME hardcoded date
    supply_secured <- transform_supply_secured(
        supply_secured,
        dataset_date = "2022-03-29",
        c_sec_cour_lm
    )
    print(" > Done.")

    print(" > Received supply...")
    supply_received <- load_sup_rec()
    env$supply_received <- supply_received
    supply_received_doses <- transform_sup_rec_doses(supply_received)
    supply_received_produ <- transform_sup_rec_product(supply_received)
    delivery_courses_doses <- eda_sup_rec_courses(
        supply_received, supply_received_doses
    )
    print(" > Done.")


    print(" > Loading supply data back to global environment...")
    env$supply_secured <- supply_secured
    env$c_sec_cour_lm <- c_sec_cour_lm
    env$delivery_courses_doses <- delivery_courses_doses
    env$c_delivery_product <- supply_received_produ
    env$supply_received_doses <- supply_received_doses
    print(" > OK.")

    return(environment())
}