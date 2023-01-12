run_supply <- function(del_date) {
    print(" > Starting local environment for supply module...")

    source("src/supply/supply_received.r", TRUE)

    print(" > Received supply...")
    supply_received <- load_sup_rec()
    supply_received_doses <- transform_sup_rec_doses(supply_received, del_date)
    datalist2 <- transform_sup_rec_product(supply_received)
    supply_received_by_product <- datalist2$supply_received_by_product
    c_delivery_product <- datalist2$c_delivery_product
    delivery_courses_doses <- eda_sup_rec_courses(
        supply_received, supply_received_doses
    )
    print(" > Done.")

    return(environment())
}