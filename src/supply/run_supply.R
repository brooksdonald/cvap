
run_supply <- function(sec_date, del_date) {
    print(" > Starting local environment for supply module...")
    source("src/supply/supply_secured.r", TRUE)
    source("src/supply/supply_received.r", TRUE)

    print(" > Loading secured supply data...")
    base_supply_secured <- load_supply_secured_data()
    print(" > Done.")
    
    print(" > Transforming secured supply data...")
    supply_secured <- transform_supply_secured_doses(
      as.data.frame(base_supply_secured$supply_secured),
      sec_date,
      as.data.frame(base_supply_secured$supply_secured_lm)
      )
    print(" > Done.")
    
    print(" > Loading received supply data...")
    supply_received <- load_supply_received()
    print(" > Done.")
    
    print(" > Transforming received supply data...")
    supply_received_doses <- transform_supply_doses(supply_received, del_date)
    supply_received_df <- transform_supply_product(supply_received)
    supply_received_by_product <- supply_received_df$supply_received_product
    c_delivery_product <- supply_received_df$supply_received_jj
    delivery_courses_doses <- transform_supply_courses(
      supply_received, supply_received_doses
    )
    print(" > Done.")

    print(" > Returning to global environment.")
    return(environment())
}
