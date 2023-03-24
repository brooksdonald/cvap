
run_supply <- function(del_date) {
  source("src/supply/supply.r", TRUE)
  print(" > Starting local environment for supply module...")

  print(" > Loading supply received data...")
  supply_received <- load_supply_received()
  print(" > Done.")
  
  print(" > Transforming supply received doses data...")
  supply_received_doses <- transform_supply_received_doses(supply_received, del_date)
  print(" > Done.")
  
  print(" > Transforming supply received product data...")
  datalist_temp <- transform_supply_received_product(supply_received)
  supply_received_by_product <- datalist_temp$supply_received_product
  c_delivery_product <- datalist_temp$supply_delivered_product
  print(" > Done.")

  print(" > Transforming received courses data...")  
  delivery_courses_doses <- eda_supply_received_courses(
    supply_received, supply_received_doses
    )
  print(" > Done.")

  print(" > Returning to local environment.")
  return(environment())
}
