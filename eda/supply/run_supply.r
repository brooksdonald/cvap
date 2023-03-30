
run_eda_supply <- function(a_data, supply_received_by_product) {
  source("eda/supply/supply.r")
  print(" > Starting local environment for supplies eda module...")

  print(" > Analysing supplies data...")
  a_data <- analyse_supply(a_data)
  print(" > Done.")

  print(" > Merge supplies data...")
  supply_received_by_product <- merge_supply_received_by_product(
    a_data, supply_received_by_product
    )
  print(" > Done.")

  print(" > Returning to local environment.")
  return(environment())
}
