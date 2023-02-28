
run_eda_supplies <- function(a_data, supply_received_by_product) {
    source("eda/supply/supply_consolidate.r")

    print(" > Starting local environment for supplies eda")
    print(" > Supplies eda...")
    a_data <- merge_a_data_details(
        a_data
    )
    print(" > Done.")

    print(" > Merge supplies data...")
    supply_received_by_product <- merge_supply_received_by_product(
      a_data, supply_received_by_product
    )
    print(" > Done.")
    

    return(environment())
}