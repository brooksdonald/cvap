# rows 2169 - 2254

run_eda_supplies <- function(a_data, delivery_courses_doses) {
    source("eda/supply/supply_consolidate.r")

    print(" > Starting local environment for supplies eda")
    print(" > Supplies eda...")
    a_data <- merge_a_data_details(
        a_data, delivery_courses_doses
    )
    print(" > Done.")

    return(environment())
}