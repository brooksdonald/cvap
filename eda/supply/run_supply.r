
run_eda_supplies <- function(a_data, supply_secured, delivery_courses_doses) {
    print(" > Starting local environment for supply eda module...")
    source("eda/supply/supply_calculate.r")

    print(" > Calculating supplies eda data...")
    a_data <- supply_calculate(a_data)
    print(" > Done.")

    print(" > Returning to global environment.")
    return(environment())
}
