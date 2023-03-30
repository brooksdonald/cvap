
run_pin <- function(refresh_api) {
  source("src/pin/pin.r")
  print(" > Starting local environment for people in need data...")

  print(" > Loading people in need data...")
  population_pin <- load_pin_data()
  print(" > Done.")

  print(" > Transforming people in need data...")
  population_pin <- transform_pin_data(population_pin)
  print(" > Done.")
  
  print(" > Returning to local environment.")
  return(environment())
}
