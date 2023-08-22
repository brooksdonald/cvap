run_add_data <- function(refresh_api) {
  print("> Loading src/add_data module scripts...")
  source("src/add_data/add_data.r")
  source("src/add_data/pin.r")
  source("src/add_data/demand_planning.r")
  
  print("> Loading additional miscellaneous datasets...")
  datalist <- load_base_data(refresh_api)
  who_dashboard <- datalist$who_dashboard
  print("> Done.")
  
  print(" > Loading people in need data...")
  population_pin <- load_pin_data()
  print(" > Done.")
  
  print(" > Loading people in need data...")
  population_pin <- transform_pin_data(population_pin)
  print(" > Done.")
  
  print(" > Loading demand planning data...")
  b_dp_red <- load_demand_plan_data()
  print(" > Done.")
  
  print(" > Transforming demand planning data...")
  b_dp <- transform_demandplan_data(b_dp_red)
  print(" > Done.")
  
  return(environment())
}