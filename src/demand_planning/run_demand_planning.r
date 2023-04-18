
run_dp <- function() {
  source("src/demand_planning/demand_planning.r")
  print(" > Starting local environment for demand planning...")
  
  print(" > Loading demand planning data...")
  base_demandplanning <- load_demandplanning()
  print(" > Done.")
  
  print(" > Transforming demand planning data...")
  b_dp <- transform_demandplanning(base_demandplanning)
  print(" > Done.")
  
  print(" > Returning to global environment. ")
  return(environment())
}
