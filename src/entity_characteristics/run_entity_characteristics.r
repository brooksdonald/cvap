
run_entity <- function() {
  source("src/entity_characteristics/entity_characteristics.r")
  source("src/entity_characteristics/population_data.r")
  print(" > Starting local environment for entity characteristics...")
  
  print(" > Loading entity characteristics data...")
  base_entitydetails <- load_base_entitydetails()
  print(" > Done.")
  
  print(" > Loading base adhoc data...")
  base_adhoc <- load_base_adhoc()
  print(" > Done.")
  
  print(" > Loading base population data...")
  base_population <- load_base_population()
  print(" > Done.")
  
  print(" > Transforming entity characteristics data...")
  entity_characteristics <- transform_entity_charcteristics(
    base_entitydetails, base_adhoc)
  print(" > Done.")
  
  print(" > Transforming base population data...")
  population_data <- transform_base_population(base_population)
  print(" > Done.")

  print(" > Returning to local environment.")
  return(environment())
}
