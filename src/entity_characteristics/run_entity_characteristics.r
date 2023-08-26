run_entity <- function() {
  print("> Loading src/entity_characteristics module scripts...")
  source("src/entity_characteristics/ec_ref.r")
  source("src/entity_characteristics/ec_pop.r")
  
  print("> Loading entity detail data...")
  entity <- load_entity_details()
  print("> Done.")
  
  print("> Loading adhoc detail data...")
  adhoc <- load_adhoc_details()
  print("> Done.")
  
  print("> Transforming / joining entity & adhoc detail data...")
  entity_characteristics <- transform_entity_details(entity, adhoc)
  print("> Done.")
  
  print("> Loading population data...")
  population_base <- load_base_population()
  print("> Done.")
  
  print("> Transforming population data...")
  population <- transform_base_population(population_base)
  print("> Done.")
  
  return(environment())
}