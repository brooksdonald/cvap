
run_entity <- function() {
    print(" > Starting local environment for entity characteristics module...")
    source("src/entity_characteristics/ec_details.r")
    source("src/entity_characteristics/ec_pop.r")

    print(" > Loading entity characteristics data...")
    entity_details <- load_entity_chars()
    print(" > Done.")
    
    print(" > Loading adhoc characteristics data...")
    adhoc_details <- load_base_adhoc()
    print(" > Done.")
    
    print(" > Transforming entity & adhoc characteristics data...")
    entity_characteristics <- transform_entity_chars(entity_details, adhoc_details)
    print(" > Done.")

    print(" > Loading population data...")
    population_data <- load_base_population()
    print(" > Done.")
    
    print(" > Transforming population data...")
    population_data <- transform_base_population(population_data, adhoc_details)
    print(" > Done.")

    print(" > Returning to global environment.")
    return(environment())
}
