# 40 - 117 lines of original code

run_entity <- function() {
    source("src/entity_characteristics/ec_ref.r")
    source("src/entity_characteristics/ec_pop_gen.r")
    source("src/entity_characteristics/ec_pop_hcw.r")

    print(" > Loading entity characteristics data...")
    ec <- load_entity_chars()
    print(" > Done.")
    
    print(" > Concerted support list...")
    b_adhoc <- load_conc_supp_list()
    print(" > Done.")
    
    print(" > Transforming entity characteristics data...")
    entity_characteristics <- transform_entity_chars(ec, b_adhoc)
    print(" > Done.")

    print(" > Population healthcare workers...")
    population_hcw <- load_population_hcw()
    population_hcw <- transform_population_hcw(
        population_hcw
    )
    print(" > Done.")

    print(" > Base population...")
    population_base <- load_base_population()
    population_data <- transform_base_population(
        population_base, population_hcw
    )
    print(" > Done.")

    return(environment())
}
