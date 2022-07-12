# 40 - 117 lines of original code

run_entity <- function() {
    source("src/entity/entity_characteristics.r")

    print(" > Loading entity characteristics data...")
    ec <- load_entity_chars()
    print(" > Done.")
    
    print(" > Concerted support list...")
    b_adhoc <- load_conc_supp_list()
    print(" > Done.")
    
    print(" > Transforming entity characteristics data...")
    entity_characteristics <- transform_entity_chars(ec, b_adhoc)
    print(" > Done.")
    return(environment())
}
