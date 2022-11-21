
run_dp <- function() {
    print(" > Starting local environment for demand planning module...")
    source("src/demand_planning/demand_planning.r")
  
    print(" > Loading demand planning data...")
    b_dp_red <- load_demand_plan()
    print(" > Done.")
    
    print(" > Transforming demand planning data...")
    b_dp <- transform_demand_plan(b_dp_red)
    print(" > Done.")
    
    print(" > Returning to global environment.")
    return(environment())
}