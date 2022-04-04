run_dp <- function(env = .GlobalEnv) {
    source("src/demand-planning/demand_planning.r")
    print(" > Starting local environment for  demand planning")
    print(" > Loading demand planning data...")
    b_dp_red <- load_demand_plan_data()
    print(" > Done.")

    print(" > Transforming demand planning data...")
    b_dp <- transform_demandplan_data(b_dp_red)
    print(" > Done.")
    
    print(" > Returning to global environment. ")

    print(" > Loading demand planning data back to global environment...")
    env$b_dp_red <- b_dp_red
    env$b_dp <- b_dp
    return(environment())
}
