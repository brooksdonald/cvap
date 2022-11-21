
run_add_data <- function(refresh_api) {
    print(" > Starting local environment for additional data module...")
    source("src/add_data/add_data.r")
  
    print(" > Loading additional data...")
    b_who_dashboard <- load_who_dashboard(refresh_api)
    print(" > Done.")
    
    print(" > Adding data to datalist...")
    datalist <- list("b_who_dashboard" = b_who_dashboard)
    print(" > Done.")
    
    print(" > Returning to global environment.")
    return(environment())
}
