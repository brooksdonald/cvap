
run_one_budget_tracker <- function(a_data) {
  print(" > Starting local environment for funding tracker base data...")
  source("consolidate/one_budget_tracker/one_budget_tracker.r")

  print(" >> Load one budget tracker data...")
  base_one_budget_tracker <- load_one_budget_tracker()
  print(" > Done.")

  print(" >> Load one budget cds data...")
  base_one_budget_cds <- load_one_budget_cds()
  print(" > Done.")
  
  print(" >> Load funding requests data...")
  base_requests <- load_requests()
  print(" > Done.")
  
    
    
  base_one_budget_cds <- transform_base_one_budget_cds(base_one_budget_cds)
    
  a_data <- merge_one_budget_tracker(a_data, base_one_budget_tracker)
  
    
  print(" > Returning to local environment.")

    
    return(environment())
}