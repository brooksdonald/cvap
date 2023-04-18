
run_one_budget_tracker <- function() {
  print(" > Starting local environment for One Budget tracker base data...")
  source("src/one_budget_tracker/one_budget_tracker.r")
  
  print(" >> Load one budget tracker data...")
  base_one_budget_tracker <- load_one_budget_tracker()
  print(" > Done.")

  print(" >> Load one budget cds data...")
  base_one_budget_cds <- load_one_budget_cds()
  print(" > Done.")
  
  print(" >> Load funding requests data...")
  base_requests <- load_requests()
  print(" > Done.")
  
  print(" >> Transform one budget cds data...")
  base_one_budget_cds <- transform_base_one_budget_cds(base_one_budget_cds)
  print(" > Done.")
  
  print(" >> Transform funding requests data...")
  base_requests <- transform_requests(base_requests)
  print(" > Done.")
    
  print(" > Returning to local environment.")
  return(environment())
}
