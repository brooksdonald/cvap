
run_finance <- function(entity_characteristics) {
  source("src/finance/finance.r")
  print(" > Starting local environment for fiancing module...")
  
  print(" > Loading financing data...")
  base_finance <- load_finance()
  print(" > Done.")
  
  print(" > Loading funding tracker data...")
  base_finance_urgent <- load_finance_urgent()
  print(" > Done.")
  
  print(" > Loading CDS data...")
  base_finance_cds <- load_finance_cds(entity_characteristics)
  print(" > Done.")
  
  print(" > Transforming financing data...")
  datalist_finance <- transform_finance(
    base_finance, entity_characteristics
  )
  b_fin_fund_del_source <- datalist_finance$base_finance_delivery_summary_source
  b_fin_fund_del_sum <- datalist_finance$base_finance_delivery_summary
  b_fin_fund_del_long <- datalist_finance$base_finance_delivery_summary_long
  print(" > Done.")
  
  print(" > Transforming urgent funding data...")
  datalist_finance_urgent <- transform_finance_urgent(
    base_finance_urgent, entity_characteristics)
  base_fin_urg_fun_long <- datalist_finance_urgent$base_finance_urgent_long
  base_fin_urg_fun_sum <- datalist_finance_urgent$base_finance_urgent_summary
  print(" > Done.")
  
  print(" > Returning to global environment. ")
  return(environment())
}
