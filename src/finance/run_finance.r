run_finance <- function(entity_characteristics) {
  source("src/finance/finance.r")
  source("src/finance/finance_timeseries.r")
  
  print(" > Starting local environment for fiancing module")
  print(" > Loading financing data...")
  b_fin_funding <- load_finance_data()
  # base_fin_urg_fun <- load_finance_urgent_data()
  # base_fin_cds_red <-
  #   load_finance_cds_data(entity_characteristics)
  print(" > Done.")
  
  print(" > Transforming financing data...")
  datalist <- transform_finance_data(b_fin_funding, entity_characteristics)
  b_fin_fund_del_source <- datalist$b_fin_fund_del_source
  b_fin_fund_del_sum <- datalist$b_fin_fund_del_sum
  b_fin_fund_del_long <- datalist$b_fin_fund_del_long
  print(" > Done.")
  
  # print(" > Transforming urgent funding data...")
  # datalist2 <- transform_fund_urgent_data(base_fin_urg_fun, entity_characteristics)
  # base_fin_urg_fun_long <- datalist2$base_fin_urg_fun_long
  # base_fin_urg_fun_sum <- datalist2$base_fin_urg_fun_sum
  # print(" > Done.")
  # 
  print(" > Producing finance timeseries data...")
  overall_fin_cumul_long <- import_finance_data()
  # overall_fin_long <- transform_fin_data(overall_fin_cumul_long) # Uncomment to include net per month
  print(" > Done.")
  
  print(" > Returning to global environment. ")
  
  return(environment())
}