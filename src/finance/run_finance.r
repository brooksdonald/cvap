run_finance <- function(entity_characteristics) {
  print("> Loading src/finance module scripts...")
  source("src/finance/finance.r")
  source("src/finance/finance_timeseries.r")

  print("> Loading financing data...")
  base_fin <- load_finance_data()
  print("> Done.")
  
  print("> Transforming financing data...")
  datalist <- transform_finance_data(base_fin, entity_characteristics)
  fin_del_sum_source <- datalist$fin_del_sum_source
  fin_del_sum <- datalist$fin_del_sum
  fin_del_sum_long <- datalist$fin_del_sum_long
  print(" > Done.")

  print("> Reconstructing financing timeseries data...")
  overall_fin_cumul_long <- import_finance_data()
  overall_fin_long <- transform_fin_data(overall_fin_cumul_long)
  print(" > Done.")
  
  return(environment())
}