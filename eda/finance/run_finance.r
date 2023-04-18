
run_eda_finance <- function(a_data) {
  source("eda/finance/finance.r")
  print(" > Starting local environment for finance eda module...")
  
  print(" > Calculating financing overview...")
  a_data <- finance_analyse(a_data)
  print(" > Done.")
  
  print(" > Filtering AMC countries...")
  a_data_amc <- finance_analyse_amc(a_data)
  print(" > Done.")
  
  print(" > Filtering HIC income countries...")
  a_data_hic <- finance_analyse_hic(a_data)
  print(" > Done.")
  
  print(" > Filtering Concerted support countries...")
  a_data_csc <- finance_analyse_csc(a_data)
  print(" > Done.")
  
  print(" > Filtering Immediate focus countries...")
  a_data_ifc <- finance_analyse_ifc(a_data)
  print(" > Done.")
  
  print(" > Filtering African countries...")
  a_data_afr <- finance_analyse_afr(a_data)
  print(" > Done.")
  
  print(" > Filtering for API view...")
  api <- api_export_table(a_data)
  print(" > Done.")
  
  print(" > Returning to local environment.")
  return(environment())
}
