run_export <- function(a_data) {
  source("eda/export/export.r")
  
  print(" > Filtering for API view...")
  api <- api_export_table(a_data)
  print(" > Done.")

  return(environment())
}
