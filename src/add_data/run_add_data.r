run_add_data <- function(refresh_api) {
  print("> Loading src/add_data module scripts...")
  source("src/add_data/add_data.r")
  
  print("> Loading additional miscellaneous datasets...")
  datalist <- load_base_data(refresh_api)
  who_dashboard <- datalist$who_dashboard
  print("> Done.")
  
  return(environment())
}