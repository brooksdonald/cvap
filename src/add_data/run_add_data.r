
run_add_data <- function(refresh_api) {
  source("src/add_data/add_data.r")
  print(" > Starting local environment for additional data...")

  print(" >> Loading additonal data...")
  datalist_add_data <- load_add_data(refresh_api)
  b_who_dashboard <- datalist_add_data$b_who_dashboard
  print(" > Done.")

  print(" > Returning to local environment.")
  return(environment())
}
