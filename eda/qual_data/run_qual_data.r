
run_eda_qual_data <- function(a_data) {
  source("eda/qual_data/qual_data.r")
  print(" > Starting local environment for qualitative data eda module...")
  
  print(" > Transforming data...")
  datalist_qual_data <- transform_qual_data(a_data)
  a_data_temp <- datalist_qual_data$a_data_temp
  a_data <- datalist_qual_data$a_data
  print(" > Done.")
  
  print(" > Returning to local environment.")
  return(environment())
}