
run_dvr <- function(auto_cleaning, headers, refresh_api) {
  
  print(" > Opening Python Environment...")
  source("src/dvr/translation_ingestion.r")
  throughput_data <- main(refresh_api)
  
  source("src/dvr/translation_cleaning_2.r")
  cleaned_data <- main(auto_cleaning, throughput_data)
  plot_function <- export_plots_of_changes
  
  source_python("src/dvr/dvr_output_daily.py")
  dvr_data <- main(cleaned_data, refresh_api, auto_cleaning, plot_function)
  
  print(" > Returning to local environment")
  return(environment())
}


CStack_info()

