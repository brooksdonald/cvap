
run_dvr <- function(auto_cleaning, headers, refresh_api) {
    print("> Checking if all Python libraries are available...")
    python_packages <- c("datetime", "io", "json", "matplotlib", "numpy", 
                         "openpyxl", "os", "pandas", "pickle", "seaborn", 
                         "urllib", "warnings", "sys")
    
    for (module in python_packages) {
      if (!py_module_available(module)) {
        print(paste0(" > ", module, " is not available. Installing now..."))
        py_install(module)
      }
    }
  print(" > All Python libraries loaded.")
  
  print("> Loading src/dvr module scripts...")
  source("src/dvr/dvr_data_ingestion.r")
  print("> Loading general administration data...")
  throughput_data <- main(refresh_api)
  print("> Done.")
  
  source_python("src/dvr/dvr_data_cleaning.py")
  print("> Cleaning general administration data...")
  cleaned_data <- main(auto_cleaning, throughput_data)
  print("> Done.")
  
  source_python("src/dvr/dvr_output_daily.py")
  print("> Interpolating cleaned general administration data...")
  dvr_data <- main(cleaned_data, refresh_api, auto_cleaning)
  print("> Done.")
  
  return(environment())
}
