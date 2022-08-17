run_dvr <- function(adm_api, auto_cleaning, headers, refresh_api) {
    if (adm_api) {

        print(" > Checking if all Python libraries are available...")
        python_packages <- c("datetime", "io", "json", "matplotlib",
            "numpy", "openpyxl", "os", "pandas", "pickle",
            "seaborn", "urllib", "warnings")
        for (module in python_packages) {
            if (!py_module_available(module)) {
                print(paste0(" > ", module, " is not available. Installing now..."))
                py_install(module)
            }
        }
        print(" > All Python libraries loaded.")

        print(" > Opening Python Environment...")
        source_python("src/dvr/dvr_data_ingestion.py")
        throughput_data <- main(refresh_api)

        source_python("src/dvr/dvr_data_cleaning.py")
        cleaned_data <- main(auto_cleaning, throughput_data)
        plot_function <- export_plots_of_changes

        source_python("src/dvr/dvr_output_daily.py")
        dvr_data <- main(cleaned_data, refresh_api, auto_cleaning, plot_function)

        source_python("src/dvr/dvr_data_fixes.py")
        main(throughput_data, dvr_data)

    } else {
        print(" > adm_api == FALSE")
        print(" > Administration data will be imported via Excel file: base_dvr_current.xlsx")
        print(" > Python environment will not be used.")
        dvr_data <- NULL
    }
    return(environment())
}