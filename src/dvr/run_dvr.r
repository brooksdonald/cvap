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

        print(" > Defining storage locations...")
        folder <- "_input/dvr_data"
        name_00 <- "analysis_vx_throughput_data.csv"
        name_01 <- "analysis_vx_throughput_supply.csv"
        name_02 <- "analysis_vx_throughput_data_cleaned.csv"
        name_03 <- "analysis_vx_throughput_output_daily.csv"
        name_04 <- "analysis_vx_data_fixes.csv"

        print(" > Opening Python Environment...")
        source_python("src/dvr/dvr_data_ingestion.py")
        throughput_data <- main(folder, name_00, refresh_api)

        wiise_supply_data <- helper_wiise_api(
            "https://extranet.who.int/xmart-api/odata/WIISE/V_COV_UTI_LONG",
            headers, FALSE)
        source_python("src/dvr/dvr_supply_data.py")
        supply_data <- main(folder, name_01, wiise_supply_data)

        source_python("src/dvr/dvr_data_cleaning.py")
        cleaned_data <- main(auto_cleaning, throughput_data, folder, name_02)

        source_python("src/dvr/dvr_output_daily.py")
        dvr_data <- main(supply_data, cleaned_data, folder, name_03,
            refresh_api, auto_cleaning)

        source_python("src/dvr/dvr_data_fixes.py")
        main(throughput_data, dvr_data, folder, name_04)

    } else {
        print(" > adm_api == FALSE")
        print(" > Administration data will be imported via Excel file: base_dvr_current.xlsx")
        print(" > Python environment will not be used.")
        dvr_data <- NULL
    }
    return(environment())
}