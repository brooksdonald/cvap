run_vaccination_rate <- function(adm_api, auto_cleaning) {
    if (adm_api) {

        print(" > Checking if all Python libraries are available...")
        python_packages <- c("datetime", "io", "json", "matplotlib",
            "numpy", "openpyxl", "os", "pandas", "pickle",
            "seaborn", "urllib", "warnings")
        for (module in python_packages) {
            if (!py_module_available(module)) {
                print(" > ", module, " is not available. Installing now...")
                py_install(module)
            }
        }
        print(" > All Python libraries loaded.")

        print(" > Defining storage locations...")
        folder <- "_input/supply_data" # should store necessary excel files
        name_01 <- "analysis_vx_throughput_data.csv"
        name_02 <- "analysis_vx_throughput_supply.csv"
        name_03 <- "analysis_vx_throughput_data_cleaned.csv"
        name_04 <- "analysis_vx_throughput_output_daily.csv"
        name_05 <- "analysis_vx_data_fixes.csv"


        print(" > Opening Python Environment...")
        source_python("src/vaccination_rate/vxrate_data_ingestion.py")
        throughput_data <- main(folder, name_01)

        source_python("src/vaccination_rate/vxrate_supply_data.py")
        supply_data <- main(folder, name_02)

        source_python("src/vaccination_rate/vxrate_data_cleaning.py")
        cleaned_data <- main(auto_cleaning, throughput_data, folder, name_03)

        source_python("src/vaccination_rate/vxrate_output_daily.py")
        adm_data <- main(supply_data, cleaned_data, folder, name_04)

        source_python("src/vaccination_rate/vxrate_data_fixes.py")
        main(throughput_data, adm_data, folder, name_05)

    } else {
        print(" > adm_api == FALSE")
        print(" > Administration data will be imported via Excel file: base_dvr_current.xlsx")
        print(" > Python environment will not be used.")
        adm_data <- NULL
    }
    return(environment())
}