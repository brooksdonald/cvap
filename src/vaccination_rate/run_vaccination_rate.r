run_vaccination_rate <- function() {
    print(" > Checking if all Python libraries are available...")
    python_packages <- c("datetime", "io", "json", "matplotlib",
        "numpy", "openpyxl", "os", "pandas", "pickle", "seaborn", "urllib")
    for (module in python_packages) {
        if (!py_module_available(module)) {
            py_install(module)
        }
    }
    print(" > All Python libraries loaded.")
    source_python("src/vaccination_rate/vxrate_data_ingestion.py", convert = TRUE)
    source_python("src/vaccination_rate/vxrate_supply_data.py", convert = TRUE)
    source_python("src/vaccination_rate/vxrate_data_cleaning.py", convert = TRUE)
    source_python("src/vaccination_rate/vxrate_output_daily.py", convert = TRUE)
    adm_data <- df12
    source_python("src/vaccination_rate/vxrate_data_fixes.py", convert = TRUE)
    return(environment())
}