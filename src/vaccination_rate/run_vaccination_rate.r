run_vaccination_rate <- function() {
    source_python("src/vaccination_rate/vxrate_data_ingestion.py", convert = TRUE)
    source_python("src/vaccination_rate/vxrate_supply_data.py", convert = TRUE)
    source_python("src/vaccination_rate/vxrate_data_cleaning.py", convert = TRUE)
    source_python("src/vaccination_rate/vxrate_output_daily.py", convert = TRUE)
    adm_data <- df12
    source_python("src/vaccination_rate/vxrate_data_fixes.py", convert = TRUE)
    return(environment())
}