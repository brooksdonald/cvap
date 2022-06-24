run_vaccination_rate_in_r <- function() {
    source_python("src/vaccination_rate/vxrate_data_ingestion.py", envir = globalenv(), convert = TRUE)
    source_python("src/vaccination_rate/vxrate_supply_data.py", envir = globalenv(), convert = TRUE)
    source_python("src/vaccination_rate/vxrate_data_cleaning.py", envir = globalenv(), convert = TRUE)
    source_python("src/vaccination_rate/vxrate_output_daily.py", envir = globalenv(), convert = TRUE)
    source_python("src/vaccination_rate/vxrate_data_fixes.py", envir = globalenv(), convert = TRUE)
}