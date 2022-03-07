# Rows 3283 - 3308 of original code. 

run_export <- function(env = .GlobalEnv) {
    source("eda/export/export.r")

    print(" > Starting local environment for data export to excel file")
    output_report <- write_to_excel()
    print(" > Done.")

    print(" > Loading excel file back to global environment...") 
    env$output_report <- output_report
    print(" > Done.")

    return(environment())
}

run_export()