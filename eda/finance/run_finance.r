
run_financing <- function(a_data) {
    print(" > Starting local environment for financing eda module...")
    source("eda/finance/finance_consolidate.r")
    
    print(" > Calculating financing overview...")
    a_data <- finance_consolidate(a_data)
    print(" > Done.")
    
    print(" > Filtering AMC covax status...")
    a_data_amc <- amc_covax_status(a_data)
    print(" > Done.")
    
    print(" > Filtering HIC income group...")
    a_data_hic <- hic_income_group(a_data)
    print(" > Done.")
    
    print(" > Filtering CoVDP csc status...")
    a_data_csc <- covdp_csc_status(a_data)
    print(" > Done.")
    
    print(" > Filtering CoVDP IFC status ...")
    a_data_ifc <- covdp_ifc_status(a_data)
    print(" > Done.")
    
    print(" > Filtering African continent...")
    a_data_africa <- africa_continent(a_data)
    print(" > Done.")
    
    print(" > Filtering for API view...")
    api <- api_export_table(a_data)
    print(" > Done.")
  
    print(" > Returning to global environment.")
    return(environment())
}
