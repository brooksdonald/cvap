
run_qual_data <- function(a_data) {
    print(" > Starting local environment for qualitative data module...")
    source("eda/qual_data/qual_data.r")

    print(" > Consolidating qualitative data...")
    datalist <- qual_data_consolidate(a_data)
    a_data_temp <- datalist$a_data_temp
    a_data <- datalist$a_data
    print(" > Done.")
    
    print(" > Returning to global environment.")
    return(environment())
}
