
qual_data_consolidate <- function(a_data) {
    print(" >> Selecting relevant columns... ")
    a_data_temp <- select(
      a_data, c(
        "a_iso",
        "adm_fv",
        "a_pop", 
        "cov_total_fv", 
        "t10_status", 
        "t40_status"
        )
      )
  
    print(" >> Generating 'note_drivers' variable... ")
    a_data <- a_data %>%
      mutate(note_drivers = note_drivers_auto)

    print(" >> Adding data to datalist... ")
    datalist <- list(
      "a_data_temp" = a_data_temp,
      "a_data" = a_data
      )
    
    print(" >> Function 'qual_data_consolidate' done")
    return(datalist)
}