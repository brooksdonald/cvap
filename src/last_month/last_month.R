
load_last_month_data <- function() {
    print(" >> Loading last month output data...")
    base_data_lm <- data.frame(
      read_excel(
        "data/input/output_master_lm.xlsx",
        sheet = "0_base_data"
        )
      )
      
    print(" >> Function 'load_last_month_data' done")
    return(base_data_lm)
}
