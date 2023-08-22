
values_table <- function(a_data, a_data_amc, a_data_africa,
                         a_data_csc, date_refresh) {
  z_values <- data.frame(c("Text"))

  z_values$pop_amc <- sum(a_data_amc$a_pop, na.rm = TRUE)
  z_values$pop_amc_hcw <- sum(a_data_amc$a_pop_hcw, na.rm = TRUE)
  z_values$pop_africa <- sum(a_data_africa$a_pop, na.rm = TRUE)
  z_values$pop_csc <- sum(a_data_csc$a_pop, na.rm = TRUE)

  z_values$pop_amc_10 <- z_values$pop_amc * 0.1
  z_values$pop_amc_20 <- z_values$pop_amc * 0.2
  z_values$pop_amc_40 <- z_values$pop_amc * 0.4
  z_values$pop_amc_70 <- z_values$pop_amc * 0.7

  z_values$refresh_date_value <- date_refresh
  
  print(" >> Function 'values_table' done")
  return(z_values)
}
