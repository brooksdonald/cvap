
pin_analyse <- function(a_data){
  print(" >> Merging people in need data to 'a_data'...")
  a_data <- a_data %>%
    mutate(cov_pin_fv = adm_td_hum / a_pop_hum,
           a_pop_pin_per = a_pop_pin / a_pop_hum)
  
  print(" >> Function 'pin_analyse' done")
  return(a_data)  
}
