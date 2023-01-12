
merge_pin <- function(a_data, temp_population_pin){
  print(" >> Merging people in need data to 'a_data'...")
  a_data <- a_data %>%
    mutate(cov_pin_fv = adm_td_hum / a_pop_hum,
           a_pop_pin_per = a_pop_pin / a_pop_hum)
  
  print(" >> Function 'merge_pin' done")
  return(a_data)  
}


