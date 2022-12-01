
calculate_pin <- function(population_pin){
  print(" >> Calculating aggregate sum for people in need variables...")

  temp_adm_td_pin <- pin_env$population_pin %>%
    group_by(a_iso) %>%
    summarise(adm_td_pin = sum(adm_td_pin))
  
  temp_a_pop_pin <- pin_env$population_pin %>%
    group_by(a_iso) %>%
    summarise(a_pop_pin = sum(a_pop_pin))
  
  temp_ghc_date_pin <- pin_env$population_pin %>%
    group_by(a_iso) %>%
    summarise(ghc_date = max(ghc_date))
  
  temp_population_pin <- left_join(
    temp_adm_td_pin,
    temp_a_pop_pin,
    by=c('a_iso'='a_iso')
  )
  
  temp_population_pin <- left_join(
    temp_population_pin,
    temp_ghc_date_pin,
    by=c('a_iso'='a_iso')
  )
  temp_population_pin <- temp_population_pin %>% distinct()
  
  print(" >> Function 'calculate_pin' done")
  return(temp_population_pin)  
}
  
merge_pin <- function(a_data, temp_population_pin){
  print(" >> Merging people in need data to 'a_data'...")
  a_data <- left_join(
    a_data,
    temp_population_pin,
    by=c('a_iso'='a_iso')
  )
  
  a_data <- a_data %>%
    mutate(cov_pin_fv = adm_td_pin / a_pop_pin)
    
  a_data <- a_data %>%
    mutate(a_pop_pin_per = a_pop_pin / a_pop)
  
  print(" >> Function 'merge_pin' done")
  return(a_data)  
}


