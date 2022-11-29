
calculate_elig_booster <- function(adm_all_long){
  print(" >> Calculating elgible booster variable...")
  adm_all_long <- adm_all_long %>% 
    group_by(a_iso) %>%
    dplyr::mutate(adm_elig_booster = lag(adm_fv_adj, n = 180, order_by=adm_date, default = NA)) %>%
    arrange(adm_date)

  print(" >> Function 'calculate_elig_booster' done")
  return(adm_all_long)  
}


merge_elig_booster <- function(b_vxrate_pub, a_data){
  print(" >> Filtering to most recent data...")
  temp_adm_all_long <- b_vxrate_pub %>% 
    group_by(a_iso) %>%
    slice(which.max(as.Date(adm_date, '%m/%d/%Y')))

  print(" >> Selecting relevant columns...")
  temp_adm_all_long <- select(
    temp_adm_all_long,
    c(
      "a_iso",
      "adm_elig_booster"
    )
  )
  
  print(" >> Merging elgible booster data to 'a_data'...")
  a_data <- left_join(
    a_data,
    temp_adm_all_long,
    by=c('a_iso'='a_iso')
  )
  
  print(" >> Calculating eligible for booster variable...")
  a_data <- a_data %>%
    mutate(elig_booster_notreceived = adm_elig_booster - adm_booster_homo)
  
  a_data <- a_data %>%
    mutate(elig_booster_notreceived = if_else(
      elig_booster_notreceived < 0,
      0,
      elig_booster_notreceived))
  
  a_data <- a_data %>%
    mutate(elig_booster_notreceived_per = elig_booster_notreceived / a_pop)
  
  print(" >> Function 'merge_elig_booster' done")
  return(a_data)  
}
