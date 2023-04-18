
merge_data_checks <- function(a_data, a_data_lm) {
  a_data_lm_temp <-
    select(
      a_data_lm,
      c(
        "a_iso",
        "adm_fv_homo_lm",
        "adm_fv_hcw_homo_lm",
        "adm_fv_60p_homo_lm",
        "adm_fv_hcw_lm",
        "adm_fv_60p_lm",
        "cov_hcw_fv_lm",
        "cov_60p_fv_lm"
      )
    )
  
  a_data<-merge(x=a_data,y=a_data_lm_temp, by="a_iso",all.x=TRUE)
  
  a_data$adm_date_hcw <- paste(as.character(a_data$adm_date_hcw), "-28")
  a_data$adm_date_60p <- paste(as.character(a_data$adm_date_60p), "-28")
  
  a_data$adm_date_hcw <- gsub(" ", "", a_data$adm_date_hcw)
  a_data$adm_date_60p <- gsub(" ", "", a_data$adm_date_60p)
  
  a_data$adm_date_hcw <- as.Date(a_data$adm_date_hcw, format="%Y-%m-%d")  
  a_data$adm_date_60p <- as.Date(a_data$adm_date_60p, format="%Y-%m-%d")  
  
  print(" >> Finished 'merge_data_checks' function...")
  return(a_data)
}


data_checks <- function(a_data) {
  print(" >>> Checking administration data")
  a_data <- a_data %>%
    mutate(check_adm_total_a1d = if_else(
      adm_a1d_homo < adm_fv_homo & (!is.na(adm_a1d_homo) & !is.na(adm_fv_homo)),
      "Lower",
      if_else(
        adm_a1d_homo > a_pop & (!is.na(adm_a1d_homo) & !is.na(a_pop)),
        "Upper",
        "Okay"))) %>%
    mutate(check_adm_total_fv = if_else(
      adm_fv_homo < adm_booster_homo & (!is.na(adm_fv_homo) & !is.na(adm_booster_homo)),
      "Lower",
      if_else(
        adm_fv_homo > a_pop & (!is.na(adm_fv_homo) & !is.na(a_pop)),
        "Upper",
        "Okay"))) %>%
    mutate(check_adm_total_booster= if_else(
      adm_booster_homo < 0 & !is.na(adm_booster_homo),
      "Lower",
      if_else(
        adm_booster_homo > a_pop & (!is.na(adm_booster_homo) & !is.na(a_pop)),
        "Upper",
        "Okay"))) %>%
    mutate(check_adm_hcw_a1d = if_else(
      adm_a1d_hcw_homo < adm_fv_hcw_homo & (!is.na(adm_a1d_hcw_homo) & !is.na(adm_fv_hcw_homo)),
      "Lower",
      if_else(
        adm_a1d_hcw_homo > a_pop_hcw & (!is.na(adm_a1d_hcw_homo) & !is.na(a_pop_hcw)),
        "Upper",
        "Okay"))) %>%
    mutate(check_adm_hcw_fv = if_else(
      adm_fv_hcw_homo < adm_booster_hcw_homo & (!is.na(adm_fv_hcw_homo) & !is.na(adm_booster_hcw_homo)),
      "Lower",
      if_else(
        adm_fv_hcw_homo > a_pop_hcw & (!is.na(adm_fv_hcw_homo) & !is.na(a_pop_hcw)),
        "Upper",
        "Okay"))) %>%
    mutate(check_adm_hcw_booster = if_else(
      adm_booster_hcw_homo < 0 & !is.na(adm_booster_hcw_homo),
      "Lower",
      if_else(
        adm_booster_hcw_homo > a_pop_hcw & (!is.na(adm_booster_hcw_homo) & !is.na(a_pop_hcw)),
        "Upper",
        "Okay"))) %>%
    mutate(check_adm_60p_a1d = if_else(
      adm_a1d_60p_homo < adm_fv_60p_homo & (!is.na(adm_a1d_60p_homo) & !is.na(adm_fv_60p_homo)),
      "Lower",
      if_else(
        adm_a1d_60p_homo > a_pop_older & (!is.na(adm_a1d_60p_homo) & !is.na(a_pop_older)),
        "Upper",
        "Okay"))) %>%
    mutate(check_adm_60p_fv = if_else(
      adm_fv_60p_homo < adm_booster_60p_homo & (!is.na(adm_fv_60p_homo) & !is.na(adm_booster_60p_homo)),
      "Lower",
      if_else(
        adm_fv_60p_homo > a_pop_older & (!is.na(adm_fv_60p_homo) & !is.na(a_pop_older)),
        "Upper",
        "Okay"))) %>%
    mutate(check_adm_60p_booster = if_else(
      adm_booster_60p_homo < 0 & !is.na(adm_booster_60p_homo),
      "Lower",
      if_else(
        adm_booster_60p_homo > a_pop_older & (!is.na(adm_booster_60p_homo) & !is.na(a_pop_older)),
        "Upper",
        "Okay")))

  
  print(" >>> Checking coverage data")
  a_data <- a_data %>%
    mutate(check_cov_total_a1d = if_else(
      cov_total_a1d < cov_total_fv & (!is.na(cov_total_a1d) & !is.na(cov_total_fv)),
      "Lower",
      if_else(
        cov_total_a1d > 1.00 & !is.na(cov_total_a1d),
        "Upper",
        "Okay"))) %>%
    mutate(check_cov_total_fv = if_else(
      cov_total_fv < cov_total_booster & (!is.na(cov_total_fv) & !is.na(cov_total_booster)),
      "Lower",
      if_else(
        cov_total_fv > 1.00 & !is.na(cov_total_fv),
        "Upper",
        "Okay"))) %>%
    mutate(check_cov_total_booster= if_else(
      cov_total_booster < 0 & !is.na(cov_total_booster),
      "Lower",
      if_else(
        cov_total_booster > 1.00 & !is.na(cov_total_booster),
        "Upper",
        "Okay"))) %>%
    mutate(check_cov_hcw_a1d = if_else(
      cov_hcw_a1d < cov_hcw_fv & (!is.na(cov_hcw_a1d) & !is.na(cov_hcw_fv)),
      "Lower",
      if_else(
        cov_hcw_a1d > 1.00 & !is.na(cov_hcw_a1d),
        "Upper",
        "Okay"))) %>%
    mutate(check_cov_hcw_fv = if_else(
      cov_hcw_fv < cov_hcw_booster & (!is.na(cov_hcw_fv) & !is.na(cov_hcw_booster)),
      "Lower",
      if_else(
        cov_hcw_fv > 1.00 & !is.na(cov_hcw_fv),
        "Upper",
        "Okay"))) %>%
    mutate(check_cov_hcw_booster = if_else(
      cov_hcw_booster < 0 & !is.na(cov_hcw_booster),
      "Lower",
      if_else(
        cov_hcw_booster > 1.00 & !is.na(cov_hcw_booster),
        "Upper",
        "Okay"))) %>%
    mutate(check_cov_60p_a1d = if_else(
      cov_60p_a1d < cov_60p_fv & (!is.na(cov_60p_a1d) & !is.na(cov_60p_fv)),
      "Lower",
      if_else(
        cov_60p_a1d > 1.00 & !is.na(cov_60p_a1d),
        "Upper",
        "Okay"))) %>%
    mutate(check_cov_60p_fv = if_else(
      cov_60p_fv < cov_60p_booster & (!is.na(cov_60p_fv) & !is.na(cov_60p_booster)),
      "Lower",
      if_else(
        cov_60p_fv > 1.00 & !is.na(cov_60p_fv),
        "Upper",
        "Okay"))) %>%
    mutate(check_cov_60p_booster = if_else(
      cov_60p_booster < 0 & !is.na(cov_60p_booster),
      "Lower",
      if_else(
        cov_60p_booster > 1.00 & !is.na(cov_60p_booster),
        "Upper",
        "Okay")))

  
  print(" >>> Checking last month data")
  a_data <- a_data %>%
    mutate(check_adm_total_fv_lm = if_else(
      (adm_fv_homo - adm_fv_homo_lm) <  0 & (!is.na(adm_fv_homo) & !is.na(adm_fv_homo_lm)),
      "Lower",
      if_else(
        ((adm_fv_homo - adm_fv_homo_lm) > (adm_fv_homo_lm * .05)) & (!is.na(adm_fv_homo) & !is.na(adm_fv_homo_lm)),
        "Upper",
          "Okay"))) %>%
    mutate(check_adm_hcw_fv_lm = if_else(
      (adm_fv_hcw_homo - adm_fv_hcw_homo_lm) <  0 & (!is.na(adm_fv_hcw_homo) & !is.na(adm_fv_hcw_homo_lm)),
      "Lower",
      if_else(
        ((adm_fv_hcw_homo - adm_fv_hcw_homo_lm) > (adm_fv_hcw_homo_lm * .05)) & (!is.na(adm_fv_hcw_homo) & !is.na(adm_fv_hcw_homo_lm)),
        "Upper",
          "Okay"))) %>%
    mutate(check_adm_60p_fv_lm = if_else(
      (adm_fv_60p_homo - adm_fv_60p_homo_lm) <  0 & (!is.na(adm_fv_60p_homo) & !is.na(adm_fv_60p_homo_lm)),
      "Lower",
      if_else(
        ((adm_fv_60p_homo - adm_fv_60p_homo_lm) > (adm_fv_60p_homo_lm * .05)) & (!is.na(adm_fv_60p_homo) & !is.na(adm_fv_60p_homo_lm)),
        "Upper",
          "Okay"))) %>%
    mutate(check_cov_total_fv_lm = if_else(
      (cov_total_fv - cov_total_fv_lm) <  0 & (!is.na(cov_total_fv) & !is.na(cov_total_fv_lm)),
      "Lower",
      if_else(
        ((cov_total_fv - cov_total_fv_lm) > (cov_total_fv_lm * .05)) & (!is.na(cov_total_fv) & !is.na(cov_total_fv_lm)),
        "Upper",
          "Okay"))) %>%
    mutate(check_cov_hcw_fv_lm = if_else(
      (cov_hcw_fv - cov_hcw_fv_lm) <  0 & (!is.na(cov_hcw_fv) & !is.na(cov_hcw_fv_lm)),
      "Lower",
      if_else(
        ((cov_hcw_fv - cov_hcw_fv_lm) > (cov_hcw_fv_lm * .05)) & (!is.na(cov_hcw_fv) & !is.na(cov_hcw_fv_lm)),
        "Upper",
        "Okay"))) %>%
    mutate(check_cov_60p_fv_lm = if_else(
      (cov_60p_fv - cov_60p_fv_lm) <  0 & (!is.na(cov_60p_fv) & !is.na(cov_60p_fv_lm)),
      "Lower",
      if_else(
        ((cov_60p_fv - cov_60p_fv_lm) > (cov_60p_fv_lm * .05)) & (!is.na(cov_60p_fv) & !is.na(cov_60p_fv_lm)),
        "Upper",
        "Okay")))

    
  print(" >>> Checking delivery doses and adm date data")
  a_data <- a_data %>%
    mutate(check_del_dose_total_lm = if_else(
      (del_dose_total - del_dose_total_lm) <  0 & (!is.na(del_dose_total) & !is.na(del_dose_total_lm)),
      "Lower",
      if_else(
        ((del_dose_total - del_dose_total_lm) > (del_dose_total_lm * .05)) & (!is.na(del_dose_total) & !is.na(del_dose_total_lm)),
        "Upper",
        "Okay"))) %>%
    mutate(check_adm_date = as.numeric(a_refresh_date) - as.numeric(adm_date)) %>%
    mutate(check_adm_date_hcw = as.numeric(a_refresh_date) - as.numeric(adm_date_hcw)) %>%
    mutate(check_adm_date_60p = as.numeric(a_refresh_date) - as.numeric(adm_date_60p))  
  
  
  a_data <- a_data %>%
    mutate(check_adm_a1d_bound = if_else(
      check_adm_total_a1d == "Lower" | check_adm_hcw_a1d == "Lower" | check_adm_60p_a1d == "Lower",
      "Lower",
      if_else(
        check_adm_total_a1d == "Upper" | check_adm_hcw_a1d == "Upper" | check_adm_60p_a1d == "Upper",
        "Upper",
        "")
    )) %>%        
    mutate(check_adm_fv_bound = if_else(
      check_adm_total_fv == "Lower" | check_adm_hcw_fv == "Lower" | check_adm_60p_fv == "Lower",
      "Lower",
      if_else(
        check_adm_total_fv == "Upper" | check_adm_hcw_fv == "Upper" | check_adm_60p_fv == "Upper",
        "Upper",
        "")
    )) %>%    
    mutate(check_adm_booster_bound = if_else(
      check_adm_total_booster == "Lower" | check_adm_hcw_booster == "Lower" | check_adm_60p_booster == "Lower",
      "Lower",
      if_else(
        check_adm_total_booster == "Upper" | check_adm_hcw_booster == "Upper" | check_adm_60p_booster == "Upper",
        "Upper",
        "")
    )) %>%    
    mutate(check_cov_a1d_bound = if_else(
      check_cov_total_a1d == "Lower" | check_cov_hcw_a1d == "Lower" | check_cov_60p_a1d == "Lower",
      "Lower",
      if_else(
        check_cov_total_a1d == "Upper" | check_cov_hcw_a1d == "Upper" | check_cov_60p_a1d == "Upper",
        "Upper",
        "")
    )) %>%    
    mutate(check_cov_fv_bound = if_else(
      check_cov_total_fv == "Lower" | check_cov_hcw_fv == "Lower" | check_cov_60p_fv == "Lower",
      "Lower",
      if_else(
        check_cov_total_fv == "Upper" | check_cov_hcw_fv == "Upper" | check_cov_60p_fv == "Upper",
        "Upper",
        "")
    )) %>%
    mutate(check_cov_booster_bound = if_else(
      check_cov_total_booster == "Lower" | check_cov_hcw_booster == "Lower" | check_cov_60p_booster == "Lower",
      "Lower",
      if_else(
        check_cov_total_booster == "Upper" | check_cov_hcw_booster == "Upper" | check_cov_60p_booster == "Upper",
        "Upper",
        "")
    )) %>% 
    mutate(check_adm_fv_lm_bound = if_else(
      check_adm_total_fv_lm == "Lower" | check_adm_hcw_fv_lm == "Lower" | check_adm_60p_fv_lm == "Lower",
      "Lower",
      if_else(
        check_adm_total_fv_lm == "Upper" | check_adm_hcw_fv_lm == "Upper" | check_adm_60p_fv_lm == "Upper",
        "Upper",
        "")
    )) %>%
    mutate(check_cov_fv_lm_bound = if_else(
      check_cov_total_fv_lm == "Lower" | check_cov_hcw_fv_lm == "Lower" | check_cov_60p_fv_lm == "Lower",
      "Lower",
      if_else(
        check_cov_total_fv_lm == "Upper" | check_cov_hcw_fv_lm == "Upper" | check_cov_60p_fv_lm == "Upper",
        "Upper",
        "")
    ))   
    
  print(" >> Function 'data_checks' done")
  return(a_data)
}
