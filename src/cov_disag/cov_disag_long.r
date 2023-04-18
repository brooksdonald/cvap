
create_hrg_timeseries <- function(headers, refresh_api) {
  print(" >> Loading long time series data for HRG...")
  targetgroup_api_data <- helper_wiise_api(
    "https://frontdoor-l4uikgap6gz3m.azurefd.net/WIISE/MT_COV_UPTAKE_TARGETGROUP_LONG",
    headers = FALSE, refresh_api)
  
  base_target <- as.data.frame(targetgroup_api_data)
  print(" >> Done.")
  
  print(" >> Filtering for healthcare workers target group...")
  target_hcw <- filter(base_target, TARGET_GROUP == "HW")
  
  print(" >> Generating variable for countries reporting on healthcare workers...")
  target_hcw <- target_hcw %>%
    mutate(hw_repstat = if_else(
      is.na(N_VACC_LAST_DOSE) == FALSE & N_VACC_LAST_DOSE != 0,
      "Reporting",
      NA_character_
    )
    )
  
  print(" >> Formatting date variable...")
  target_hcw$DATE <-
    as.Date(paste0(as.character(target_hcw$DATE), '-01'), format = '%Y-%m-%d')
  
  print(" >> Filtering for countries reporting on healthcare workers...")
  target_hcw <- filter(target_hcw, hw_repstat == "Reporting")
  
  print(" >> Calculating maximum entry per ISO and DATE for doses...")
  target_hcw <- target_hcw %>%
    group_by(ISO_3_CODE, DATE) %>%
    top_n(1, N_VACC_LAST_DOSE)
  
  print(" >> Selecting relevant columns...")
  target_hcw <- target_hcw %>%
    select(ISO_3_CODE,
           DATE,
           N_VACC_DOSE1,
           N_VACC_LAST_DOSE,
           N_VACC_BOOSTER_DOSE
    )
  
  print(" >> Filtering for older adults target group...")
  target_old <- filter(base_target, TARGET_GROUP == "OLDER_60")
  
  print(" >> Generating variable for countries reporting on older adults...")
  target_old <- target_old %>%
    mutate(old_repstat = if_else(
      is.na(N_VACC_LAST_DOSE) == FALSE & N_VACC_LAST_DOSE != 0,
      "Reporting",
      NA_character_
    )
    )
  
  print(" >> Formatting date variable...")
  target_old$DATE <-
    as.Date(paste0(as.character(target_old$DATE), '-01'), format = '%Y-%m-%d')
  
  print(" >> Filtering for countries reporting on older adults...")
  target_old <- filter(target_old, old_repstat == "Reporting")
  
  print(" >> Calculating maximum entry per ISO and DATE for doses...")
  target_old <- target_old %>%
    group_by(ISO_3_CODE, DATE) %>%
    top_n(1, N_VACC_LAST_DOSE)
  
  print(" >> Selecting relevant columns...")
  target_old <- target_old %>%
    select(ISO_3_CODE,
           DATE,
           N_VACC_DOSE1,
           N_VACC_LAST_DOSE,
           N_VACC_BOOSTER_DOSE
    )
  
  print(" >> Renaming columns...")
  colnames(target_old) <- c(
    "ISO_3_CODE",
    "DATE",
    "N_VACC_DOSE1_old",
    "N_VACC_LAST_DOSE_old",
    "N_VACC_BOOSTER_DOSE_old"
  )
  
  print(" >> Joining healthcare workers and older adults target group data...")
  target_hcwold <- full_join(target_hcw, 
                             target_old, 
                             by = c("ISO_3_CODE" = "ISO_3_CODE","DATE" = "DATE"))
  
  target_hcwold <- target_hcwold %>%
    mutate(adm_date_month = if_else(year(DATE) == 2022, 
                                    as.numeric(month(DATE) + 12),
                                    as.numeric(month(DATE))))
  print(" > Done.")
  
  print(" >> Function 'create_hrg_timeseries' done")  
  return(target_hcwold)
}
