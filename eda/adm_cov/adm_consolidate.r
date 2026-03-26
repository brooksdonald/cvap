
extract_vxrate_details <- function(c_vxrate_latest) {
  print(" >> Remove duplicative base details from latest vxrate summary...")
  c_vxrate_latest_red <-
    select(
      c_vxrate_latest, -c(
        "a_continent",
        "a_region_who",
        "a_income_group",
        "a_status_covax",
        "a_region_unicef",
        "a_name_short",
        "a_name_long",
        "a_region_sub_who",
        "a_status_who",
        "a_status_csc",
        "a_status_gavi",
        "a_continent_sub",
        "pol_jj",
        "pol_old",
        "pol_old_source",
        "a_pop",
        "ss_target",
        "ss_deadline",
        "country_source",
        "date_13jan.x",
        "adm_tar_hcw_wpro",
        "a_pop_hcw",
        "a_pop",
        "pol_boost",
        "ri_dtp1",
        "ri_dtp3",
        "ri_mcv1",
        "ri_mcv2",
        "ri_zero_dose",
        "a_pop_comorb_increased_prop",
        "a_pop_comorb_high_prop",
        "a_pop_comorb_high_young_prop",
        "a_pop_comorb_high_older_prop",
        "min_vx_rollout_date"
      )
    )

    return(c_vxrate_latest_red)
}

merge_dataframes <- function(
  entity_characteristics,
  c_vxrate_latest_red,
  population,
  uptake_gender_data,
  who_dashboard,
  sup_rec,
  b_dp,
  sup_rec_jj,
  fin_del_sum,
  population_pin
  ) {
    # Renaming iso columns to a_iso before merge
    df_list <- list(
      entity_characteristics,
      c_vxrate_latest_red,
      population,
      uptake_gender_data,
      who_dashboard,
      sup_rec,
      b_dp,
      sup_rec_jj,
      fin_del_sum,
      population_pin
    )
    # Merge details
    a_data <- helper_join_dataframe_list(
      df_list,
      join_by = "a_iso"
    )
  return(as.data.frame(a_data))
}

transform_vxrate_merge <- function(a_data, date_refresh) {
  print(" >>> Setting static dates")
  a_data <- a_data %>%
    
    # Set static dates
    mutate(a_date_refresh = date_refresh,
           
           adm_target_hcw = case_when(
             is.na(adm_tar_hcw_wpro) ~ adm_target_hcw,
             TRUE ~ adm_tar_hcw_wpro))
  
  #Calculate JJ proportion
  print(" >>> Computing JJ doses KPIs")
  a_data <- a_data %>%
      mutate(del_dose_minjj = del_dose_total  - del_dose_jj) %>% 
      mutate(del_dose_jj_prop = if_else(
        is.na(del_dose_jj),
        0,
        del_dose_jj / del_dose_total))

  # Calculate introduction status
  print(" >>> Computing introduction status...")
  a_data <- a_data %>%
    mutate(adm_status_intro = if_else(
      is.na(adm_tot_td) | adm_tot_td == 0,
      "No product introduced",
      "Product introduced"
    )
  )

  print(">>> Calculating total population percentages and proportions...")
  a_data <- a_data %>%
    mutate(a_pop_10 = a_pop * 0.1,
           a_pop_20 = a_pop * 0.2,
           a_pop_40 = a_pop * 0.4,
           a_pop_70 = a_pop * 0.7,
           a_pop_18p_prop = a_pop_18p / a_pop_2021,
           a_pop_18u_prop = a_pop_18u / a_pop_2021,
           a_pop_hcw_prop = a_pop_hcw / a_pop_2021,
           a_pop_60p_prop = a_pop_60p / a_pop_2021,
           a_pop_12p_prop = a_pop_12p / a_pop_2021,
           a_pop_12u_prop = a_pop_12u / a_pop_2021)
  
  print(">>> Assigning older adult population based on policy...")
  a_data <- a_data %>%
    mutate(
      a_pop_old = case_when(
        pol_old == "45 and older" ~ a_pop_45p,
        pol_old == "50 and older" ~ a_pop_50p,
        pol_old == "55 and older" ~ a_pop_55p,
        pol_old == "60 and older" ~ a_pop_60p,
        pol_old == "65 and older" ~ a_pop_65p,
        pol_old == "70 and older" ~ a_pop_70p,
        pol_old == "75 and older" ~ a_pop_75p,
        TRUE ~ a_pop_60p))

  # Calculate theoretical fully vaccinated for non-reporters for current, lm, and 2m
  print(" >>> Computing theoretically fully vaxxed for non reporters...")
  a_data <- a_data %>%
    mutate(
      adm_tot_cps_homo = case_when(
        adm_tot_a1d == 0 & adm_tot_cps == 0 & adm_tot_boost == 0 ~ adm_tot_td / 2,
        adm_tot_a1d == 0 & adm_tot_cps == 0 & adm_tot_boost != 0 ~ (adm_tot_td - adm_tot_boost) / 2,
        adm_tot_a1d != 0 & adm_tot_cps == 0 & adm_tot_boost == 0 ~ adm_tot_td - adm_tot_a1d,
        adm_tot_a1d != 0 & adm_tot_cps == 0 & adm_tot_boost != 0 ~ adm_tot_td - adm_tot_a1d - adm_tot_boost,
        TRUE ~ adm_tot_cps),
      
      adm_tot_cps_13jan_homo = case_when(
        adm_tot_a1d_13jan == 0 & adm_tot_cps_13jan == 0 & adm_tot_boost_13jan == 0 ~ adm_tot_td_13jan / 2,
        adm_tot_a1d_13jan == 0 & adm_tot_cps_13jan == 0 & adm_tot_boost_13jan != 0 ~ (adm_tot_td_13jan - adm_tot_boost_13jan)/ 2,
        adm_tot_a1d_13jan != 0 & adm_tot_cps_13jan == 0 & adm_tot_boost_13jan == 0 ~ adm_tot_td_13jan - adm_tot_a1d_13jan,
        adm_tot_a1d_13jan != 0 & adm_tot_cps_13jan == 0 & adm_tot_boost_13jan != 0 ~ adm_tot_td_13jan - adm_tot_a1d_13jan - adm_tot_boost_13jan,
        TRUE ~ adm_tot_cps_13jan),
      
      adm_tot_a1d_homo = case_when(
        adm_tot_a1d == 0 & adm_tot_cps == 0 ~ adm_tot_td / 2,
        adm_tot_a1d < adm_tot_cps_homo ~ adm_tot_cps_homo,
        TRUE ~ adm_tot_a1d),
      
      adm_tot_td_per = adm_tot_td / a_pop,
      adm_tot_pv = pmax(0, adm_tot_a1d_homo - adm_tot_cps_homo),
      adm_tot_boost_homo = pmin(adm_tot_boost, adm_tot_cps_homo, a_pop))

  # Calculate adm_tot_a1d and adm_tot_cps coverage for current, lm, and 2m, including change
  print(" >>> Computing adm_tot_a1d and adm_tot_cps coverage...")
  a_data <- a_data %>%
    mutate(
      cov_tot_a1d = adm_tot_a1d / a_pop,
      cov_tot_a1d_adjust = case_when(
        adm_tot_a1d <= adm_tot_cps ~ NA_real_,
        TRUE ~ (adm_tot_a1d / a_pop)),
      
      cov_tot_a1d_13jan = adm_tot_a1d_13jan / a_pop,
      cov_tot_cps = pmin(1, adm_tot_cps_homo / a_pop),
      cov_tot_cps_theo = (adm_tot_td / 2) / a_pop,
      cov_tot_cps_13jan = adm_tot_cps_13jan_homo / a_pop,
      cov_tot_cps_cur_13jan = pmax(0, cov_tot_cps - cov_tot_cps_13jan))
  
  # Correct GRL and SJM
  a_data <- a_data %>%
    mutate(
      cov_tot_cps = case_when(
        a_iso == "GRL" ~ cov_tot_cps[a_iso == "DNK"],
        a_iso == "SJM" ~ cov_tot_cps[a_iso == "NOR"],
        TRUE ~ cov_tot_cps))

  # Indicator reporting status for target group-specific uptake data
  print(" >>> Indicator reporting status for target group-specific uptake data...")
  a_data <- a_data %>%
    mutate(
      adm_hcw_cps_repstat = case_when(
        is.na(adm_cps_hcw) ~ "Not reporting",
        adm_cps_hcw > 0 ~ "Reporting",
        TRUE ~ "Not reporting"),
      
      adm_old_cps_repstat = case_when(
        is.na(adm_cps_60p) ~ "Not reporting",
        adm_cps_60p > 0 ~ "Reporting",
        TRUE ~ "Not reporting")) 

  # Converting Ingested data from API to numeric values
  a_data$adm_a1d_hcw <- as.numeric(a_data$adm_a1d_hcw)
  a_data$adm_cps_hcw <- as.numeric(a_data$adm_cps_hcw)
  
  # Correct GRL and SJM
  a_data <- a_data %>%
    mutate(
      adm_hcw_cps_repstat = case_when(
        a_iso == "GRL" ~ adm_hcw_cps_repstat[a_data$a_iso == "DNK"],
        a_iso == "SJM" ~ adm_hcw_cps_repstat[a_data$a_iso == "NOR"],
        TRUE ~ adm_hcw_cps_repstat),
      
      adm_old_cps_repstat = case_when(
        a_iso == "GRL" ~ adm_old_cps_repstat[a_data$a_iso == "DNK"],
        a_iso == "SJM" ~ adm_old_cps_repstat[a_data$a_iso == "NOR"],
        TRUE ~ adm_old_cps_repstat))
  
  # Healthcare worker
  a_data <- a_data %>%
    mutate(hcw_flag = if_else(
      a_pop_hcw > adm_target_hcw,
      "Yes",
      NA_character_)) %>%
    mutate(hcw_diff = pmax(a_pop_hcw - adm_target_hcw, 0, na.rm = TRUE))

  # Calculate target group coverage figures
  print(" >>> Computing target group coverage figures...")

  # Calculate health are care worker coverage figures
  a_data <- a_data %>%
    mutate(adm_hcw_cps_homo = pmin(adm_cps_hcw, a_pop_hcw),
           adm_hcw_cps_adjust = pmin(adm_cps_hcw + (hcw_diff * cov_tot_cps), a_pop_hcw),
           
           adm_hcw_a1d_homo = case_when(
             pmin(adm_a1d_hcw, a_pop_hcw) < adm_hcw_cps_adjust ~ adm_hcw_cps_adjust,
             TRUE ~ pmin(adm_a1d_hcw, a_pop_hcw)),
           
           adm_hcw_boost_homo = pmin(a_pop_hcw, adm_boost_hcw, adm_hcw_cps_adjust),
           
           cov_hcw_a1d = case_when(
             is.na(hcw_flag) ~ pmin(adm_hcw_a1d_homo / a_pop_hcw, 1),
             TRUE ~ pmin((adm_hcw_a1d_homo + (hcw_diff * cov_tot_a1d)) / a_pop_hcw, 1)),
           
           cov_hcw_a1d_adjust = case_when(
             adm_a1d_hcw <= adm_cps_hcw ~ NA_real_,
             TRUE ~ cov_hcw_a1d),
           
           cov_hcw_cps = pmin(adm_hcw_cps_adjust / a_pop_hcw, 1)) %>%
    
    mutate(cov_hcw_boost = pmin(adm_hcw_boost_homo / a_pop_hcw, 1))

  # Calculate older adult coverage figures
  a_data <- a_data %>%
    mutate(
      adm_old_cps_homo = pmin(a_pop_old, adm_cps_60p),
      adm_old_a1d_homo = case_when(
        pmin(a_pop_old, adm_a1d_60p) < adm_old_cps_homo ~ adm_old_cps_homo,
        TRUE ~ pmin(a_pop_old, adm_a1d_60p)),
      
      adm_old_boost_homo = pmin(adm_boost_60p, a_pop_old),
      
      cov_old_a1d = pmin(adm_old_a1d_homo / a_pop_old, 1),
      cov_old_a1d_adjust = case_when(
        adm_a1d_60p <= adm_cps_60p ~ NA_real_,
        TRUE ~ cov_old_a1d),
      
      cov_old_cps = pmin(adm_old_cps_homo / a_pop_old, 1),
      cov_old_boost = pmin(adm_old_boost_homo / a_pop_old, 1),
      cov_old_a1d_cps = case_when(
        cov_old_cps == 0 | is.na(cov_old_cps) ~ cov_old_a1d,
        TRUE ~ cov_old_a1d - cov_old_cps),
      
      cov_old_cps_boost = case_when(
        cov_old_boost == 0 | is.na(cov_old_boost) ~ cov_old_cps,
        TRUE ~ cov_old_cps - cov_old_boost))

  a_data <- a_data %>%
    mutate(
      cov_hcw_cps = case_when(
        a_iso == "GRL" ~ cov_hcw_cps[a_data$a_iso == "DNK"],
        a_iso == "SJM" ~ cov_hcw_cps[a_data$a_iso == "NOR"],
        TRUE ~ cov_hcw_cps),

      cov_old_cps = case_when(
        a_iso == "GRL" ~ cov_old_cps[a_data$a_iso == "DNK"],
        a_iso == "SJM" ~ cov_old_cps[a_data$a_iso == "NOR"],
        TRUE ~ cov_hcw_cps))

  # Calculate 4-week average daily rates as % of pop.
  print(" >>> Computing 4-week average daily rates as % of pop...")
  a_data <- a_data %>%
    mutate(dvr_4wk_fv = pmax(0, dvr_4wk_fv),
           dvr_4wk_td_per = dvr_4wk_td / a_pop,
           dvr_4wk_fv_per = dvr_4wk_fv / a_pop,
           dvr_4wk_td_max_per = dvr_4wk_td_max / a_pop)

  # Calculate coverage difference between HCWs and total in reporting countries
  print(" >>> Computing coverage difference between HCWs and total in reporting countries...")
  a_data <- a_data %>%
    mutate(
      cov_total_hcw_diff = case_when(
        adm_hcw_cps_repstat == "Reporting" ~ cov_hcw_cps - cov_tot_cps,
        TRUE ~ NA))
  
  # Calculate coverage difference between 60 plus and total in reporting countries
  print(" >>> Computing coverage difference between HCWs and total in reporting countries...")
  a_data <- a_data %>%
    mutate(
      cov_total_60p_diff = ifelse(
        adm_old_cps_repstat == "Reporting",
        cov_old_cps - cov_tot_cps,
        NA
      ))
  
  a_data <- a_data %>%
    mutate(adm_tot_td_adj  = adm_tot_td / a_pop)


  datalist <- list("a_data" = a_data)
  return(datalist)
}