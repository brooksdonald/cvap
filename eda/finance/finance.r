
finance_analyse <- function(a_data) {
  print(" >> Calculating remaining doses, absolute and % pop...")
  a_data <- a_data %>%
    mutate(fund_percapita = fund_total / a_pop) %>%
    mutate(fund_percapita_cat = 
             if_else(fund_percapita < 1,
                     "1) < 1 USD",
                     if_else(fund_percapita < 2,
                             "2) 1-1.9 USD",
                             if_else(fund_percapita < 5,
                                     "3) 2-4.9 USD",
                                     if_else(fund_percapita >= 5,
                                             "4) 5+ USD",
                                             NA_character_)
                             )
                     )
             )
    )
  
  print(" >> Sorting columnds...")
  a_data <- a_data %>%
    select("a_iso", sort(colnames(.)))
  
  print(" >> Function 'finance_analyse' done")
  return(a_data)
}


finance_analyse_amc <- function(a_data) {
  print(" >> Filtering AMC countries...")
  a_data_amc <- filter(a_data, a_covax_status == "AMC" & intro_status == "Product introduced")
  
  print(" >> Function 'finance_analyse_amc' done")
  return(a_data_amc)
}


finance_analyse_hic <- function(a_data) {
  print(" >> Filtering High income countries...")
  a_data_hic <- filter(a_data, a_income_group == "HIC")
  
  print(" >> Function 'finance_analyse_hic' done")
  return(a_data_hic)
}

finance_analyse_csc <- function(a_data) {
  print(" >> Filtering Concerted support countries...")
  a_data_csc <- filter(a_data, a_csc_status == "Concerted support country")
  
  print(" >> Function 'finance_analyse_csc' done")
  return(a_data_csc)
}


finance_analyse_ifc <- function(a_data) {
  print(" >> Filtering Immediate focus countries...")
  
  print(" >> Function 'finance_analyse_ifc' done")
  a_data_ifc <- filter(a_data, a_ifc_status == "Immediate focus")
  return(a_data_ifc)
}


finance_analyse_afr <- function(a_data) {
  print(" >> Filtering African content countries...")
  a_data_africa <- filter(a_data, a_continent == "Africa" & intro_status == "Product introduced")
  
  print(" >> Function 'finance_analyse_afr' done")
  return(a_data_africa)
}


api_export_table <- function(a_data) {
  print(" >> Generating API export data...")
  print(" >> Filtering API export data...")
  api <- filter(a_data, a_who_status == "Member State" |
                  a_who_status == "Special" |
                  a_who_status == "Territory or Area")
  
  print(" >> Selecting relevant columns...")
  api <- select(
    api, c(
      "a_iso",
      "a_name_short",
      "a_who_region",
      "a_unicef_region",
      "a_income_group",
      "a_income_group_vis",
      "a_covax_status",
      "a_csc_status",
      "a_pop",
      "a_pop_female",
      "a_pop_male",
      "a_pop_18u",
      "a_pop_18p",
      "a_pop_60p",
      "a_pop_70",
      "a_pop_hcw",
      "del_date",
      "del_dose_total",
      "del_dose_since_lm",
      "del_cour_total",
      "del_cour_total_per",
      "del_cour_covax_prop",
      "pu_del_rem",
      "pu_del_rem_per",
      "pu_del_rem_timeto",
      "pu_used_per",
      "note_supplyneed",
      "expiry_risk",
      "adm_date",
      "adm_td",
      "adm_a1d",
      "adm_fv",
      "adm_fv_homo",
      "adm_pv",
      "adm_booster",
      "adm_date_gender",
      "adm_fv_female",
      "adm_fv_male",
      "adm_date_60p",
      "adm_fv_60p",
      "adm_date_hcw",
      "adm_fv_hcw",
      "dvr_4wk_td",
      "dvr_4wk_td_per",
      "dvr_4wk_td_max",
      "dvr_4wk_td_max_per",
      "dvr_4wk_fv",
      "dvr_4wk_fv_per",
      "dvr_4wk_td_lm",
      "dvr_4wk_td_change_lm_trend",
      "dvr_4wk_td_change_lm_per",
      "cov_total_a1d",
      "cov_total_fv",
      "cov_total_booster",
      "cov_total_fv_less_1m",
      "cov_total_fv_atpace_31dec",
      "cov_total_fem_fv",
      "cov_total_male_fv",
      "cov_hcw_fv",
      "cov_60p_fv",
      "t10_status",
      "t10_timeto",
      "t20_status",
      "t20_timeto",
      "t40_status",
      "t40_timeto",
      "t40_cour_req",
      "t40_cour_need_del",
      "t40_cour_need_adm",
      "t70_status",
      "t70_timeto",
      "t70_cour_req",
      "t70_cour_need_del",
      "t70_cour_need_adm",
      "ndvp_target",
      "ndvp_deadline",
      "ndvp_status",
      "ndvp_rate_needed_dose",
      "ndvp_scaleup_dose",
      "ndvp_timeto",
      "fund_total",
      "fund_percapita",
      "a_refresh_date"
    )
  )
  
  print(" >> Renaming columns...")
  colnames(api) <- c(
    "code",
    "name_short",
    "who_region",
    "unicef_region",
    "income_group",
    "income_group_vis",
    "covax_status",
    "concerted_support_status",
    "pop",
    "pop_fem",
    "pop_male",
    "pop_u18",
    "pop_18p",
    "pop_60p",
    "pop_70per",
    "pop_hcw",
    "del_date",
    "del_dose_total",
    "del_dose_since_lm",
    "del_cour_total",
    "del_cour_total_per",
    "del_cour_covax_prop",
    "pu_del_rem",
    "pu_del_rem_per",
    "pu_del_rem_timeto",
    "pu_used_per",
    "flag_supplyneed",
    "flag_expiryrisk",
    "adm_date",
    "adm_td",
    "adm_a1d",
    "adm_fv",
    "adm_fv_adjust",
    "adm_pv",
    "adm_booster",
    "adm_date_gender",
    "adm_fv_fem",
    "adm_fv_male",
    "adm_date_60p",
    "adm_fv_60p",
    "adm_date_hcw",
    "adm_fv_hcw",
    "dvr_4wk_td",
    "dvr_4wk_td_per",
    "dvr_4wk_td_max",
    "dvr_4wk_td_max_per",
    "dvr_4wk_fv",
    "dvr_4wk_fv_per",
    "dvr_4wk_td_lm",
    "dvr_4wk_td_change_lm_trend",
    "dvr_4wk_td_change_lm_per",
    "cov_total_a1d",
    "cov_total_fv",
    "cov_total_booster",
    "cov_total_fv_since_lm",
    "cov_total_fv_atpace_31dec",
    "cov_total_fem_fv",
    "cov_total_male_fv",
    "cov_hcw_fv",
    "cov_60p_fv",
    "t10_status",
    "t10_timeto",
    "t20_status",
    "t20_timeto",
    "t40_status",
    "t40_timeto",
    "t40_cour_req",
    "t40_cour_need_del",
    "t40_cour_need_adm",
    "t70_status",
    "t70_timeto",
    "t70_cour_req",
    "t70_cour_need_del",
    "t70_cour_need_adm",
    "ndvp_target",
    "ndvp_deadline",
    "ndvp_status",
    "ndvp_rate_needed",
    "ndvp_scaleup",
    "ndvp_timeto",
    "fund_total",
    "fund_percapita",
    "refresh_date"
  )
  
  api$is_current <- TRUE
  
  print(" >> Function 'api_export_table' done")
  return(api)
}
