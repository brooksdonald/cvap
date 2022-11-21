
grouping_by_one <- function(a_data) {
    print(" >> Grouping by one column...")
  
    print(" >> Grouping by one column: Coverage fully vaccinated (% pop.)...")
    a_data <- a_data %>%
      group_by(a_covax_status) %>%
      mutate(cov_total_fv_rank = row_number(cov_total_fv)) %>%
      mutate(cov_total_fv_bins = ntile(cov_total_fv_rank, 2))
  
    print(" >> Grouping by one column: 4-week average daily vaccination rate (% pop. / day)...")
    a_data <- a_data %>%
      group_by(a_covax_status) %>%
      mutate(dvr_4wk_td_per_rank = row_number(dvr_4wk_td_per)) %>%
      mutate(dvr_4wk_td_per_bins = ntile(dvr_4wk_td_per_rank, 2))
  
    print(" >> Grouping by one column: Supply secured and/or expected (% pop.)...")
    a_data <- a_data %>%
      group_by(a_covax_status) %>%
      mutate(sec_total_per_rank = row_number(sec_total_per)) %>%
      mutate(sec_total_per_bins = ntile(sec_total_per_rank, 2))
  
    print(" >> Grouping by one column: Supply received (% pop.)...")
    a_data <- a_data %>%
      group_by(a_covax_status) %>%
      mutate(del_cour_total_per_rank = row_number(del_cour_total_per)) %>%
      mutate(del_cour_total_per_bins = ntile(del_cour_total_per_rank, 2))
  
    print(" >> Grouping by one column: ISO code...")
    a_data <- a_data %>%
      group_by(a_covax_status) %>%
      mutate(iso_rank = row_number(a_iso)) %>%
      mutate(iso_bins = ntile(iso_rank, 2))
  
    print(" >> Grouping by one column: Product utilization...")
    a_data <- a_data %>%
      group_by(a_covax_status) %>%
      mutate(pu_used_per_rank = row_number(pu_used_per)) %>%
      mutate(pu_used_per_bins = ntile(pu_used_per_rank, 2))

    print(" >> Function 'grouping_by_one' done")
    return(a_data)
}

grouping_by_two <- function(a_data) {
    print(" >> Grouping by two column...")
  
    print(" >> Grouping by two column: Short name...")
    a_data <- a_data %>%
      group_by(a_covax_status, intro_status) %>%
      mutate(iso_vx_rank = row_number(a_name_short)) %>%
      mutate(iso_vx_bins = ntile(iso_vx_rank, 4))
  
    print(" >> Grouping by two column: Proportion of supply received from COVAX...")
    a_data <- a_data %>%
      group_by(a_covax_status, intro_status) %>%
      mutate(del_cour_covax_prop_rank = row_number(del_cour_covax_prop)) %>%
      mutate(del_cour_covax_prop_bins = ntile(del_cour_covax_prop_rank, 2))
  
    print(" >> Grouping by two column: Proportion of supply delivered that remains...")
    a_data <- a_data %>%
      group_by(a_covax_status, intro_status) %>%
      mutate(rem_cour_del_prop_rank = row_number(rem_cour_del_prop)) %>%
      mutate(rem_cour_del_prop_bins = ntile(rem_cour_del_prop_rank, 2))
  
    print(" >> Grouping by two column: Supply received since last month (% pop.)...")
    a_data <- a_data %>%
      group_by(a_covax_status, intro_status) %>%
      mutate(del_cour_since_lm_per_rank = row_number(del_cour_since_lm_per)) %>%
      mutate(del_cour_since_lm_per_bins = ntile(del_cour_since_lm_per_rank, 2))
  
    print(" >> Grouping by two column: Proportion of coverage achieved in past month...")
    a_data <- a_data %>%
      group_by(a_covax_status, intro_status) %>%
      mutate(cov_total_fv_less_1m_rank = row_number(cov_total_fv_less_1m_prop)) %>% #nolint
      mutate(cov_total_fv_less_1m_bins = ntile(cov_total_fv_less_1m_rank, 2))
  
    print(" >> Grouping by two column: Proportion of secured courses that have been received...")
    a_data <- a_data %>%
      group_by(a_covax_status, intro_status) %>%
      mutate(sec_del_prop_rank = row_number(sec_del_prop)) %>%
      mutate(sec_del_prop_bins = ntile(sec_del_prop_rank, 2))
    
    print(" >> Grouping by two column: Financing per capita...")
    a_data <- a_data %>%
      group_by(a_covax_status, intro_status) %>%
      mutate(fund_percapita_rank = row_number(fund_percapita)) %>%
      mutate(fund_percapita_bins = ntile(fund_percapita_rank, 2))
    
    print(" >> Grouping by two column: HCW coverage...")
    a_data <- a_data %>%
      group_by(a_covax_status, intro_status) %>%
      mutate(cov_hcw_fv_rank = row_number(cov_hcw_fv)) %>%
      mutate(cov_hcw_fv_bins = ntile(cov_hcw_fv_rank, 2))
    
    print(" >> Grouping by two column: 60+ coverage...")
    a_data <- a_data %>%
      group_by(a_covax_status, intro_status) %>%
      mutate(cov_60p_fv_rank = row_number(cov_60p_fv)) %>%
      mutate(cov_60p_fv_bins = ntile(cov_60p_fv_rank, 2))
    
    print(" >> Grouping by two column: Gender coverage...")
    a_data <- a_data %>%
      group_by(a_covax_status, intro_status) %>%
      mutate(cov_total_fem_fv_rank = row_number(cov_total_fem_fv)) %>%
      mutate(cov_total_fem_fv_bins = ntile(cov_total_fem_fv_rank, 2))
    
    print(" >> Grouping by two column: Gender coverage repstat...")
    a_data <- a_data %>%
      group_by(adm_fv_gen_repstat, intro_status) %>%
      mutate(cov_total_gen_diff_rank = row_number(cov_total_gen_diff)) %>%
      mutate(cov_total_gen_diff_bins = ntile(cov_total_gen_diff_rank, 2))
  
    print(" >> Grouping by two column: Country coverage target...")
    a_data <- a_data %>%
      group_by(a_covax_status, intro_status) %>%
      mutate(ndvp_target_rank = row_number(ndvp_target)) %>%
      mutate(ndvp_target_bins = ntile(ndvp_target_rank, 2))
    
    print(" >> Grouping by two column: CSC total courses received...")
    a_data <- a_data %>%
      group_by(intro_status, a_csc_status) %>%
      mutate(csc_del_dose_rank = row_number(del_dose_total)) %>%
      mutate(csc_del_dose_bins = ntile(csc_del_dose_rank, 2))
  
    print(" >> Grouping by two column: Proportion of supply secured from COVAX...")
    a_data <- a_data %>%
      group_by(a_covax_status, intro_status) %>%
      mutate(sec_covax_prop_rank = row_number(sec_covax_prop)) %>%
      mutate(sec_covax_prop_bins = ntile(sec_covax_prop_rank, 2))
  
    print(" >> Grouping by two column: Proportion of JJ doses...")
    a_data <- a_data %>%
      group_by(intro_status, a_csc_status) %>%
      mutate(csc_del_dose_jj_rank = row_number(del_dose_jj_prop)) %>%
      mutate(csc_del_dose_jj_bins = ntile(csc_del_dose_jj_rank, 2))  
    
    print(" >> Function 'grouping_by_two' done")
    return(a_data)
}

grouping_by_three <- function(a_data) {
    print(" >> Grouping by three column: Booster per pop...")
    a_data <- a_data %>%
      group_by(a_covax_status, intro_status, booster_status) %>%
      mutate(booster_per_rank = row_number(cov_total_fv)) %>%
      mutate(booster_per_bins = ntile(booster_per_rank, 2))
    
    a_data <- a_data %>%
      group_by(a_covax_status, intro_status, booster_status) %>%
      mutate(booster_rank = row_number(cov_total_booster)) %>%
      mutate(booster_bins = ntile(booster_rank, 2)) %>%
      data.frame()
    
    print(" >> Function 'grouping_by_three' done")
    return(a_data)
}
