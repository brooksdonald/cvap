grouping_by_one <- function(a_data) {

  # Coverage fully vaccinated (% pop.)
  a_data <- a_data %>%
    group_by(a_covax_status) %>%
    
    mutate(cov_total_fv_rank = row_number(cov_total_fv)) %>%
    
    mutate(cov_total_fv_bins = ntile(cov_total_fv_rank, 2))


  ##4-week average daily vaccination rate (% pop. / day)
  a_data <- a_data %>%
    group_by(a_covax_status) %>%
    
    mutate(dvr_4wk_td_per_rank = row_number(dvr_4wk_td_per)) %>%
    
    mutate(dvr_4wk_td_per_bins = ntile(dvr_4wk_td_per_rank, 2))


  ##Supply secured and/or expected (% pop.)
  a_data <- a_data %>%
    group_by(a_covax_status) %>%
    
    mutate(sec_total_per_rank = row_number(sec_total_per)) %>%
    
    mutate(sec_total_per_bins = ntile(sec_total_per_rank, 2))


  ##Supply received (% pop.)
  a_data <- a_data %>%
    group_by(a_covax_status) %>%
    
    mutate(del_cour_total_per_rank = row_number(del_cour_total_per)) %>%
    
    mutate(del_cour_total_per_bins = ntile(del_cour_total_per_rank, 2))


  ##ISO code
  a_data <- a_data %>%
    group_by(a_covax_status) %>%
    
    mutate(iso_rank = row_number(a_iso)) %>%
    
    mutate(iso_bins = ntile(iso_rank, 2))


  ##Product utilization
  a_data <- a_data %>%
    group_by(a_covax_status) %>%
    
    mutate(pu_used_per_rank = row_number(pu_used_per)) %>%
    
    mutate(pu_used_per_bins = ntile(pu_used_per_rank, 2))

    return(a_data)

}

grouping_by_two <- function(a_data) {
  ##Short name
  a_data <- a_data %>%
    group_by(a_covax_status, intro_status) %>%
    
    mutate(iso_vx_rank = row_number(a_name_short)) %>%
    
    mutate(iso_vx_bins = ntile(iso_vx_rank, 4))


  ##Proportion of supply received from COVAX
  a_data <- a_data %>%
    group_by(a_covax_status, intro_status) %>%
    
    mutate(del_cour_covax_prop_rank = row_number(del_cour_covax_prop)) %>%
    
    mutate(del_cour_covax_prop_bins = ntile(del_cour_covax_prop_rank, 2))


  ##Proportion of supply delivered that remains
  a_data <- a_data %>%
    group_by(a_covax_status, intro_status) %>%
    
    mutate(rem_cour_del_prop_rank = row_number(rem_cour_del_prop)) %>%
    
    mutate(rem_cour_del_prop_bins = ntile(rem_cour_del_prop_rank, 2))


  ##Supply received since last month (% pop.)
  a_data <- a_data %>%
    group_by(a_covax_status, intro_status) %>%
    
    mutate(del_cour_since_lm_per_rank = row_number(del_cour_since_lm_per)) %>%
    
    mutate(del_cour_since_lm_per_bins = ntile(del_cour_since_lm_per_rank, 2))


  ##Proportion of coverage achieved in past month
  a_data <- a_data %>%
    group_by(a_covax_status, intro_status) %>%
    
    mutate(cov_total_fv_less_1m_rank = row_number(cov_total_fv_less_1m_prop)) %>%
    
    mutate(cov_total_fv_less_1m_bins = ntile(cov_total_fv_less_1m_rank, 2))


  ##Proportion of secured courses that have been received
  a_data <- a_data %>%
    group_by(a_covax_status, intro_status) %>%
    
    mutate(sec_del_prop_rank = row_number(sec_del_prop)) %>%
    
    mutate(sec_del_prop_bins = ntile(sec_del_prop_rank, 2))


  ##Proportion of supply secured from COVAX
  a_data <- a_data %>%
    group_by(a_covax_status, intro_status) %>%
    
    mutate(sec_covax_prop_rank = row_number(sec_covax_prop)) %>%
    
    mutate(sec_covax_prop_bins = ntile(sec_covax_prop_rank, 2)) %>%
    
    data.frame()

    return(a_data)
}
