grouping_by_one <- function(a_data) {
  print(">> Ranking & binning for total cps coverage...")
  # Coverage fully vaccinated (% pop.)
  a_data <- a_data %>%
    group_by(a_status_covax) %>%
    mutate(cov_total_fv_rank = row_number(cov_total_fv),
           cov_total_fv_bins = ntile(cov_total_fv_rank, 2))

    return(a_data)

}

grouping_by_two <- function(a_data) {
  print(">>  Ranking & binning for AMC hcw cps coverage...")
  a_data <- a_data %>%
    group_by(a_status_covax, adm_status_intro) %>%
    mutate(cov_hcw_fv_rank = row_number(cov_hcw_fv),
           cov_hcw_fv_bins = ntile(cov_hcw_fv_rank, 2))
  
  print(">> Ranking / binning for AMC older adult cps coverage...")
  a_data <- a_data %>%
    group_by(a_status_covax, adm_status_intro) %>%
    mutate(cov_60p_fv_rank = row_number(cov_60p_fv),
           cov_60p_fv_bins = ntile(cov_60p_fv_rank, 2))

  return(a_data)
}