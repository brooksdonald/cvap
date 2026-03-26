grouping_by_one <- function(a_data) {
  print(">> Ranking & binning for total cps coverage...")
  # Coverage fully vaccinated (% pop.)
  a_data <- a_data %>%
    group_by(a_status_covax) %>%
    mutate(cov_tot_cps_rank = row_number(cov_tot_cps),
           cov_tot_cps_bins = ntile(cov_tot_cps_rank, 2))

    return(a_data)

}

grouping_by_two <- function(a_data) {
  print(">>  Ranking & binning for AMC hcw cps coverage...")
  a_data <- a_data %>%
    group_by(a_status_covax, adm_status_intro) %>%
    mutate(cov_hcw_cps_rank = row_number(cov_hcw_cps),
           cov_hcw_cps_bins = ntile(cov_hcw_cps_rank, 2))
  
  print(">> Ranking / binning for AMC older adult cps coverage...")
  a_data <- a_data %>%
    group_by(a_status_covax, adm_status_intro) %>%
    mutate(cov_old_cps_rank = row_number(cov_old_cps),
           cov_old_cps_bins = ntile(cov_old_cps_rank, 2))

  return(a_data)
}