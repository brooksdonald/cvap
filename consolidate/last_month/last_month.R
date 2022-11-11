
load_last_month_data <- function() {
    print(" >> Load last month output data...")
  
  base_data_lm <- data.frame(
    read_excel(
      "data/input/output_master_lm.xlsx",
      sheet = "0_base_data")
  )
      
    return(base_data_lm)
}

transform_last_month_data <- function(base_data_lm) {
  base_data_current <- rank_bin_env$a_data
  base_data_current <- select(
    base_data_current,
    c(
      "a_iso",
      "adm_a1d_homo",
      "adm_fv_homo",
      "adm_booster_homo",
      "adm_td",
      "cov_total_fv",
      "cov_total_a1d",
      "adm_fv_hcw_repstat",
      "adm_fv_hcw_adjust",
      "cov_hcw_fv",
      "adm_a1d_hcw_homo",
      "adm_fv_60p_repstat",
      "adm_fv_60p_homo",
      "cov_60p_fv",
      "adm_a1d_60p_homo",
      "a_pop_hcw",
      "a_pop_60p",
      "a_pop_older",
      "a_pop",
      "dvr_4wk_td_per",
      "fund_percapita",
      "del_cour_total_per"
    )
  )
  
  
  base_data_lm <- select(
    base_data_lm,
    c(
      "a_iso",
      "adm_a1d_homo",
      "adm_fv_homo",
      "adm_booster",
      "adm_td",
      "cov_total_fv",
      "cov_total_a1d",
      "adm_fv_hcw_repstat",
      "adm_fv_hcw_adjust",
      "cov_hcw_fv",
      "adm_a1d_hcw_homo",
      "adm_fv_60p_repstat",
      "adm_fv_60p_homo",
      "cov_60p_fv",
      "adm_a1d_60p_homo",
      "a_pop_hcw",
      "a_pop_60p",
      "a_pop_older",
      "a_pop",
      "dvr_4wk_td_per",
      "fund_percapita",
      "del_cour_total_per"
      )
    )
    
  colnames(base_data_lm) <- c(
    "a_iso",
    "adm_a1d_homo_lm",
    "adm_fv_homo_lm",
    "adm_booster_homo_lm",
    "adm_td_lm",
    "cov_total_fv_lm",
    "cov_total_a1d_lm",
    "adm_fv_hcw_repstat_lm",
    "adm_fv_hcw_adjust_lm",
    "cov_hcw_fv_lm",
    "adm_a1d_hcw_homo_lm",
    "adm_fv_60p_repstat_lm",
    "adm_fv_60p_homo_lm",
    "cov_60p_fv_lm",
    "adm_a1d_60p_homo_lm",
    "a_pop_hcw_lm",
    "a_pop_60p_lm",
    "a_pop_older_lm",
    "a_pop_lm",
    "dvr_4wk_td_per_lm",
    "fund_percapita_lm",
    "del_cour_total_per_lm"
  )
  
  base_data_lm_change <- left_join(base_data_current, base_data_lm, by = c("a_iso" = "a_iso"))
                                   
  return(base_data_lm_change)
}


