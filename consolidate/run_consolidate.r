
run_consolidate <- function(a_data, a_data_amc, a_data_africa, a_data_csc, 
                            a_data_ifc, b_vxrate_change_lw, refresh_date) {
  source("consolidate/consolidate_filter.r")
  source("consolidate/consolidate_adm.r")
  source("consolidate/consolidate_covcom.r")
  source("consolidate/consolidate_covtar.r")
  source("consolidate/consolidate_values.r")
  print(" > Starting local environment for data consolidation module...")
  
  print(" > Consolidating base file...")
  condense_list <- load_base_condense_file(a_data)
  print(" > Done.")
  
  print(" > Consolidating Vaccination rate...")
  vrcat_list <- vxrate(condense_list)
  print(" > Done.")
  
  print(" > Consolidating targets...")
  tgt_list <- targets(condense_list)
  print(" > Done.")
  
  print(" > Consolidating comparisons...")
  com_list <- comparisons(condense_list)
  print(" > Done.")
  
  print(" > Creating values table...")
  z_values <- values_table(a_data, a_data_amc, a_data_africa,
                           a_data_csc, a_data_ifc, refresh_date)
  print(" > Done.")
  
  print(" > Loading eda consolidation data back to global environment...")
  e_vrcat_all <- vrcat_list[["all"]]
  e_cov_com_hcw_all <- com_list[["com_hcw_amc"]]
  e_cov_com_60p_all <- com_list[["com_60p_amc"]]
  e_cov_com_hcw_csc <- com_list[["com_hcw_csc"]]
  e_cov_com_60p_csc <- com_list[["com_60p_csc"]]
  e_ndvp_all <- tgt_list[["ndvp"]]
  
  print(" > Returning to local environment.")
  return(environment())
}
