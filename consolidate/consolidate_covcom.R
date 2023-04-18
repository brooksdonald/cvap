
comparisons <- function(condense_list) {
  name_list <- list("amc", "csc")
  
  col_names_value <- list("cov_hcw_amc", "cov_60p_amc", "cov_hcw_csc", "cov_60p_csc")
  col_names_name <- list("com_hcw_amc_stat_", "com_60p_amc_stat_", "com_hcw_csc_stat_", "com_60p_csc_stat_")
  
  data_list <- list()
  
  for (colname in col_names_value) {
    data_list[[colname]] <- list()
  }
  
  for (name in name_list) {
    df <- condense_list[[name]]
    aggregate_col_list <- list(
      df$cov_total_hcw_com,
      df$cov_total_60p_com,
      df$cov_total_hcw_com_csc,
      df$cov_total_60p_com_csc
    )
    for (i in seq_len(length(col_names_value))) {
      df_value <- aggregate(df$a_name_short,
                            list(aggregate_col_list[[i]]),
                            paste,
                            collapse = "; "
      )
      colnames(df_value) <- c(
        col_names_value[[i]], paste0(col_names_name[[i]], name)
      )
      data_list[[col_names_name[[i]]]] <- append(data_list[[col_names_name[[i]]]], list(df_value))
    }
  }
  
  # Comparison Data: AMC Countries | HCWs
  e_com_hcw_amc <- helper_join_dataframe_list(
    data_list[["com_hcw_amc_stat_"]], join_by = "cov_hcw_amc"
  )
  
  colnames(e_com_hcw_amc) <- c("com_stat", "cov_hcw_amc", "cov_hcw_csc")
  e_com_hcw_amc <- select(e_com_hcw_amc, -c("cov_hcw_csc"))
  
  # Comparison Data: AMC Countries | 60 plus
  e_com_60p_amc <- helper_join_dataframe_list(
    data_list[["com_60p_amc_stat_"]], join_by = "cov_60p_amc"
  )
  
  colnames(e_com_60p_amc) <- c("com_stat", "cov_60p_amc", "cov_60p_csc")
  e_com_60p_amc <- select(e_com_60p_amc, -c("cov_60p_csc"))
  
  # Comparison Data: CSC Countries | HCWs
  e_com_hcw_csc <- helper_join_dataframe_list(
    data_list[["com_hcw_csc_stat_"]], join_by = "cov_hcw_csc"
  )
  
  colnames(e_com_hcw_csc) <- c("com_stat", "cov_hcw_amc", "cov_hcw_csc")
  e_com_hcw_csc <- select(e_com_hcw_csc, -c("cov_hcw_amc"))
  
  # Comparison Data: CSC Countries | 60 plus
  e_com_60p_csc <- helper_join_dataframe_list(
    data_list[["com_60p_csc_stat_"]], join_by = "cov_60p_csc"
  )
  
  colnames(e_com_60p_csc) <- c("com_stat", "cov_60p_amc", "cov_60p_csc")
  e_com_60p_csc <- select(e_com_60p_csc, -c("cov_60p_amc"))
  
  print(" >> Function 'comparisons' done")
  return(
    list("com_hcw_amc" = e_com_hcw_amc,
         "com_60p_amc" = e_com_60p_amc,
         "com_hcw_csc" = e_com_hcw_csc,
         "com_60p_csc" = e_com_60p_csc
    )
  )
}
