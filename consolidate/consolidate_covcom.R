## Past coverage targets

comparisons <- function(condense_list) {
  target_list <- list("amc")
  
  column_names_value <- list("cov_hcw_", "cov_60p_")
  column_names_name <- list(
    "com_stat",
    "com_stat"
  )
  data_list <- list()
  
  for (colname in column_names_value) {
    data_list[[colname]] <- list()
  }
  
  for (target in target_list) {
    df <- condense_list[[target]]
    aggregate_columns_list <- list(
      df$cov_total_hcw_com,
      df$cov_total_60p_com
    )
    for (i in seq_len(length(column_names_value))) {
      df_value <- aggregate(df$a_name_short,
                            list(aggregate_columns_list[[i]]),
                            paste,
                            collapse = "; "
      )
      colnames(df_value) <- c(
        column_names_name[i], paste0(column_names_value[i], target
        )
      )
      data_list[[i]] <- append(data_list[[i]], list(df_value))
    }
  }
  

  e_com_hcw_all <- helper_join_dataframe_list(
    data_list[["cov_hcw_"]], join_by = "com_stat"
  )
  e_com_60p_all <- helper_join_dataframe_list(
    data_list[["cov_60p_"]], join_by = "com_stat"
  )
  return(list(
    "com_hcw" = e_com_hcw_all,
    "com_60p" = e_com_60p_all
  )
  )
}
