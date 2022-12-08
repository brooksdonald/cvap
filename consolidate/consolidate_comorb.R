## Past coverage targets

comparisons_comorb <- function(condense_list) {
  target_list <- list("amc")
  
  column_names_value <- list("cov_")
  column_names_name <- list(
    "com_stat"
  )
  data_list <- list()
  
  for (colname in column_names_value) {
    data_list[[colname]] <- list()
  }
  
  for (target in target_list) {
    df <- condense_list[[target]]
    aggregate_columns_list <- list(
      df$cov_total_comorb_com
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
  
  
  e_com_comorb_all <- helper_join_dataframe_list(
    data_list[["cov_"]], join_by = "com_stat"
  )
  return(list(
    "com" = e_com_comorb_all
  )
  )
}
