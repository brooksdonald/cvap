
vxrate <- function(condense_list) {
  name_list <- list("amc", "africa", "emr", "amr")

  col_names_value <- list("dvr_cat", "dvr_trend", "booster_stat", "booster_hcw_stat")
  col_names_name <- list("cat_", "trend_", "booster_", "booster_hcw_")
  
  data_list <- list()

  for (colname in col_names_value) {
    data_list[[colname]] <- list()
    }

  for (name in name_list) {
    df <- condense_list[[name]]
    aggregate_col_list <- list(
      df$dvr_4wk_td_per_cat,
      df$dvr_4wk_td_change_lm_trend,
      df$booster_status,
      df$cov_hcw_booster_cat
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
  
  e_vrcat_all <- helper_join_dataframe_list(
    data_list[["cat_"]], join_by = "dvr_cat"
    )
  e_vrcat_all <- e_vrcat_all %>% replace(is.na(.), "None")
  
  e_trend_month_emr <- helper_join_dataframe_list(
    data_list[["trend_"]], join_by = "dvr_trend"
    )
  
  e_booster_all <- helper_join_dataframe_list(
    data_list[["booster_"]], join_by = "booster_stat"
    )
  
  e_booster_hcw <- helper_join_dataframe_list(
    data_list[["booster_hcw_"]], join_by = "booster_hcw_stat"
    )
  
  print(" >> Function 'vxrate' done")
  return(list(
      "all" = e_vrcat_all,
      "trend" = e_trend_month_emr,
      "booster" = e_booster_all,
      "booster_hcw" = e_booster_hcw
      )
  )
}