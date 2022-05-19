vxrate <- function(condense_list) {
    name_list <- list("amc", "africa", "emr", "amr")

    col_names_value <- list("dvr_cat", "dvr_trend", "booster_stat")
    col_names_name <- list("cat_", "trend_", "booster_")
    data_list <- list()

    for (colname in col_names_value) {
        data_list[[colname]] <- list()
    }

    for (name in name_list) {
        df <- condense_list[[name]]

        # Daily vaccination rate category, Monthly vxrate rate trend & booster
        aggregate_col_list <- list(
            df$dvr_4wk_td_per_cat,
            df$dvr_4wk_td_change_lm_trend,
            df$booster_status
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
    return(list(
        "all" = e_vrcat_all,
        "trend" = e_trend_month_emr,
        "booster" = e_booster_all
        )
    )
}
