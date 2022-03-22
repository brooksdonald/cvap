vxrate <- function(condense_list) {

    name_list <- list("amc", "africa", "emr", "amr")

    # Daily vaccination rate category, Monthly vaccination rate trend & booster
    df_list_cat <- list()
    df_list_trend <- list()
    df_list_booster <- list()

    for (name in name_list) {
        df <- condense_list[[name]]
        df_cat <- aggregate(df$a_name_short,
                       list(df$dvr_4wk_td_per_cat),
                       paste,
                       collapse = "; "
        )
        colnames(df_cat) <- c("dvr_cat", paste0("cat_", name))
        df_list_cat <- append(df_list_cat, list(df_cat))

        df_trend <- aggregate(df$a_name_short,
                       list(df$dvr_4wk_td_change_lm_trend),
                       paste,
                       collapse = "; "
        )
        colnames(df_trend) <- c("dvr_trend", paste0("trend_", name))
        df_list_trend <- append(df_list_trend, list(df_trend))
        # Booster use
        df_booster <- aggregate(df$a_name_short,
                       list(df$booster_status),
                       paste,
                       collapse = "; "
        )
        colnames(df_booster) <- c("booster_stat", paste0("booster_", name))
        df_list_booster <- append(df_list_booster, list(df_booster))

    }
    e_vrcat_all <- helper_join_dataframe_list(df_list_cat, join_by = "dvr_cat")
    e_vrcat_all <- e_vrcat_all %>% replace(is.na(.), "None")
    e_trend_month_emr <- helper_join_dataframe_list(df_list_trend, join_by="dvr_trend")
    e_booster_all <- helper_join_dataframe_list(df_list_booster, join_by="booster_stat")
    return(list("all" = e_vrcat_all, "trend" = e_trend_month_emr, "booster" = e_booster_all))

}
