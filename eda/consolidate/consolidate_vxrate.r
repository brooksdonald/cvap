vxrate <- function(condense_list) {

    name_list = list("amc", "africa", "amr", "emr")

    df_list_cat = list()
    df_list_trend = list()

    for (name in name_list) {
        df = condense_list[[name]]
        df_cat <- aggregate(df$a_name_short,
                       list(df$dvr_4wk_td_per_cat),
                       paste,
                       collapse = "; "
        )
        colnames(df_cat) <- c("dvr_cat", paste0("cat_", name))
        df_list_cat = append(df_list_cat, list(df_cat))


        df_trend <- aggregate(df$a_name_short,
                       list(df$dvr_4wk_td_change_lm_trend),
                       paste,
                       collapse = "; "
        )
        colnames(df_trend) <- c("dvr_trend", paste0("trend_", name))
        df_list_trend = append(df_list_trend, list(df_trend))
    }

        

    
    e_vrcat_all = helper_join_dataframe_list(df_list_cat, join_by="dvr_cat")
    e_vrcat_all <- e_vrcat_all %>% replace(is.na(.), "None")

    e_trend_month_emr = helper_join_dataframe_list(df_list_trend, join_by="dvr_trend")


     return(list("all" = e_vrcat_all, "trend" = e_trend_month_emr))
    #return(name_list)

}

