## Past coverage targets

targets <- function(condense_list) {
    target_list = list("amc", "africa")

    df_ten_perc = list()
    df_twenty_perc = list()
    df_forty_perc = list()
    df_seventy_perc = list()
    scaleup_list = list()

    for (target in target_list) {
        df = condense_list[[target]]
        df_ten <- aggregate(df$a_name_short,
                                list(df$t10_status),
                                paste,
                                collapse = "; "
        )
        colnames(df_ten) <- c("tar_stat", paste0("t10_", target))
        df_ten = append(df_ten_perc, list(df_ten))

        df_twenty <- aggregate(df$a_name_short,
                                list(df$t20_status),
                                paste,
                                collapse = "; "
        )
        colnames(df_twenty) <- c("tar_stat", paste0("t20_", target))
        df_twenty = append(df_twenty_perc, list(df_twenty))


        
        df_forty <- aggregate(df$a_name_short,
                                list(df$t40_status),
                                paste,
                                collapse = "; "
        )
        colnames(df_forty) <- c("tar_stat", paste0("t40_", target))
        df_forty = append(df_forty_perc, list(df_forty))


        df_seventy <- aggregate(df$a_name_short,
                                list(df$t70_status),
                                paste,
                                collapse = "; "
        )
        colnames(df_seventy) <- c("tar_stat", paste0("t70_", target))
        df_seventy = append(df_seventy_perc, list(df_seventy))


        scaleup <- aggregate(df$a_name_short,
                                list(df$t70_scaleup_cat),
                                paste,
                                collapse = "; "
        )
        colnames(scaleup) <- c("scaleup_cat", paste0("t70_", target))  
        scaleup = append(scaleup_list, list(scaleup))    
        
    }

    e_tar_past_all = helper_join_dataframe_list(df_ten_perc, df_twenty_perc, df_forty_perc, join_by="tar_stat")
    e_tar_past_all <- e_tar_past_all %>% replace(is.na(.), "None")

    e_tar_cur_all = helper_join_dataframe_list(df_seventy_perc, join_by = "tar_stat")

    e_tar_cur_scale_all = helper_join_dataframe_list(scaleup_list, join_by = "scaleup_cat")  

    return(list(e_tar_past_all, e_tar_cur_all, e_tar_cur_scale_all))
}

