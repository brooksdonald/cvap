supplies_cons <- function(condense_list) {
    supplies_list = list("amc", "africa", "amr", "emr")

    # Secured category & Courses delivered category
    df_list_secured = list()
    df_list_delivered = list()
    df_list_utilization = list()

    for (supp in supplies_list) {
        df = condense_list[[supp]]

        # Secured category
        df_secured <- aggregate(df$a_name_short,
                       list(df$sec_total_per_cat),
                       paste,
                       collapse = "; "
        )
        colnames(df_secured) <- c("cat", paste0("seccat_", supp))
        df_list_secured = append(df_list_secured, list(df_secured))

        # Courses delivered category
        df_delivered <- aggregate(df$a_name_short,
                       list(df$del_cour_total_per_cat),
                       paste,
                       collapse = "; "
        )
        colnames(df_delivered) <- c("cat", paste0("delcat_", supp))
        df_list_delivered = append(df_list_delivered, list(df_delivered))

        # Product utilization category
        df_utilization <- aggregate(df$a_name_short,
                       list(df$pu_used_per_cat),
                       paste,
                       collapse = "; "
        )
        colnames(df_utilization) <- c("cat", paste0("pucat_", supp))
        df_list_utilization = append(df_list_utilization, list(df_utilization))

    }


    # e_secdelpu_all = helper_join_dataframe_list(df_list_secured, df_list_delivered, df_list_utilization, join_by = "cat")

    e_secdelpu_sec = helper_join_dataframe_list(df_list_secured, join_by = "cat")
    e_secdelpu_all <- e_secdelpu_sec %>% replace(is.na(.), "None") 

    e_secdelpu_del = helper_join_dataframe_list(df_list_delivered, join_by = "cat")
    e_secdelpu_all <- e_secdelpu_del %>% replace(is.na(.), "None") 

    e_secdelpu_util = helper_join_dataframe_list(df_list_utilization, join_by = "cat")
    e_secdelpu_all <- e_secdelpu_util %>% replace(is.na(.), "None") 

return(list(e_secdelpu_all))

}

