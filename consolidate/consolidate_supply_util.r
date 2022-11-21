
supplies_cons <- function(condense_list) {
    print(" >> Generating supplies lists for AMC participants, Africa, America, and Eastern Meditteranean region...")
    supplies_list <- list("amc", "africa", "amr", "emr")
    col_names_value <- list("seccat_", "delcat_", "pucat_", "covcat_")
    col_names_name <- list("cat", "cat", "cat", "cov_cat")
    data_list <- list()

    for (colname in col_names_value) {
        data_list[[colname]] <- list()
    }

    for (supp in supplies_list) {
        df <- condense_list[[supp]]

        # Secured, Courses delivered, Product utilization, Coverage category
        aggregate_col_list <- list(
            df$sec_total_per_cat,
            df$del_cour_total_per_cat,
            df$pu_used_per_cat,
            df$cov_total_fv_cat
        )
        for (i in seq_len(length(col_names_value))) {
            df_value <- aggregate(df$a_name_short,
                                list(aggregate_col_list[[i]]),
                                paste,
                                collapse = "; "
            )
            colnames(df_value) <- c(
                col_names_name[i], paste0(col_names_value[i], supp
                )
            )
            data_list[[i]] <- append(data_list[[i]], list(df_value))
        }
    }
    e_secdelpu_all <- helper_join_dataframe_list(as.list(c(
        data_list$seccat_, data_list$delcat_, data_list$pucat_)),
        join_by = "cat"
    )
    e_secdelpu_all <- e_secdelpu_all %>% replace(is.na(.), "None")
    e_cov_all <- helper_join_dataframe_list(
        data_list[["covcat_"]], join_by = "cov_cat"
    )
    
    print(" >> Function 'supplies_cons' done")
    return(list(
        "all" = e_secdelpu_all,
        "coverage" = e_cov_all
        )
      )
}
