
targets <- function(condense_list) {
    print(" >> Generating target lists for AMC participants and Africa region...")
    target_list <- list("amc", "africa")
    column_names_value <- list("t10_", "t20_", "t40_", "t70_", "ndvp_")
    column_names_name <- list("tar_stat", "tar_stat", "tar_stat", 
                              "tar_stat", "ndvp_cat")
    data_list <- list()

    for (colname in column_names_value) {
        data_list[[colname]] <- list()
    }

    for (target in target_list) {
        df <- condense_list[[target]]
        aggregate_columns_list <- list(
            df$t10_status,
            df$t20_status,
            df$t40_status,
            df$t70_status,
            df$ndvp_status
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
    e_tar_past_all <- helper_join_dataframe_list(
        as.list(c(data_list$t10_, data_list$t20_, data_list$t40_)),
        join_by = "tar_stat"
    )
    e_tar_past_all <- e_tar_past_all %>% replace(is.na(.), "None")
    e_tar_cur_all <- helper_join_dataframe_list(
        data_list[["t70_"]], join_by = "tar_stat"
    )
    e_ndvp_all <- helper_join_dataframe_list(
        data_list[["ndvp_"]], join_by = "ndvp_cat"
    )
    
    print(" >> Function 'targets' done")
    return(list(
        "tenperc" = e_tar_past_all,
        "seventyperc" = e_tar_cur_all,
        "ndvp" = e_ndvp_all
        )
    )
}
