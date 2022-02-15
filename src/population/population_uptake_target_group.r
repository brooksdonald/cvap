
load_population_target_groups <- function() {
    print(" >> Loading COV Uptake target group data...")
    uptake_target_group <- data.frame(
        read_excel("data/input/data_export_WIISE_V_COV_UPTAKE_TARGETGROUP_LAST_MONTH_LONG.xlsx",
            sheet = "v_COV_UPTAKE_TARGETGROUP_LAST_M"
        )
    )

    # Reduce columns & rename
    print(" >> Reducing columns and renaming them...")
    uptake_target_group <- 
        select(
            uptake_target_group, 
            c
            (
                "ISO_3_CODE",
                "DATE",
                "TARGET_GROUP",
                "N_VACC_DOSE1",
                "N_VACC_LAST_DOSE"
            )
        )

    print(" >> Renaming columns...")
    colnames(uptake_target_group) <- c(
        "a_iso",
        "date", 
        "target_group",
        "adm_a1d",
        "adm_fv"
    )

    return(uptake_target_group)   

}

#TODO Refactor this section 
transform_population_target_groups <- function(uptake_target_group) {
    uptake_df <- list()

    # Sort for healthcare workers, remove target columns & rename
    for (target_group in c("HW", "OLDER_60")) {
        df <- uptake_target_group %>%
            filter(
                target_group == "HW"
            )%>%
            select(
                uptake_target_group,
                -"target_group"
            )
            colnames(c_uptake_hcw) <- c("a_iso","adm_date_hcw", "adm_a1d_hcw", "adm_fv_hcw")
            uptake_df <- append(uptake_df, list(df))
        
        df <- uptake_target_group %>%
            filter(
                target_group == "OLDER_60"
            )%>%
            select(
                uptake_target_group,
                -"target_group"
            )
            colnames(c_uptake_60p) <- c("a_iso","adm_date_60p", "adm_a1d_60p", "adm_fv_60p")
            uptake_df <- append(uptake_df, list(df))

    }

    # Consolidate population uptake gender into a single dataframe
    uptake_df <- append(uptake_df, list(uptake_target_group))
    
    return(uptake_target_group)
}