load_population_target_gender <- function() {
    print(" >> Loading COV Uptake gender data...")
    uptake_gender <- data.frame(
        read_excel("data/input/data_export_WIISE_V_COV_UPTAKE_GENDER_LAST_MONTH_LONG.xlsx",
            sheet = "v_COV_UPTAKE_GENDER_LAST_MONTH_"
        )
    )

    # Reduce columns & rename
    print(" >> Selecting uptake gender data...")
    uptake_gender <- 
        select(
            uptake_gender,
            c
            (
                "ISO_3_CODE",
                "DATE",
                "GENDER",
                "N_VACC_DOSE1",
                "N_VACC_LAST_DOSE"
            )
        )

        print(" >> Renaming Columns...")
        colnames(uptake_gender) <- c(
            "a_iso",
            "date", 
            "gender",
            "adm_a1d",
            "adm_fv"
        )

        return(uptake_gender)
}

# TODO Refactor this section
transform_population_target_gender <- function(uptake_gender, uptake_target_group) {
    data_frame <- list()

    for (gender in c("MALE", "FEMALE")) {
        df <- uptake_gender %>%
            filter(
                gender == "MALE"
            )
            colnames(df) <- c("a_iso","adm_date_gender", "adm_a1d_male", "adm_fv_male")
            data_frame <- append(data_frame, list(df))

        df <-uptake_gender %>%
            filter(
                gender == "FEMALE"
            )
            colnames(df) <- c("a_iso","adm_a1d_fem", "adm_fv_fem")
            data_frame <- append(data_frame, list(df))
        

    }


    # Consolidate population uptake gender into a single dataframe
    data_frame <- append(data_frame, list(uptake_gender))

    uptake_gender_data <- Reduce(
        function(x, y) merge(x, y, by = "a_iso", all = TRUE),
        data_frame
    )

    return(uptake_gender_data)

}
