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
transform_population_target_gender <- function(uptake_gender) {
    # Sort for males, remove target columns & rename
    c_uptake_male <- filter(c_uptake_gender, gender  == "MALE")
    c_uptake_male <- select(c_uptake_male, -"gender")

    colnames(c_uptake_male) <- c("a_iso","adm_date_gender", "adm_a1d_male", "adm_fv_male")

    # Sort for females, remove target columns & rename
    c_uptake_fem <- filter(c_uptake_gender, gender == "FEMALE")
    c_uptake_fem <- select(c_uptake_fem, -c("gender", "date"))

    colnames(c_uptake_fem) <- c("a_iso","adm_a1d_fem", "adm_fv_fem")

    # Merge dataframes
    c_uptake_disag <- full_join(c_uptake_hcw, c_uptake_60p, by = "a_iso") %>%
    full_join(., c_uptake_male, by = "a_iso") %>%
    full_join(., c_uptake_fem, by = "a_iso")

    return(uptake_gender)
}
