
load_base_population <- function() {
    print(" >> Loading base population data...")
    base_population <- data.frame(
        read_excel("data/_input/static/base_population_unpop.xlsx",
            sheet = "base_population"
        )
    )

    print(" >> Selecting base population data...")
    base_population <-
        select(
            base_population,
            c
            (
                "COUNTRY_FK",
                "GENDER_FK",
                "AGEGROUP_FK",
                "VALUE"
            )
        )

    print(" >> Renaming Columns...")
    colnames(base_population) <- c(
        "a_iso",
        "gender",
        "age_group",
        "value"
    )

    return(base_population)
}

transform_base_population <- function(base_population, population_hcw) {
    print(" >> Getting the total population from UNPOP...")
    # Creating an empty list for all dataframes to be joined
    data_frames <- list()
    
    z_pop_tot <- base_population %>%
        filter(gender == "BOTH" & age_group == "ALL")
    z_pop_tot <- select(z_pop_tot, c("a_iso", "value"))
    colnames(z_pop_tot) <- c("a_iso", "a_pop_2021")
    data_frames <- append(data_frames, list(z_pop_tot))

    print(" >> Segregating the different age groups...")

    # adding "Y" to the age ranges to match the desired output
    a_pop_12u <- c(helper_add_char_to_list(c(0:9), "Y0"), 
                   helper_add_char_to_list(c(10:11), "Y"), "Y0")
    a_pop_12p <- helper_add_char_to_list(c(12:100), "Y")
    a_pop_18u <- c(helper_add_char_to_list(c(0:9), "Y0"), 
                   helper_add_char_to_list(c(10:17), "Y"), "Y0")
    a_pop_18p <- helper_add_char_to_list(c(18:100), "Y")
    a_pop_45p <- helper_add_char_to_list(c(45:100), "Y")
    a_pop_50p <- helper_add_char_to_list(c(50:100), "Y")
    a_pop_55p <- helper_add_char_to_list(c(55:100), "Y")
    a_pop_60p <- helper_add_char_to_list(c(60:100), "Y")
    a_pop_65p <- helper_add_char_to_list(c(65:100), "Y")

    age_ranges <- list(a_pop_12u, a_pop_12p, a_pop_18u, a_pop_18p, a_pop_45p,
                       a_pop_50p, a_pop_55p, a_pop_60p, a_pop_65p)
    
    # has to match the length of a list above
    age_ranges_names <- c(
        "a_pop_12u",
        "a_pop_12p",
        "a_pop_18u",
        "a_pop_18p",
        "a_pop_45p",
        "a_pop_50p",
        "a_pop_55p",
        "a_pop_60p",
        "a_pop_65p"
    )


    # Filter for different ages
    for (i in seq_len(length(age_ranges_names))) {
        
        df <- base_population %>%
            filter(gender == "BOTH" &
                age_group %in% age_ranges[[i]]) %>%
            group_by(a_iso) %>%
            summarize_at("value", sum, na.rm = TRUE)
        colnames(df) <- c("a_iso", age_ranges_names[i])

        data_frames <- append(data_frames, list(df))
    }

    # Filter for total, both genders
    for (g in c("MALE", "FEMALE")) {
        df <- base_population %>%
            filter(
                gender == g & age_group == "ALL"
            ) %>%
            group_by(a_iso) %>%
            summarize_at("value",
                sum,
                na.rm = TRUE
            )

        colnames(df) <- c("a_iso", paste("a_pop", sep = "_", tolower(g)))
        data_frames <- append(data_frames, list(df))
    }

    # Consolidate population values into single dataframe

    data_frames <- append(data_frames, list(population_hcw))

    population_data <- helper_join_dataframe_list(
        data_frames,
        join_by = "a_iso",
        ally = TRUE
    )
    return(population_data)
}