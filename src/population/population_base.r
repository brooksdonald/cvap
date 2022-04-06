
load_base_population <- function() {
    print(" >> Loading base population data...")
    base_population <- data.frame(
        read_excel("data/_input/static/base_population.xlsx",
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
    print(" >> Segregating the different age groups...")
    age_range12u <- (1:11)
    age_range12 <- (12:100)
    age_range18u <- (1:17)
    age_range18 <- (18:100)
    age_range60 <- (60:100)

    # Adding "Y0" to age_range11 for values between 1-9
    age_range12u_list <- list()

    for (age in age_range12u) {
        if (age < 10) {
            age <- paste("Y0", as.character(age), sep = "")
            age_range12u_list <- append(age_range12u_list, age)
        }
        else {
            age <- paste("Y", as.character(age), sep = "")
            age_range12u_list <- append(age_range12u_list, age)
            }
        }

    # Adding "Y0" to age_range17 for values between 1-9
    #TODO Merge u12 and u18 functions
    age_range18u_list <- list()
    
    for (age in age_range18u) {
        if (age < 10) {
            age <- paste("Y0", as.character(age), sep = "")
            age_range18u_list <- append(age_range18u_list, age)
        }
        else {
            age <- paste("Y", as.character(age), sep = "")
            age_range18u_list <- append(age_range18u_list, age)
        }
    }

    # adding "Y" to the age ranges to match the desired output
    a_pop_12u <- unlist(age_range12u_list)
    a_pop_12p <- helper_add_char_to_list(age_range12)
    a_pop_18u <- unlist(age_range18u_list)
    a_pop_18p <- helper_add_char_to_list(age_range18)
    a_pop_60p <- helper_add_char_to_list(age_range60)

    print(a_pop_12u)
    print(a_pop_12p)
    print(a_pop_18u)
    print(a_pop_18p)
    print(a_pop_60p)

    age_ranges <- c(a_pop_12u, a_pop_12p, a_pop_18u, a_pop_18p, a_pop_60p)
    # has to match the length of a list above
    age_ranges_names <- c("a_pop_12u", "a_pop_12p", "a_pop_18p", "a_pop_18p", "a_pop_60p")
    data_frames <- list()

    for (i in seq_len(length(age_ranges_names))) {
        df <- base_population %>%
            filter(gender == "BOTH" &
                age_group %in% age_ranges[i]) %>%
            group_by(a_iso) %>%
            summarize_at("value", sum, na.rm = TRUE)
        colnames(df) <- c("a_iso", age_ranges_names[i])

        data_frames <- append(data_frames, list(df))
    }

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

    population_data <- Reduce(
        function(x, y) merge(x, y, by = "a_iso", all = TRUE),
        data_frames
    )

    return(population_data)
}