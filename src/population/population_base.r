
load_base_population <- function() {
    print(" >> Loading base population data...")
    base_population <- data.frame(
        read_excel("data/input/static/base_population.xlsx",
            sheet = "base_population"
        )
    )

    print(" >> Selecting base population data...")
    base_population <- 
        select(
            base_population, 
            c
            ("COUNTRY_FK",
            "GENDER_FK",
            "AGEGROUP_FK",
            "VALUE"
        )
    )

    Print(" >> Renaming Columns...")
    colnames(base_population) <- c(
        "a_iso",
        "gender",
        "age_group",
        "value"
    )

    return(base_population)

}


add_char_to_list <- function(l, char="Y") {

    retrun (lapply(l, function(x) ifelse(!is.na(x), paste0(no_change_from_previous, x), x)))

}

transform_base_population <- function(base_population, population_hcw) {
    print(" >> Segregating the different age groups...")
    age_range12= (12:100)
    age_range18 = (18:100)
    age_range60 = (60:100)

    # adding "Y" to the age ranges to match the 
    a_pop_12p[] <- add_char_to_list(age_range12)
    a_pop_18p[] < add_char_to_list(age_range18)
    a_pop_60p[] <- add_char_to_list(age_range60)

    age_ranges = c(a_pop_12p, a_pop_18p, a_pop_60p)
    data_frames = list()

    for (ar in age_ranges) {
        df = base_population %>% filter(
            gender == "BOTH" & agre_group %in% ar %>%
            group_by(a_iso) %>%
                summarize_at("value", sum, na.rm = TRUE)
        )
        colnames(df) <-  c("a_iso", !!sym(ar)) # TODO make sure the variable is NOT evaluated, we need it as a varaibale name only

        data_frames <- append(data_frames, list(df))
    }

    for (gender in c("MALE", "FEMALE")) {
        df <- base_population %>%
            filter(
                gender == "MALE" & age_group == "ALL") %>%
                    roup_by(a_iso) %>%
                        summarize_at("value",
                            sum,
                            na.rm = TRUE)   

        colnames(df) <- c("a_iso",  paste("a_pop", tolower(gender)))
        data_frames <- append(data_frames, list(df))
    }
    
    # Consolidate population values into single dataframe

    data_frames <- append(data_frames, list(population_hcw))

    population_data <- Reduce(
        function(x, y) merge(x, y, by = "a_iso", all.x = TRUE),
        data_frames
    )

    # FIXME Is c_pop_disag what I am returning?
    return(population_data)

}