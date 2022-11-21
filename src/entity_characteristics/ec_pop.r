
load_base_population <- function() {
    print(" >> Loading base population data... ")
    base_population <- data.frame(
      read_excel(
        "data/input/static/base_population_unpop.xlsx",
        sheet = "base_population"
        )
      )

    print(" >> Selecting relevant columns... ")
    base_population <- select(
      base_population, c(
        "COUNTRY_FK",
        "GENDER_FK",
        "AGEGROUP_FK",
        "VALUE"
        )
      )

    print(" >> Renaming columns... ")
    colnames(base_population) <- c(
      "a_iso",
      "gender",
      "age_group",
      "value"
      )

    print(" >> Function 'load_base_population' done")
    return(base_population)
}

transform_base_population <- function(population_data, adhoc_details) {
    print(" >> Creating list for dataframes to be joined...")
    temp_population_df <- list()
    
    print(" >> Filtering for relevent entries...")
    temp_base_population <- population_data %>%
      filter(gender == "BOTH" & age_group == "ALL") %>%
      select(a_iso, value)
    
    print(" >> Renaming selected columns...")
    colnames(temp_base_population) <- c("a_iso", "a_pop_2021")
    
    print(" >> Appending to dataframe...")
    temp_population_df <- append(temp_population_df, list(temp_base_population))

    print(" >> Segregating different age groups...")
    print(" >> Adding 'Y' to the age ranges...")
    a_pop_12u <- c(helper_add_char_to_list(c(0:9), "Y0"), 
                   helper_add_char_to_list(c(10:11), "Y"), 
                   "Y0")
    a_pop_12p <- helper_add_char_to_list(c(12:100), "Y")
    a_pop_18u <- c(helper_add_char_to_list(c(0:9), "Y0"), 
                   helper_add_char_to_list(c(10:17), "Y"), 
                   "Y0")
    a_pop_18p <- helper_add_char_to_list(c(18:100), "Y")
    a_pop_45p <- helper_add_char_to_list(c(45:100), "Y")
    a_pop_50p <- helper_add_char_to_list(c(50:100), "Y")
    a_pop_55p <- helper_add_char_to_list(c(55:100), "Y")
    a_pop_60p <- helper_add_char_to_list(c(60:100), "Y")
    a_pop_65p <- helper_add_char_to_list(c(65:100), "Y")
    a_pop_70p <- helper_add_char_to_list(c(70:100), "Y")
    a_pop_75p <- helper_add_char_to_list(c(75:100), "Y")

    age_ranges <- list(a_pop_12u, a_pop_12p, a_pop_18u, a_pop_18p, a_pop_45p,
                       a_pop_50p, a_pop_55p, a_pop_60p, a_pop_65p, a_pop_70p,
                       a_pop_75p)
    
    age_ranges_names <- c(
      "a_pop_12u",
      "a_pop_12p",
      "a_pop_18u",
      "a_pop_18p",
      "a_pop_45p",
      "a_pop_50p",
      "a_pop_55p",
      "a_pop_60p",
      "a_pop_65p",
      "a_pop_70p",
      "a_pop_75p"
    )

    print(" >> Filtering for specified age ranges...")
    for (i in seq_len(length(age_ranges_names))) {
      df <- population_data %>%
        filter(gender == "BOTH" & age_group %in% age_ranges[[i]]) %>%
        group_by(a_iso) %>%
        summarize_at("value", sum, na.rm = TRUE)
      
      colnames(df) <- c("a_iso", age_ranges_names[i])
      temp_population_df <- append(temp_population_df, list(df))
    }

    print(" >> Filtering for total and gender...")
    for (g in c("MALE", "FEMALE")) {
      df <- population_data %>%
        filter(gender == g & age_group == "ALL") %>%
        group_by(a_iso) %>%
        summarize_at("value", sum, na.rm = TRUE)
      
      colnames(df) <- c("a_iso", paste("a_pop", sep = "_", tolower(g)))
      temp_population_df <- append(temp_population_df, list(df))
    }

    print(" >> Consolidate population values into single dataframe...")
    population_data <- helper_join_dataframe_list(
      temp_population_df,
      join_by = "a_iso",
      ally = TRUE
      )
    
    print(" >> Merging healthcare worker data...")
    population_hcw <- select(
      adhoc_details, c(
        "iso",
        "a_pop_hcw"
      )
    )
        
    population_data <- left_join(
      population_data, population_hcw, by = c("a_iso" = "iso")
    )
    
    print(" >> Function 'transform_base_population' done")
    return(population_data)
}
