load_base_population <- function() {
  print(">> Loading base population data file...")
  base_population <- data.frame(
    read_excel("data/input/static/base_population_unpop.xlsx",
               sheet = "base_population")
  )
  
  print(">> Selecting & renaming relevant population data...")
  base_population <- base_population %>%
    select(COUNTRY_FK,
           GENDER_FK,
           AGEGROUP_FK,
           VALUE) %>%
    rename(
      a_iso = COUNTRY_FK,
      gender = GENDER_FK,
      age_group = AGEGROUP_FK,
      value = VALUE
    )
  
  print(">> Done.")
  return(base_population)
}

transform_base_population <-
  function(base_population, population_hcw) {
    print(">> Preparing total population data...")
    data_frames <- list()
    
    pop_tot <- base_population %>%
      filter(gender == "BOTH" & age_group == "ALL") %>%
      select(a_iso, value) %>%
      mutate(a_pop_2021 = value)
    
    data_frames <- append(data_frames, list(pop_tot))
    
    print(">> Preparing age range-specific population data...")
    # Add "Y" to the age ranges to match the desired output
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
    a_pop_80p <- helper_add_char_to_list(c(80:100), "Y")
    
    # Prepare list of all age ranges of interest
    age_range <-
      list(
        a_pop_12u,
        a_pop_12p,
        a_pop_18u,
        a_pop_18p,
        a_pop_45p,
        a_pop_50p,
        a_pop_55p,
        a_pop_60p,
        a_pop_65p,
        a_pop_70p,
        a_pop_75p,
        a_pop_80p
      )
    
    age_range_names <- c(
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
      "a_pop_75p",
      "a_pop_80p"
    )
    
    # Filter for different age ranges
    for (i in seq_len(length(age_range_names))) {
      df <- base_population %>%
        filter(gender == "BOTH" &
                 age_group %in% age_range[[i]]) %>%
        group_by(a_iso) %>%
        summarize_at("value", sum, na.rm = TRUE)
      colnames(df) <- c("a_iso", age_range_names[i])
      
      data_frames <- append(data_frames, list(df))
    }
    
    print(">> Preparing gender-specific population data...")
    # Filter for total, both genders
    for (g in c("MALE", "FEMALE")) {
      df <- base_population %>%
        filter(gender == g & age_group == "ALL") %>%
        group_by(a_iso) %>%
        summarize_at("value",
                     sum,
                     na.rm = TRUE)
      
      colnames(df) <-
        c("a_iso", paste("a_pop", sep = "_", tolower(g)))
      data_frames <- append(data_frames, list(df))
    }
    
    print(">> Consolidating population values into single dataframe...")
    population_data <- helper_join_dataframe_list(data_frames,
                                                  join_by = "a_iso",
                                                 ally = TRUE)
    
    print(">> Done.")
    return(population_data)
  }
