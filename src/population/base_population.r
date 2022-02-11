
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

transform_base_population <- function() {
    print(" >> Segregating the different age groups...")
    age_range12= (12:100)
    age_range18 = (18:100)
    age_range60 = (60:100)

    # adding "Y" to the age ranges to match the 
    age_range12[] <- lapply(age_range12, function(x) ifelse(!is.na(x), paste0("Y", x), x))
    age_range12

    age_range18[] <- lapply(age_range18, function(x) ifelse(!is.na(x), paste0("Y", x), x))
    age_range18

    age_range60[] <- lapply(age_range60, function(x) ifelse(!is.na(x), paste0("Y", x), x))
    age_range60

    # Filter for 12 and above
    print(" >> Filtering age group 12 and above...")
    z_pop_12p <- base_population %>%
    filter(
        gender == "BOTH" & age_group%in%age_range12) %>%
        group_by(a_iso) %>%
        
            summarize_at("value",
                sum,
                na.rm = TRUE
        )

    colnames(z_pop_12p) <- c("a_iso","a_pop_12p")

    # Filter for 18 and above
    print(" >> Filtering age group 18 and above...")
    z_pop_18p <- base_population %>%
    filter(
        gender == "BOTH" & age_group%in%age_range18) %>%
        group_by(a_iso) %>%
  
        summarize_at("value",
            sum,
            na.rm = TRUE
        ) 

    colnames(z_pop_18p) <- c("a_iso","a_pop_18p")

    # Filter for 60 and above
    print(" >> Filtering age group 60 and above...")

    z_pop_60p <- base_population %>%
    filter(
        gender == "BOTH" & age_group%in%age_range60) %>%
        group_by(a_iso) %>%
    
        summarize_at("value",
            sum,
            na.rm = TRUE
        )

    colnames(z_pop_60p) <- c("a_iso","a_pop_60p")

    # Filter for total, male
    z_pop_total_male <- base_population %>%
    filter(
        gender == "MALE" & age_group == "ALL") %>%
    
        group_by(a_iso) %>%
    
        summarize_at("value",
            sum,
            na.rm = TRUE
        )   

    colnames(z_pop_total_male) <- c("a_iso","a_pop_male")


    # Filter for total, female
    z_pop_total_fem <- base_population %>%
    filter(
        gender == "FEMALE" & age_group == "ALL") %>%
    
        group_by(a_iso) %>%
    
        summarize_at("value",
            sum,
            na.rm = TRUE
        )

    colnames(z_pop_total_fem) <- c("a_iso","a_pop_fem")

    # Consolidate population values into single dataframe

    c_pop_disag <- left_join(z_pop_total_male, z_pop_total_fem, by = "a_iso") %>%
    left_join(., z_pop_12p, by = "a_iso") %>%
    left_join(., z_pop_18p, by = "a_iso") %>%
    left_join(., z_pop_60p, by = "a_iso") %>%
    full_join(., population_hcw, by = "a_iso")

    #FIXME Is c_pop_disag what I am returning?
    return(c_pop_disag)

}