
load_base_population <- function() {
    print(" >> Loading base population data...")
    b_pop_total <- data.frame(
        read_excel("data/input/static/base_population.xlsx",
            sheet = "base_population"
        )
    )

    print(" >> Selecting base population data...")
    z_pop_total <- 
        select(
            b_pop_total, 
            c
            ("COUNTRY_FK",
            "GENDER_FK",
            "AGEGROUP_FK",
            "VALUE"
        )
    )

    Print(" >> Renaming Columns...")
    colnames(z_pop_total) <- c(
        "a_iso",
        "gender",
        "age_group",
        "value"
    )

    return(z_pop_total)

}

transform_base_population <- function() {
    Print(" >> Segregating the different age groups...")
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
    z_pop_12p <- z_pop_total %>%
    filter(
        gender == "BOTH" & age_group%in%age_range12) %>%
        group_by(a_iso) %>%
        
            summarize_at("value",
                sum,
                na.rm = TRUE
        )

    colnames(z_pop_12p) <- c("a_iso","a_pop_12p")

    # Filter for 18 and above
    z_pop_18p <- z_pop_total %>%
    filter(
        gender == "BOTH" & age_group%in%age_range18) %>%
        group_by(a_iso) %>%
  
        summarize_at("value",
            sum,
            na.rm = TRUE
        ) 

    colnames(z_pop_18p) <- c("a_iso","a_pop_18p")

    # Filter for 60 and above
    z_pop_60p <- z_pop_total %>%
    filter(
        gender == "BOTH" & age_group%in%age_range60) %>%
        group_by(a_iso) %>%
    
        summarize_at("value",
            sum,
            na.rm = TRUE
        )

    colnames(z_pop_60p) <- c("a_iso","a_pop_60p")

    # Filter for total, male
    z_pop_total_male <- z_pop_total %>%
    filter(
        gender == "MALE" & age_group == "ALL") %>%
    
        group_by(a_iso) %>%
    
        summarize_at("value",
            sum,
            na.rm = TRUE
        )   

    colnames(z_pop_total_male) <- c("a_iso","a_pop_male")


    # Filter for total, female
    z_pop_total_fem <- z_pop_total %>%
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
    full_join(., z_pop_hcw, by = "a_iso")

    #FIXME What am I returning here?
    return()

}