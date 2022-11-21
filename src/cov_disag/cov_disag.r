
load_population_uptake <- function(headers, refresh_api) {
    print(" >> Loading population uptake data... ")
    base_uptake_gender <- load_population_uptake_gender(headers, refresh_api)
    base_uptake_group <- load_population_uptake_group(headers, refresh_api)
    datalist <- list("uptake_gender" = base_uptake_gender,
                     "uptake_group" = base_uptake_group)
    print(" >> Function 'load_population_uptake' done")
    return(datalist)
}

transform_population_uptake <- function(uptake_gender, uptake_group) {
    print(" >> Transforming population uptake data... ")
    temp_uptake_gender_df <- transform_population_uptake_gender(uptake_gender)
    temp_uptake_group_df <- transform_population_uptake_group(uptake_group)
    temp_population_uptake_df <- append(temp_uptake_group_df, temp_uptake_gender_df)
    
    population_uptake <- helper_join_dataframe_list(
      temp_population_uptake_df,
      join_by = "a_iso",
      ally = TRUE
    )
    
    population_uptake$adm_date_gender <- population_uptake$adm_date_gender.x
    population_uptake <- select(population_uptake, -c("adm_date_gender.y", "adm_date_gender.x"))
    
    print(" >> Function 'transform_population_uptake' done")
    return(population_uptake)
}

load_population_uptake_gender <- function(headers, refresh_api) {
    print(" >> Loading population uptake gender data... ")
    base_uptake_gender <- helper_wiise_api(
      "https://extranet.who.int/xmart-api/odata/WIISE/V_COV_UPTAKE_GENDER_LAST_MONTH_LONG",
      headers, refresh_api)
    
    print(" >> Selecting relevant columns... ")
    base_uptake_gender <- select(
      base_uptake_gender, c(
        "ISO_3_CODE",
        "DATE",
        "GENDER",
        "N_VACC_DOSE1",
        "N_VACC_LAST_DOSE",
        "N_VACC_BOOSTER_DOSE"
        )
      )
    
    print(" >> Renaming columns... ")
    colnames(base_uptake_gender) <- c(
      "a_iso",
      "date",
      "gender",
      "adm_a1d",
      "adm_fv",
      "adm_booster"
      )
    
    print(" >> Function 'load_population_uptake_gender' done")
    return(base_uptake_gender)
}

load_population_uptake_group <- function(headers, refresh_api) {
    print(" >> Loading population uptake group data... ")
    base_uptake_group <- helper_wiise_api(
      "https://extranet.who.int/xmart-api/odata/WIISE/V_COV_UPTAKE_TARGETGROUP_LAST_MONTH_LONG",
      headers, refresh_api)
  
    print(" >> Selecting relevant columns... ")
    base_uptake_group <- select(
      base_uptake_group, c(
      "ISO_3_CODE",
      "DATE",
      "TARGET_GROUP",
      "N_VACC_DOSE1",
      "N_VACC_LAST_DOSE",
      "N_VACC_BOOSTER_DOSE",
      "NUMBER_TARGET"
      )
    )
    
    print(" >> Renaming columns... ")
    colnames(base_uptake_group) <- c(
      "a_iso",
      "date",
      "target_group",
      "adm_a1d",
      "adm_fv",
      "adm_booster",
      "adm_target"
      )
    
    print(" >> Removing duplicates...")
    base_uptake_group <- helper_check_for_duplicates(base_uptake_group)
    
    print(" >> Function 'load_population_uptake_group' done")
    return(base_uptake_group)
}

transform_population_uptake_gender <- function(base_uptake_gender) {
    print(" >> Transforming population uptake gender data... ")
    temp_uptake_gender_df <- list()
    
    print(" >> Sorting for gender, removing target columns...")
    var_columns <- c("adm_a1d", "adm_fv", "adm_booster")
    for (g in c("MALE", "FEMALE")) {
      temp_uptake_gender <- base_uptake_gender %>%
        filter(gender == g) %>%
        select(-"gender")
      
      print(" >> Renaming columns... ")
      colnames(temp_uptake_gender) <- c(
        "a_iso", 
        "adm_date_gender",
        helper_tr_add_suffix_to_list(var_columns, paste0("_", tolower(g)))
        )
      temp_uptake_gender_df <- append(temp_uptake_gender_df, list(temp_uptake_gender))
    }
    print(" >> Function 'transform_population_uptake_gender' done")
    return(temp_uptake_gender_df)
}


transform_population_uptake_group <- function(base_uptake_group) {
    print(" >> Transforming population uptake groups data... ")
    temp_uptake_group_df <- list()
    
    age_group_suffix <- list("HW" = "_hcw", "OLDER_60" = "_60p")
    var_columns <- c("adm_date", "adm_a1d", "adm_fv", "adm_booster","adm_target")
    
    print(" >> Sorting for healthcare workers and older adults, removing target columns...")
    for (tg in c("HW", "OLDER_60")) {
      temp_uptake_group <- base_uptake_group %>%
        filter(target_group == paste(tg) & is.na(adm_fv) == FALSE) %>%
        select(-"target_group")
      
      print(" >> Renaming columns... ")
      colnames(temp_uptake_group) <- c(
        "a_iso",
        helper_tr_add_suffix_to_list(var_columns, unlist(age_group_suffix[[tg]]))
        )
      
      temp_uptake_group_df <- append(temp_uptake_group_df, list(temp_uptake_group))
    }
    
    print(" >> Function 'transform_population_uptake_group' done")
    return(temp_uptake_group_df)
}

