
load_population_uptake <- function(headers, refresh_api) {
  print(" >> Loading target groups and gender data...")
  
  uptake_gender <- load_population_uptake_gender(headers, refresh_api)
  uptake_groups <- load_population_uptake_groups(headers, refresh_api)
  datalist_temp <- list("uptake_gender" = uptake_gender,
                        "uptake_groups" = uptake_groups)
  
  print(" >> Function 'load_population_uptake' done")  
  return(datalist_temp)
}


transform_population_uptake <- function(uptake_gender, uptake_groups) {
  print(" >> Transforming target groups and gender...")
  
  uptake_genders <- transform_population_uptake_gender(uptake_gender)
  uptake_groups <- transform_population_uptake_groups(uptake_groups)
  
  df_to_append <- append(uptake_groups, uptake_genders)
  
  output <- helper_join_dataframe_list(
    df_to_append,
    join_by = "a_iso",
    ally = TRUE
  )
  
  output$adm_date_gender <- output$adm_date_gender.x
  output <- select(output, -c("adm_date_gender.y", "adm_date_gender.x"))
  
  print(" >> Function 'transform_population_uptake' done")  
  return(output)
}


load_population_uptake_gender <- function(headers, refresh_api) {
  print(" >>> Loading COV Uptake gender data...")
  
  uptake_gender <- helper_wiise_api(
    "https://frontdoor-l4uikgap6gz3m.azurefd.net/WIISE/V_COV_UPTAKE_GENDER_LAST_MONTH_LONG",
    headers = FALSE, refresh_api)
  
  print(" >>> Selecting relevant columns...")
  uptake_gender <- select(
    uptake_gender, c(
      "ISO_3_CODE",
      "DATE",
      "GENDER",
      "N_VACC_DOSE1",
      "N_VACC_LAST_DOSE",
      "N_VACC_BOOSTER_DOSE"
    )
  )
  
  print(" >>> Renaming columns...")
  colnames(uptake_gender) <- c(
    "a_iso",
    "date",
    "gender",
    "adm_a1d",
    "adm_fv",
    "adm_booster"
  )
  
  print(" >>> Function 'load_population_uptake_gender' done")  
  return(uptake_gender)
}


load_population_uptake_groups <- function(headers, refresh_api) {
  print(" >>> Loading COV Uptake target group data...")
  
  uptake_target_group <- helper_wiise_api(
    "https://frontdoor-l4uikgap6gz3m.azurefd.net/WIISE/V_COV_UPTAKE_TARGETGROUP_LAST_MONTH_LONG",
    headers = FALSE, refresh_api)
  
  print(" >>> Selecting relevant columns...")
  uptake_target_group <- select(
    uptake_target_group, c(
      "ISO_3_CODE",
      "DATE",
      "TARGET_GROUP",
      "N_VACC_DOSE1",
      "N_VACC_LAST_DOSE",
      "N_VACC_BOOSTER_DOSE",
      "NUMBER_TARGET"
    )
  )
  
  print(" >>> Renaming columns...")
  colnames(uptake_target_group) <- c(
    "a_iso",
    "date",
    "target_group",
    "adm_a1d",
    "adm_fv",
    "adm_booster",
    "adm_target"
  )
  
  print(" >>> Removing duplicates...")
  uptake_target_group <- helper_check_for_duplicates(uptake_target_group)
  
  print(" >>> Function 'load_population_uptake_groups' done")  
  return(uptake_target_group)
}


transform_population_uptake_gender <- function(uptake_gender) {
  print(" >>> Transforming COV Uptake gender data...")
  datalist_temp <- list()
  
  var_columns <- c("adm_a1d", "adm_fv", "adm_booster")
  for (g in c("MALE", "FEMALE")) {
    df <- uptake_gender %>%
      filter(gender == g)
    df <- df %>% select(-"gender")
    colnames(df) <- c(
      "a_iso", 
      "adm_date_gender",
      helper_tr_add_suffix_to_list(var_columns, paste0("_", tolower(g)))
    )
    datalist_temp <- append(datalist_temp, list(df))
  }
  
  print(" >>> Function 'transform_population_uptake_gender' done")  
  return(datalist_temp)
}


transform_population_uptake_groups <- function(uptake_target_group) {
  print(" >>> Transforming COV Uptake groups data...")
  uptake_df <- list()
  
  age_group_suffix <- list("HW" = "_hcw", "OLDER_60" = "_60p")
  var_columns <- c("adm_date", "adm_a1d", "adm_fv", "adm_booster","adm_target")
  
  for (tg in c("HW", "OLDER_60")) {
    df <- uptake_target_group %>%
      filter(target_group == paste(tg) & is.na(adm_fv) == FALSE) %>%
      select(-"target_group")
    colnames(df) <- c(
      "a_iso",
      helper_tr_add_suffix_to_list(var_columns, unlist(age_group_suffix[[tg]]))
    )
    uptake_df <- append(uptake_df, list(df))
  }
  
  print(" >>> Function 'transform_population_uptake_groups' done")  
  return(uptake_df)
}
