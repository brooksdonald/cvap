
create_hrg_timeseries <- function() {
    print(" >> Retrieving data... ")
    raw_target <-
      GET(
        'https://frontdoor-l4uikgap6gz3m.azurefd.net/WIISE/V_COV_UPTAKE_TARGETGROUP_LONG'
        )
    raw_target_text <- content(raw_target, "text")
    raw_target_json <- fromJSON(raw_target_text, flatten = TRUE)
    base_target <- as.data.frame(raw_target_json)

    print(" >> Removing first six characters from API dataframes... ")
    names(base_target) <- substring(names(base_target), 7)

    print(" >> Filtering data for healthcare workers... ")
    target_hcw <- filter(base_target, TARGET_GROUP == "HW")

    print(" >> Generating HCW reporting status variable... ")
    target_hcw <- target_hcw %>%
      mutate(
        hw_repstat = if_else(
          is.na(N_VACC_LAST_DOSE) == FALSE & N_VACC_LAST_DOSE != 0,
          "Reporting",
          NA_character_
          )
        )

    target_hcw$DATE <-
      as.Date(paste0(as.character(target_hcw$DATE), '-01'), format = '%Y-%m-%d')

    print(" >> Filtering data for countries reporting on healthcare workers... ")
    target_hcw <- filter(target_hcw, hw_repstat == "Reporting")

    print(" >> Grouping data by ISO and date... ")
    target_hcw <- target_hcw %>%
      group_by(ISO_3_CODE, DATE) %>%
      top_n(1, N_VACC_LAST_DOSE)

    print(" >> Selecting relevant columns... ")
    target_hcw <- target_hcw %>%
      select(ISO_3_CODE,
             DATE,
             N_VACC_DOSE1,
             N_VACC_LAST_DOSE,
             N_VACC_BOOSTER_DOSE)

    print(" >> Filtering data for older adults... ")
    target_old <- filter(base_target, TARGET_GROUP == "OLDER_60")

    print(" >> Generating older adult reporting status variable... ")
    target_old <- target_old %>%
      mutate(
        old_repstat = if_else(
          is.na(N_VACC_LAST_DOSE) == FALSE & N_VACC_LAST_DOSE != 0,
          "Reporting",
          NA_character_
          ))

    target_old$DATE <-
      as.Date(paste0(as.character(target_old$DATE), '-01'), format = '%Y-%m-%d')

    print(" >> Filtering data for countries reporting on older adults... ")
    target_old <- filter(target_old, old_repstat == "Reporting")

    print(" >> Grouping data by ISO and date... ")
    target_old <- target_old %>%
      group_by(ISO_3_CODE, DATE) %>%
      top_n(1, N_VACC_LAST_DOSE)

    print(" >> Selecting relevant columns... ")
    target_old <- target_old %>%
      select(ISO_3_CODE,
             DATE,
             N_VACC_DOSE1,
             N_VACC_LAST_DOSE,
             N_VACC_BOOSTER_DOSE)

    print(" >> Renaming columns... ")
    colnames(target_old) <- c(
      "ISO_3_CODE",
      "DATE",
      "N_VACC_DOSE1_old",
      "N_VACC_LAST_DOSE_old",
      "N_VACC_BOOSTER_DOSE_old"
      )

    print(" >> Joining healthcare worker and older adult data... ")
    target_hcwold <- full_join(target_hcw, target_old, by = c("ISO_3_CODE" = "ISO_3_CODE","DATE" = "DATE"))

    target_hcwold <- target_hcwold %>%
      mutate(adm_date_month = if_else(year(DATE) == 2022,
                                      as.numeric(month(DATE) + 12),
                                      as.numeric(month(DATE))))

    print(" >> Function 'transform_population_uptake_group' done")
    return(target_hcwold)
}
