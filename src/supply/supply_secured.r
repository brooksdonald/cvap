
load_supply_secured_data <- function() {
    print(" >> Loading supply received datasets...")    
    supply_secured <- extract_supply_secured()
    supply_secured_lm <- extract_supply_secured_lm()
    base_supply_secured <- list(
      "supply_secured" = supply_secured,
      "supply_secured_lm" = supply_secured_lm
      )
    
    print(" >> Function 'load_supply_secured_data' done")
    return(base_supply_secured)
}

extract_supply_secured <- function() {
    print(" >> Loading supply secured data...")
    supply_input <- data.frame(
      read_excel(
        "data/input/base_supply_secured_summary.xlsx",
        sheet = "supply_tracker",
        skip = 2
        )
      )
    
    ## This function can be deleted in the future, added in July 2022 to aid the transition.
    helper_check_if_two_rows_were_deleted(supply_input)
    
    print(" >> Selecting relevant columns... ")
    supply_secured <- select(
      supply_input, c(
        "ISO3",
        "Secured.and.or.Expected.Vaccine..millions.of.doses.",
        "Bilateral.Deals..millions.of.doses.",
        "Bilateral.Donations..millions.of.doses.",
        "COVAX.Total..millions.of.doses.",
        "EU.Deal..millions.of.doses.",
        "Other.sources..millions.of.doses.",
        "Domestic.Supply..millions.of.doses."
        )
      )
    
    print(" >> Renaming columns... ")
    colnames(supply_secured) <- c(
      "a_iso",
      "sec_total_dose",
      "sec_bilat_dose",
      "sec_donat_dose",
      "sec_covax_dose",
      "sec_eu_dose",
      "sec_other_dose",
      "sec_domestic_dose"
      )
    
    print(" >> Renaming KOS to XKX...")
    supply_secured <- helper_rename_KOS_to_XKX(supply_secured, "a_iso")
    
    print(" >> Function 'extract_supply_secured' done")
    return(supply_secured)
}

extract_supply_secured_lm <- function() {
    print(" >> Loading supply secured last month data...")
    supply_input_lm <- data.frame(
      read_excel("data/input/base_supply_secured_summary_lastmonth.xlsx",
                 sheet = "supply_tracker",
                 skip = 2
                 )
      )
    
    ## This function can be deleted in the future, added in July 2022 to aid the transition.
    helper_check_if_two_rows_were_deleted(supply_input_lm)
    
    print(" >> Selecting relevant columns... ")
    supply_secured_lm <- select(
      supply_input_lm, c(
        "ISO3",
        "Secured.and.or.Expected.Vaccine..millions.of.doses."
        )
      )
    
    print(" >> Renaming columns... ")
    colnames(supply_secured_lm) <- c(
      "a_iso",
      "sec_total_dose_lm"
      )
    
    print(" >> Function 'extract_supply_secured_lm' done")
    return(supply_secured_lm)
}

transform_supply_secured_doses <- function(supply_secured, sec_date, supply_secured_lm) { #nolint
    print(" >> Transforming supply secured and supply secured last month datasets...")
    print(" >> Transforming supply secured data...")
    multiplier <- 1000000
    columns_to_multiply <- list(
      "sec_total_dose",
      "sec_bilat_dose",
      "sec_donat_dose",
      "sec_covax_dose",
      "sec_other_dose",
      "sec_eu_dose",
      "sec_domestic_dose"
      )
  
    print(" >> Multiplying current supply by a million...")
    for (c in columns_to_multiply) {
      supply_secured <- mutate(supply_secured, 
                               UQ(rlang::sym(c)) :=
                                 UQ(rlang::sym(c)) * multiplier)
      }

    print(" >> Transforming supply secured data last month...")
    print(" >> Multiplying last month supply summary by a million...")
    supply_secured_lm <- supply_secured_lm %>%
      mutate(
        sec_total_dose_lm = sec_total_dose_lm * 1000000
        )
    
    print(" >> Replacing NAs...")
    supply_secured <- supply_secured %>% replace(is.na(.), 0)
    supply_secured_lm <- supply_secured_lm %>% replace(is.na(.), 0)
    
    print(" >> Rounding...")
    supply_secured <- supply_secured %>% mutate_if(is.numeric, round)
    supply_secured_lm <- supply_secured_lm %>% mutate_if(is.numeric, round)

    print(" >> Summing others...")
    supply_secured <- supply_secured %>%
        mutate(
          sec_other_sum_dose = sec_eu_dose + sec_other_dose + sec_domestic_dose
          )

    print(" >> Adding secured supply dataset date...")
    supply_secured$sec_date <- sec_date
    
    print(" >> Merging current and last month secured supply datasets...")
    supply_secured <- left_join(supply_secured, supply_secured_lm, by = "a_iso")
    
    print(" >> Function 'transform_supply_secured_doses' done")
    return(supply_secured)
}
