
load_entity_chars <- function() {
    print(" >> Loading entity characteristics data...")
    entity_details <- data.frame(
      read_excel(
        "data/input/static/base_entitydetails.xlsx",
        sheet = "data"
        )
      )

    print(" >> Selecting relevant columns...")
    entity_details <- select(
      entity_details, c(
        "CODE",
        "NAMEWORKEN",
        "ABREVPUBLEN",
        "CONTINENT",
        "WHOREGIONC",
        "WHO14SUBREGIONS",
        "MAJORGEOAREASSUBS",
        "UNICEFREGION",
        "WHO_LEGAL_STATUS_TITLE",
        "COVAX",
        "WBINCOMESTATUS",
        "IFC",
        "GAVI"
        )
      )

    print(" >> Renaming columns...")
    colnames(entity_details) <- c(
      "a_iso",
      "a_name_long",
      "a_name_short",
      "a_continent",
      "a_who_region",
      "a_who_subregion",
      "a_continent_sub",
      "a_unicef_region",
      "a_who_status",
      "a_covax_status",
      "a_income_group",
      "a_ifc_status",
      "a_gavi_status"
      )

    print(" >> Function 'load_entity_chars' done")
    return(entity_details)
}

load_base_adhoc <- function() {
    print(" > Loading adhoc characteristics data...")
    adhoc_details <- data.frame(
      read_excel(
        "data/input/static/base_adhoc.xlsx",
        sheet = "data"
        )
      )

    print(" >> Renaming columns...")
    colnames(adhoc_details) <- c(
      "iso",
      "a_who_region",
      "a_csc_status",
      "ndvp_mid_target",
      "ndvp_mid_deadline",
      "ndvp_mid_rep_rate",
      "jj_policy",
      "older_def",
      "older_source",
      "expiry_risk",
      "ss_target",
      "ss_deadline",
      "country_source",
      "date",
      "a_pop",
      "a_pop_hcw",
      "a_pop_hcw_source"
      )
    
    adhoc_details <- select(adhoc_details, -c("a_who_region"))
    
    print(" >> Function 'load_base_adhoc' done")
    return(adhoc_details)
}

transform_entity_chars <- function(entity_details, adhoc_details) {
    print(" >> Joining adhoc_details to entity_details...")
    entity_characteristics <- left_join(
      entity_details, adhoc_details, by = c("a_iso" = "iso")
      )

    print(" >> Adjusting WHO region...")
    entity_characteristics$a_who_region <- helper_replace_values_with_map(
      data = entity_characteristics$a_who_region,
      values = c("AMRO", "AFRO", "EMRO", "EURO", "SEARO", "WPRO"),
      map = c("AMR", "AFR", "EMR", "EUR", "SEAR", "WPR"),
      na_fill = "Other"
      )

    print(" >> Adjusting income levels...")
    entity_characteristics <- entity_characteristics %>%
      mutate(a_income_group =
               if_else(
                 grepl("High income", a_income_group),
                 "High income",
                 a_income_group
                 )
             )
    
    print(" >> Adjusting income group...")
    entity_characteristics$a_income_group <- helper_replace_values_with_map(
        data = entity_characteristics$a_income_group,
        values = c("High income", "Upper middle income", 
                   "Lower middle income", "Low income"),
        map = c("HIC", "UMIC", "LMIC", "LIC"),
        na_fill = "Other"
        )
    
    entity_characteristics$a_income_group_vis <- helper_replace_values_with_map(
      data = entity_characteristics$a_income_group,
      values = c("HIC", "UMIC", "LMIC", "LIC", "Other"),
      map = c("4) HIC", "3) UMIC", "2) LMIC", "1) LIC", "5) Other"),
      na_fill = "5) Other"
      )
    
    print(" >> Adjusting Africa sub-regions...")
    entity_characteristics$a_continent_sub <- helper_replace_values_with_map(
      data = entity_characteristics$a_continent_sub,
      values = c("Eastern Africa", "Western Africa", "Middle Africa",
                 "Southern Africa", "Northern Africa"),
      map = c("Eastern", "Western", "Central", "Southern", "Northern"),
      drop_rest = FALSE
      )
    
    print(" >> Selecting relevant columns...")
    entity_characteristics <- select(
      entity_characteristics, -c(
        "a_pop_hcw", "a_pop_hcw_source")
    )

    print(" >> Function 'transform_entity_chars' done")
    return(entity_characteristics)
}
