
load_entity_chars <- function() {
    print(" >> Loading entity characteristics data...")
    entity_details <- data.frame(
        read_excel("data/input/static/base_entitydetails.xlsx",
            sheet = "data"
        )
    )
    
    b_pop_who <- data.frame(
      read_excel("data/input/static/base_population_who.xlsx",
                 sheet = "data"
      )
    )
    
    b_pop_who <- select(b_pop_who, c("iso","value"))

    print(" >> Selecting data...")
    entity_details <-
        select(
            entity_details,
            c(
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
    
    entity_characteristics <- full_join(entity_details, b_pop_who, c("CODE" = "iso"))

    print(" >> Renaming columns...")
    colnames(entity_characteristics) <- c(
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
        "a_gavi_status",
        "a_pop"
    )

    return(entity_characteristics)
}

load_conc_supp_list <- function() {
    print(" >> Loading concerted support list data...")
    b_adhoc <- data.frame(
        read_excel(
            "data/input/static/base_adhoc.xlsx",
            sheet = "data"
        )
    )

    ## Rename columns
    print(" >> Renaming adhoc Columns...")
    colnames(b_adhoc) <- c(
        "iso",
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
        "country_source"
    )
    return(b_adhoc)
}

transform_entity_chars <- function(entity_characteristics, b_adhoc) {
    print(" >> Joining b_csl to entity_characteristics...")
    entity_characteristics <- left_join(
        entity_characteristics, b_adhoc, by = c("a_iso" = "iso")
    )

    print(" >> Rework WHO region...")
    entity_characteristics$a_who_region <- helper_replace_values_with_map(
        data = entity_characteristics$a_who_region,
        values = c("AMRO", "AFRO", "EMRO", "EURO", "SEARO", "WPRO"),
        map = c("AMR", "AFR", "EMR", "EUR", "SEAR", "WPR"),
        na_fill = "Other"
    )

    print(" >> Rework WHO income levels...")

    # Fix high-income inconsistent spelling
    entity_characteristics <- entity_characteristics %>%
        mutate(a_income_group = if_else(grepl("High income", a_income_group),
            "High income", a_income_group
        ))

    entity_characteristics$a_income_group <- helper_replace_values_with_map(
        data = entity_characteristics$a_income_group,
        values = c(
            "High income", "Upper middle income",
            "Lower middle income", "Low income"
        ),
        map = c("HIC", "UMIC", "LMIC", "LIC"),
        na_fill = "Other"
    )
    
    entity_characteristics$a_income_group_vis <- helper_replace_values_with_map(
      data = entity_characteristics$a_income_group,
      values = c(
        "HIC", "UMIC", "LMIC", "LIC", "Other"
      ),
      map = c("4) HIC", "3) UMIC", "2) LMIC", "1) LIC", "5) Other"),
      na_fill = "5) Other"
    )
    
    print(" >> Rework Africa sub-regions...")
    entity_characteristics$a_continent_sub <- helper_replace_values_with_map(
        data = entity_characteristics$a_continent_sub,
        values = c("Eastern Africa", "Western Africa", "Middle Africa",
                   "Southern Africa", "Northern Africa"),
        map = c("Eastern", "Western", "Central", "Southern", "Northern"),
        drop_rest = FALSE
    )

    return(entity_characteristics)
}
