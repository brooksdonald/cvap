
load_entity_chars <- function() {
    print(" >> Loading entity characteristics data...")
    entity_details <- data.frame(
        read_excel("data/_input/static/base_entitydetails.xlsx",
            sheet = "data"
        )
    )

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
                "WBINCOMESTATUS"
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
        "a_income_group"
    )
    # TODO should we drop NA here since some rows are blank and we are populating it with Other later?

    return(entity_details)
}


transform_entity_chars <- function(entity_characteristics) {
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
    
    print(" >> Rework Africa sub-regions...")
    
    entity_characteristics$a_continent_sub <- helper_replace_values_with_map(
        data = entity_characteristics$a_who_region,
        values = c("Eastern Africa", "Western Africa", "Middle Africa", 
                   "Southern Africa", "Northern Africa"),
        map = c("Eastern", "Western", "Central", "Southern", "Northern"),
        na_fill = "Other"
    )

    return(entity_characteristics)
}