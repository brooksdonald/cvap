
load_entity_characteristics <- function() {
    print(" >> Loading entity characteristics data...")
    entity_details <- data.frame(
        read_excel("input/static/base_entitydetails.xlsx",
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
        "a_unicef_region",
        "a_who_status",
        "a_covax_status",
        "a_income_group"
    )

    return(entity_details)
}



transform_entity_characteristics <- function(entity_characteristics) {
    print(" >> Rework WHO region...")
    # TODO map entities with a hash/dict and map them to data.frame
    entity_characteristics <- entity_characteristics %>%
        mutate(a_who_region = if_else(
            a_who_region == "AMRO",
            "AMR",
            if_else(
                a_who_region == "AFRO",
                "AFR",
                if_else(
                    a_who_region == "EMRO",
                    "EMR",
                    if_else(
                        a_who_region == "EURO",
                        "EUR",
                        if_else(
                            a_who_region == "SEARO",
                            "SEAR",
                            if_else(a_who_region == "WPRO", "WPR",
                                "Other"
                            )
                        )
                    )
                )
            )
        ))


    # TODO map income with hash and to the dataframes
    print(" >> Rework WHO income levels...")
    entity_characteristics <- entity_characteristics %>%
        mutate(a_income_group = if_else(
            grepl("High income", a_income_group),
            "HIC",
            if_else(
                a_income_group == "Upper middle income",
                "UMIC",
                if_else(
                    a_income_group == "Lower middle income",
                    "LMIC",
                    if_else(a_income_group == "Low income", "LIC",
                        "Other"
                    )
                )
            )
        ))

    return(entity_characteristics)
}