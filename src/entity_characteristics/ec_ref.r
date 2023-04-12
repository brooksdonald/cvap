
load_entity_chars <- function() {
    print(" >> Loading entity characteristics data...")
    entity_details <- data.frame(
        read_excel("data/input/static/base_entitydetails.xlsx",
            sheet = "data"
        )
    )

    print(" >> Selecting data...")
    entity_characteristics <-
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
        "a_gavi_status"
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
        "a_who_region",
        "a_csc_status",
        "a_ivb_status",
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
        "adm_target_hcw_wpro",
        "a_pop_hcw",
        "desk_officer",
        "booster_policy",
        "a_pop",
        "ri_dtp1",
        "ri_dtp3",
        "ri_mcv1",
        "ri_mcv2",
        "ri_zero_dose",
        "a_pop_comorb_increased_prop",
        "a_pop_comorb_high_prop",
        "a_pop_comorb_high_young_prop",
        "a_pop_comorb_high_older_prop",
        "country_name_friendly",
        "region_name",
        "min_vx_rollout_date"
    )
    b_adhoc <- subset(b_adhoc, select=-c(a_who_region))
    b_adhoc <- b_adhoc %>%
      mutate(ndvp_mid_deadline = as.Date(ndvp_mid_deadline))
    
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
    
    entity_characteristics <- entity_characteristics %>%
      mutate(a_pop_hcw = if_else(a_pop_hcw == 0, NA_real_, a_pop_hcw))

    return(entity_characteristics)
}
