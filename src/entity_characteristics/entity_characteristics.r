
load_base_entitydetails <- function() {
  print(" >> Loading entity characteristics data...")
  base_entitydetails <- data.frame(
    read_excel("data/input/static/base_entitydetails.xlsx",
               sheet = "data"
               )
    )

  print(" >> Selecting relevant columns...")
  base_entitydetails <-select(
    base_entitydetails, c(
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
  colnames(base_entitydetails) <- c(
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

  print(" >> Function 'load_base_entitydetails' done")
  return(base_entitydetails)
}


load_base_adhoc <- function() {
  print(" >> Loading concerted support list data...")
  base_adhoc <- data.frame(
      read_excel("data/input/static/base_adhoc.xlsx",
                 sheet = "data"
                 )
      )

  print(" >> Renaming columns...")
  colnames(base_adhoc) <- c(
    "a_iso",
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
    "adm_target_hcw_wpro",
    "a_pop_hcw",
    "desk_officer",
    "booster_policy",
    "a_pop",
    "ri_dtp1",
    "ri_dtp3",
    "ri_mcv1",
    "ri_mcv2",
    "ri_zero_dose"
    )
  
  print(" >> Selecting relevant columns...")
  base_adhoc <- subset(base_adhoc, select=-c(a_who_region))
  
  print(" >> Formatting 'ndvp_mid_deadline' variable...")
  base_adhoc <- base_adhoc %>%
    mutate(ndvp_mid_deadline = as.Date(ndvp_mid_deadline))
  
  print(" >> Function 'load_base_adhoc' done")
  return(base_adhoc)
}


transform_entity_charcteristics <- function(base_entitydetails, base_adhoc) {
  print(" >> Joining base_adhoc data to base_entitydetails data...")
  entity_characteristics <- left_join(
    base_entitydetails, base_adhoc, by = c("a_iso")
    )

  print(" >> Reworking WHO region variable...")
  entity_characteristics$a_who_region <- helper_replace_values_with_map(
    data = entity_characteristics$a_who_region,
    values = c("AMRO", "AFRO", "EMRO", "EURO", "SEARO", "WPRO"),
    map = c("AMR", "AFR", "EMR", "EUR", "SEAR", "WPR"),
    na_fill = "Other"
    )

  print(" >> Reworking WHO income levels variable...")
  entity_characteristics <- entity_characteristics %>%
    mutate(a_income_group = if_else(grepl("High income", a_income_group),
                                    "High income",
                                    a_income_group)
           )

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
  
  print(" >> Reworking Africa sub-regions variable...")
  entity_characteristics$a_continent_sub <- helper_replace_values_with_map(
    data = entity_characteristics$a_continent_sub,
    values = c("Eastern Africa", "Western Africa", "Middle Africa",
               "Southern Africa", "Northern Africa"),
    map = c("Eastern", "Western", "Central", "Southern", "Northern"),
    drop_rest = FALSE
    )
  
  print(" >> Formatting 'a_pop_hcw' variable...")
  entity_characteristics <- entity_characteristics %>%
    mutate(a_pop_hcw = if_else(a_pop_hcw == 0, NA_real_, a_pop_hcw))

  print(" >> Function 'transform_entity_charcteristics' done")
  return(entity_characteristics)
}
