load_entity_details <- function() {
  print(">> Loading entity detail data file...")
  entity_details <- data.frame(read_excel("data/input/static/base_entitydetails.xlsx",
                                          sheet = "data"))
  
  print(">> Selecting & renaming relevant entity detail data...")
  entity_characteristics <- entity_details %>%
    select(
      CODE,
      NAMEWORKEN,
      ABREVPUBLEN,
      CONTINENT,
      WHOREGIONC,
      WHO14SUBREGIONS,
      MAJORGEOAREASSUBS,
      UNICEFREGION,
      WHO_LEGAL_STATUS_TITLE,
      COVAX,
      WBINCOMESTATUS,
      GAVI
    ) %>%
    rename(
      a_iso = CODE,
      a_name_long = NAMEWORKEN,
      a_name_short = ABREVPUBLEN,
      a_region_who = WHOREGIONC,
      a_region_sub_who = WHO14SUBREGIONS,
      a_region_unicef = UNICEFREGION,
      a_continent = CONTINENT,
      a_continent_sub = MAJORGEOAREASSUBS,
      a_status_who = WHO_LEGAL_STATUS_TITLE,
      a_status_covax = COVAX,
      a_status_gavi = GAVI,
      a_income_group = WBINCOMESTATUS
    )
  
  print(">> Done.")
  return(entity_characteristics)
}

load_adhoc_details <- function() {
  print(">> Loading adhoc detail data file...")
  adhoc <- data.frame(read_excel("data/input/static/base_adhoc.xlsx",
                                 sheet = "data"))
  
  print(">> Renaming adhoc detail data...")
  colnames(adhoc) <- c(
    "iso",
    "a_region_who",
    "a_status_csc",
    "a_status_ivb",
    "pol_jj",
    "pol_old",
    "pol_old_source",
    "ss_target",
    "ss_deadline",
    "country_source",
    "date_13jan",
    "adm_tar_hcw_wpro",
    "a_pop_hcw",
    "pol_boost",
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
    "min_vx_rollout_date"
  )
  
  print(">> Removing unnecessary adhoc detail variables...")
  adhoc <- select(adhoc, -c(a_region_who))
  
  print(">> Done.")
  return(adhoc)
}

transform_entity_details <- function(entity, adhoc) {
  print(">> Joining entity and adhoc detail dataframes...")
  entity_characteristics <- left_join(entity, adhoc, by = c("a_iso" = "iso"))
  
  print(">> Modifying WHO regions...")
  entity_characteristics$a_region_who <-
    helper_replace_values_with_map(
      data = entity_characteristics$a_region_who,
      values = c("AMRO", "AFRO", "EMRO", "EURO", "SEARO", "WPRO"),
      map = c("AMR", "AFR", "EMR", "EUR", "SEAR", "WPR"),
      na_fill = "Other"
    )
  
  print(">> Modifying income groups...")
  entity_characteristics <- entity_characteristics %>%
    mutate(a_income_group = if_else(
      grepl("High income", a_income_group),
      "High income",
      a_income_group
    ))
  
  entity_characteristics$a_income_group <-
    helper_replace_values_with_map(
      data = entity_characteristics$a_income_group,
      values = c(
        "High income",
        "Upper middle income",
        "Lower middle income",
        "Low income"
      ),
      map = c("HIC", "UMIC", "LMIC", "LIC"),
      na_fill = "Other"
    )
  
  entity_characteristics$a_income_group_vis <-
    helper_replace_values_with_map(
      data = entity_characteristics$a_income_group,
      values = c("HIC", "UMIC", "LMIC", "LIC", "Other"),
      map = c("4) HIC", "3) UMIC", "2) LMIC", "1) LIC", "5) Other"),
      na_fill = "5) Other"
    )
  
  print(">> Preparing income groups with seperated IND & IDN...")
  entity_characteristics <- entity_characteristics %>%
    mutate(a_income_group_ind = ifelse(
      a_income_group_vis == "2) LMIC",
      ifelse(
        a_iso == "IND" | a_iso == "IDN",
        "2) LMIC - India & Indonesia",
        "2) LMIC excl. India & Indonesia"
      ),
      a_income_group_vis
    ))
  
  print(">> Modifying African sub-regions...")
  entity_characteristics$a_continent_sub <-
    helper_replace_values_with_map(
      data = entity_characteristics$a_continent_sub,
      values = c(
        "Eastern Africa",
        "Western Africa",
        "Middle Africa",
        "Southern Africa",
        "Northern Africa"
      ),
      map = c("Eastern", "Western", "Central", "Southern", "Northern"),
      drop_rest = FALSE
    )
  
  print(">> Cleaning HCW population data...")
  entity_characteristics <- entity_characteristics %>%
    mutate(a_pop_hcw = if_else(a_pop_hcw == 0, NA_real_, a_pop_hcw))
  
  print(">> Done.")
  return(entity_characteristics)
}
