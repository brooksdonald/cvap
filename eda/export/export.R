api_export_table <- function(a_data) {
  
  api <- filter(
    a_data,
    a_status_who == "Member State" |
      a_status_who == "Special" |
      a_status_who == "Territory or Area"
  )
  
  api <- select(
    api,
    c(
      "a_name_short",
      "a_iso",
      "a_region_who",
      "a_region_unicef",
      "a_income_group",
      "a_income_group_vis",
      "a_status_covax",
      "a_status_csc",
      "a_pop",
      "a_pop_female",
      "a_pop_male",
      "a_pop_18u",
      "a_pop_18p",
      "a_pop_60p",
      "a_pop_70",
      "a_pop_hcw",
      "date_del",
      "del_dose_total",
      "del_dose_since_lm",
      "del_cour_total",
      "del_cour_total_per",
      "del_cour_covax_prop",
      "pu_del_rem",
      "pu_del_rem_per",
      "pu_del_rem_timeto",
      "pu_used_per",
      "adm_date",
      "adm_tot_td",
      "adm_tot_a1d",
      "adm_tot_cps",
      "adm_tot_cps_homo",
      "adm_pv",
      "adm_tot_boost",
      "adm_date_gender",
      "adm_cps_female",
      "adm_cps_male",
      "adm_date_60p",
      "adm_cps_60p",
      "adm_date_hcw",
      "adm_cps_hcw",
      "dvr_4wk_td",
      "dvr_4wk_td_per",
      "dvr_4wk_td_max",
      "dvr_4wk_td_max_per",
      "dvr_4wk_fv",
      "dvr_4wk_fv_per",
      "dvr_4wk_td_lm",
      "dvr_4wk_td_change_lm_trend",
      "dvr_4wk_td_change_lm_per",
      "cov_total_a1d",
      "cov_total_fv",
      "cov_total_booster",
      "cov_total_fv_less_1m",
      "cov_total_fem_fv",
      "cov_total_male_fv",
      "cov_hcw_fv",
      "cov_60p_fv",
      "t10_status",
      "t20_status",
      "t40_status",
      "t40_cour_req",
      "t40_cour_need_del",
      "t40_cour_need_adm",
      "t70_status",
      "t70_cour_req",
      "t70_cour_need_del",
      "t70_cour_need_adm",
      "ndvp_target",
      "ndvp_deadline",
      "ndvp_status",
      "fin_tot",
      "fin_percapita",
      "a_date_refresh"
    )
  )
  
  colnames(api) <- c(
    "name_short",
    "code",
    "region_who",
    "region_unicef",
    "income_group",
    "income_group_vis",
    "status_covax",
    "status_csc",
    "pop",
    "pop_fem",
    "pop_male",
    "pop_u18",
    "pop_18p",
    "pop_60p",
    "pop_70per",
    "pop_hcw",
    "del_date",
    "del_dose_total",
    "del_dose_since_lm",
    "del_cour_total",
    "del_cour_total_per",
    "del_cour_covax_prop",
    "pu_del_rem",
    "pu_del_rem_per",
    "pu_del_rem_timeto",
    "pu_used_per",
    "adm_date",
    "adm_tot_td",
    "adm_tot_a1d",
    "adm_tot_cps",
    "adm_tot_cps_adjust",
    "adm_pv",
    "adm_tot_boost",
    "adm_date_gender",
    "adm_fem_cps",
    "adm_mal_cps",
    "adm_old_date",
    "adm_old_cps",
    "adm_hcw_date",
    "adm_hcw_cps",
    "dvr_4wk_td",
    "dvr_4wk_td_per",
    "dvr_4wk_td_max",
    "dvr_4wk_td_max_per",
    "dvr_4wk_fv",
    "dvr_4wk_fv_per",
    "dvr_4wk_td_lm",
    "dvr_4wk_td_change_lm_trend",
    "dvr_4wk_td_change_lm_per",
    "cov_tot_a1d",
    "cov_tot_cps",
    "cov_tot_boost",
    "cov_tot_cps_since_lm",
    "cov_fem_cps",
    "cov_mal_cps",
    "cov_hcw_cps",
    "cov_old_cps",
    "t10_status",
    "t20_status",
    "t40_status",
    "t40_cour_req",
    "t40_cour_need_del",
    "t40_cour_need_adm",
    "t70_status",
    "t70_cour_req",
    "t70_cour_need_del",
    "t70_cour_need_adm",
    "ct_target",
    "ct_deadline",
    "ct_status",
    "fin_tot",
    "fin_percapita",
    "refresh_date"
  )
  
  api$is_current <- TRUE
  
  return(api)
}

dashboard_export_table <- function(a_data) {
  
  dashboard <- a_data %>%
    ungroup() %>%
    filter(a_status_who == "Member State" |
             a_status_who == "Special" |
             a_status_who == "Territory or Area"
           )
  
  dashboard <- dashboard %>%
    select(a_iso,
           adm_date,
           adm_tot_td,
           adm_tot_a1d,
           adm_tot_cps_homo,
           adm_tot_boost,
           adm_tot_td_per,
           cov_total_a1d,
           cov_total_fv,
           cov_total_booster,
           date_intro
    ) %>%
    mutate(adm_tot_td_per = adm_tot_td_per * 100,
           cov_total_a1d = cov_total_a1d * 100,
           cov_total_fv = cov_total_fv * 100,
           cov_total_booster = cov_total_booster * 100,
           adm_date_year = year(adm_date),
           adm_date_week = if_else(isoweek(adm_date) < 10, paste0("0", isoweek(adm_date)), as.character(isoweek(adm_date))),
           adm_date_epi = as.character(paste(paste0(adm_date_year, "-W", adm_date_week), "7", sep = "-")))
  
  temp <- dashboard %>%
    filter(is.na(adm_date) == FALSE) %>%
    mutate(adm_date_new = ISOweek2date(adm_date_epi)) %>%
    select(a_iso,
           adm_date_new)
  
  dashboard <- left_join(dashboard, temp, by = "a_iso") %>%
    select(-adm_date_year,
           -adm_date_week,
           -adm_date_epi,
           -adm_date)

  colnames(dashboard) <- c(
    "COUNTRY",
    "DOSES_ADMINISTERED",
    "PERSONS_VACCINATED_ONE_PLUS_DOSE",
    "PERSONS_VACCINATED_COMPLETE",
    "PERSONS_VACCINATED_BOOSTER_ADD_DOSE",
    "DOSES_ADMINISTERED_PER100",
    "PERSONS_VACCINATED_ONE_PLUS_DOSE_PER",
    "PERSONS_VACCINATED_COMPLETE_PER",
    "PERSONS_VACCINATED_BOOSTER_ADD_DOSE_PER",
    "INTRO_DATE_FIRST",
    "DATE"
  )
  
  return(dashboard)
}
