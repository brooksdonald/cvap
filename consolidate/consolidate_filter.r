# Create base condense file

load_base_condense_file <- function(a_data) {
    c_condense <-
    select(
        a_data,
        c(
            "a_iso",
            "a_name_short",
            "a_continent",
            "a_region_who",
            "a_status_who",
            "a_status_covax",
            "a_status_csc",
            "dvr_4wk_td_per_cat",
            "dvr_4wk_td_change_lm_trend",
            "cov_total_fv_cat",
            "t10_status",
            "t20_status",
            "t40_status",
            "t70_status",
            "ndvp_status",
            "adm_status_boost",
            "cov_hcw_booster_cat",
            "cov_total_hcw_com",
            "cov_total_60p_com",
            "cov_total_hcw_com_csc",
            "cov_total_60p_com_csc",
            "pu_used_per_cat",
            "del_cour_total_per_cat"
        )
    )

    # Filter by grouping
    c_condense_amc <- filter(c_condense, a_status_covax == "AMC")
    c_condense_csc <- filter(c_condense, a_status_csc == "Concerted support country")
    c_condense_amc_exc <-
    filter(c_condense, a_status_covax == "AMC")
    c_condense_africa <- filter(c_condense, a_continent == "Africa")

    c_condense_afr <- filter(c_condense, a_region_who == "AFR")
    c_condense_amr <- filter(c_condense, a_region_who == "AMR")
    c_condense_emr <- filter(c_condense, a_region_who == "EMR")
    c_condense_eur <- filter(c_condense, a_region_who == "EUR")
    c_condense_sear <- filter(c_condense, a_region_who == "SEAR")
    c_condense_wpr <- filter(c_condense, a_region_who == "WPR")

    condense_list <- list("amc" = c_condense_amc, 
                          "amc_exc" = c_condense_amc_exc,
                          "africa" = c_condense_africa,
                          "afr" = c_condense_afr,
                          "amr" = c_condense_amr,
                          "emr" = c_condense_emr,
                          "eur" = c_condense_eur,
                          "sear" = c_condense_sear,
                          "wpr" = c_condense_wpr,
                          "csc"= c_condense_csc)

    return(condense_list)

}