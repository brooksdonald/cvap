# Create base condense file

load_base_condense_file <- function(a_data) {
    c_condense <- 
    select(
        a_data,
        c(
            "a_iso",
            "a_name_short",
            "a_continent",
            "a_who_region",
            "a_who_status",
            "a_covax_status",
            "dvr_4wk_td_per_cat",
            "dvr_4wk_td_change_lm_trend",
            "cov_total_fv_cat",
            "t10_status",
            "t20_status",
            "t40_status",
            "t70_status",
            "t70_scaleup_cat",
            "booster_status",
            "pu_used_per_cat",
            "del_cour_total_per_cat",
            "sec_total_per_cat"
        )
    )

    # Filter by grouping
    c_condense_amc <- filter(c_condense, a_covax_status == "AMC")
    c_condense_amc_exc <-
    filter(c_condense, a_covax_status == "AMC" & a_iso != "NIC")
    c_condense_africa <- filter(c_condense, a_continent == "Africa")

    c_condense_afr <- filter(c_condense, a_who_region == "AFR")
    c_condense_amr <- filter(c_condense, a_who_region == "AMR")
    c_condense_emr <- filter(c_condense, a_who_region == "EMR")
    c_condense_eur <- filter(c_condense, a_who_region == "EUR")
    c_condense_sear <- filter(c_condense, a_who_region == "SEAR")
    c_condense_wpr <- filter(c_condense, a_who_region == "WPR")

    condense_list <- list("amc" = c_condense_amc, 
                          "amc_exc" = c_condense_amc_exc, 
                          "africa" = c_condense_africa, 
                          "afr" = c_condense_afr, 
                          "amr" = c_condense_amr, 
                          "emr" = c_condense_emr, 
                          "eur" = c_condense_eur, 
                          "sear" = c_condense_sear, 
                          "wpr" = c_condense_wpr)

    return(condense_list)

}


