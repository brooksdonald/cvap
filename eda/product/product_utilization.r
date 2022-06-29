# Product utilization

dose_utilization <- function(a_data, refresh_date) {
    ## Calculate remaining doses, absolute and % pop.
    print(" >>> Computing remaining doses, absolute and % pop...")
    a_data <- a_data %>%
        mutate(pu_del_rem = pmax(
            (if_else(
                is.na(del_dose_total) | del_dose_total == 0,
                NA_real_,
                ((del_dose_total * 0.9) - adm_td))),
            0)) %>%

        mutate(pu_del_rem_wast = pmax(
            (if_else(
                is.na(del_dose_total) | del_dose_total == 0,
                NA_real_,
                (del_dose_total - adm_td))),
            0)) %>%

        mutate(pu_del_rem_wast_lm = pmax(
            (if_else(
                is.na(del_dose_total_lm) | del_dose_total_lm == 0,
                NA_real_,
                (del_dose_total_lm - adm_td_lm))),
            0)) %>%

        mutate(del_dose_wast_per = del_dose_wast / a_pop) %>%
        mutate(del_dose_total_per = del_dose_total / a_pop) %>%
        mutate(pu_del_rem_per = pu_del_rem / a_pop) %>%
        mutate(pu_del_rem_wast_per = pu_del_rem_wast / a_pop) %>%
        mutate(pu_del_rem_timeto = if_else(
            is.infinite(pu_del_rem / dvr_4wk_td),
            NA_real_,
            pu_del_rem / dvr_4wk_td)) %>%
        mutate(pu_del_rem_timeto_date =
            as.Date(refresh_date + pu_del_rem_timeto))

    ## Calculate percent of doses received utilized
    print(" >>> Computing percent of doses received utilized...")
    a_data <- a_data %>%
        mutate(pu_del_rem_prop = if_else(
            pu_del_rem > 0,
            (pu_del_rem / del_dose_total),
            if_else(
                is.na(pu_del_rem),
                NA_real_,
                if_else(pu_del_rem <= 0, 0,
                NA_real_)
            )
        )) %>%
    mutate(pu_used_per = 1 - pu_del_rem_prop)

    ## Assign percent utilization categories
    print(" >>> Assigning percent utilization categories...")
    breaks <- c(0, 0.25, 0.5, 0.75, 1)
    tags <- c("0) <25%", "1) 25-49%", "2) 50-74%", "3) 75-100%")
    a_data$pu_used_per_cat <- cut(
        a_data$pu_used_per,
        breaks = breaks,
        include.lowest = TRUE,
        right = FALSE,
        labels = tags
    )
    return(a_data)
}

supply_pending <- function(a_data) {
    # Calculate supply secured not yet delivered, supply received not yet administered
    print(" >>> Computing supply secured not yet delivered, supply received not yet administered...")
    a_data <- a_data %>%
    mutate(sec_tobedel = pmax(sec_total - del_cour_total, 0)) %>%
    mutate(sec_tobedel_per = sec_tobedel / a_pop) %>%
    mutate(sec_tobedel_dose = sec_total_dose - del_dose_total) %>%
    mutate(sec_tobedel_dose_lm = sec_total_dose_lm - del_dose_total_lm) %>%
    mutate(sec_tobedel_dose_per = sec_tobedel_dose / a_pop) %>%
    mutate(rem_cour_del = pmax(del_cour_total - del_cour_wast - adm_fv_homo -
        (((adm_a1d_homo - adm_fv_homo) + adm_booster)  * 0.5), 0)) %>%
    mutate(rem_cour_del_wast = pmax(del_cour_total - adm_fv_homo -
        (((adm_a1d_homo - adm_fv_homo) + adm_booster)  * 0.5), 0)) %>%
    mutate(rem_cour_del_per = rem_cour_del / a_pop) %>%
    mutate(rem_cour_del_wast_per = rem_cour_del_wast / a_pop) %>%
    mutate(rem_cour_del_prop = rem_cour_del / del_cour_total)
    return(a_data)
}

course_sufficiency <- function(a_data, refresh_date) {
    # Calculate proportions of courses of total
    print(" >>> Computing proportions of courses of total...")
    a_data <- a_data %>%
        mutate(sec_del_prop = del_cour_total / sec_total)

    # Calculate if courses secured, received, and administered sufficient to reach targets
    print(" >>> Computing if courses secured, received, & administered sufficient to reach targets...")
    a_data <- a_data %>%
        mutate(t20_suff_sec = if_else(
            sec_total_per >= 0.2, "Yes", "No")) %>%
        mutate(t20_suff_del = if_else(
            del_cour_total_per >= 0.2, "Yes", "No")) %>%
        mutate(t40_suff_sec = if_else(
            sec_total_per >= 0.4, "Yes", "No")) %>%
        mutate(t40_suff_del = if_else(
            del_cour_total_per >= 0.4, "Yes", "No")) %>%
        mutate(t70_suff_sec = if_else(
            sec_total_per >= 0.7, "Yes", "No")) %>%
        mutate(t70_suff_del = if_else(
            del_cour_total_per >= 0.7, "Yes", "No"))    
 
    # Calculate absolute courses needed for reach targets
    print(" >>> Computing absolute courses needed to reach targets...")
    a_data <- a_data %>%
        mutate(t20_cour_req = round((a_pop * 0.2) * 1.1)) %>%
        mutate(t40_cour_req = round((a_pop * 0.4) * 1.1)) %>%
        mutate(t70_cour_req = round((a_pop * 0.7) * 1.1))

    # Calculate remaining secured, received, and admnistered courses required for targets
    print(" >>> Computing remaining secured, received, and admnistered courses required for targets...")
    a_data <- a_data %>%
        mutate(t20_cour_need_sec =
            round(pmax(t20_cour_req - sec_total, 0))) %>%
        mutate(t20_cour_need_del =
            round(pmax(t20_cour_req - del_cour_total, 0))) %>%
        mutate(t20_cour_need_adm =
            round(pmax(t20_cour_req - adm_fv_homo, 0))) %>%
        mutate(t40_cour_need_sec =
            round(pmax(t40_cour_req - sec_total, 0))) %>%
        mutate(t40_cour_need_del =
            round(pmax(t40_cour_req - del_cour_total, 0))) %>%
        mutate(t40_cour_need_adm =
            round(pmax(t40_cour_req - adm_fv_homo, 0))) %>%
        mutate(t70_cour_need_sec =
            round(pmax(t70_cour_req - sec_total, 0))) %>%
        mutate(t70_cour_need_del =
            round(pmax(t70_cour_req - del_cour_total, 0))) %>%
        mutate(t70_cour_need_adm =
            round(pmax(t70_cour_req - adm_fv_homo, 0)))
    return(a_data)
}

course_progress <- function(a_data, b_smartsheet, refresh_date, timeto_t70) {
    # Homogenize country coverage targets
    print(" >>>> Homogenizing country coverage targets...")
    a_data <- a_data %>%
    mutate(dp_deadline = as.Date(dp_deadline)) %>%
    mutate(ss_deadline = as.Date(ss_deadline))

    a_data <- a_data %>%
    mutate(ndvp_target = if_else(
        is.na(dp_target) == FALSE,
        dp_target,
        ss_target)) %>%
    mutate(ndvp_target_source = if_else(
        is.na(dp_target) == FALSE,
        "DP",
        "SS")) %>%
    mutate(ndvp_deadline = if_else(
        ndvp_target_source == "DP",
        dp_deadline,
        ss_deadline))

    a_data <- a_data %>%
    mutate(ndvp_deadline = as.Date(ndvp_deadline)) %>%
    mutate(timeto_ndvp = as.numeric(ndvp_deadline - refresh_date)) %>%
    mutate(a_pop_ndvp = a_pop * ndvp_target) %>%
    mutate(a_pop_ndvp_mid = a_pop * ndvp_mid_target)

    # Calculate progress against country coverage targets
    print(" >>> Computing progress against country coverage targets...")
    a_data <- a_data %>%
    mutate(ndvp_goalmet = if_else(
        cov_total_fv >= ndvp_target, 
        "Yes", 
        "No")) %>%
    mutate(ndvp_target_active = if_else(
        as.Date(ndvp_deadline) < refresh_date,
        NA_real_,
        ndvp_target)) %>%
    mutate(ndvp_rem =
        pmax(ndvp_target - cov_total_fv, 0)) %>%
    mutate(ndvp_peratpace = 
        ((adm_fv_homo + (dvr_4wk_fv * 
            as.numeric(as.Date(ndvp_deadline) - refresh_date)
        )) / a_pop)) %>%
    mutate(ndvp_pertogo = 
        pmax(ndvp_target - ndvp_peratpace, 0)) %>%
    mutate(ndvp_willmeet = if_else(
        ndvp_peratpace >= ndvp_target,
        "Yes",
        "No")) %>%
    mutate(ndvp_timeto = round(if_else(is.infinite(
        ((adm_pv + ((a_pop_ndvp - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td)),
        NA_real_,
        pmax((adm_pv + ((a_pop_ndvp - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td,
            0)))) %>%
    mutate(ndvp_ontrack = if_else(
        ndvp_peratpace >= ndvp_target & cov_total_fv <= ndvp_target,
        "Yes",
        "No")) %>%
    mutate(ndvp_offtrack = if_else(
        ndvp_goalmet != "Yes" & ndvp_willmeet != "Yes",
            "Yes",
            "No")) %>%
    mutate(ndvp_status = if_else(
        is.na(ndvp_target) | ndvp_target == 0,
        "Not captured",
        if_else(
            ndvp_goalmet == "Yes",
            "Goal met",
            if_else(
                is.na(ndvp_deadline),
                "No timeline",
                if_else(
                    ndvp_deadline < refresh_date,
                    "Deadline past",
                    if_else(
                        ndvp_ontrack == "Yes",
                        "On track",
                        if_else(ndvp_offtrack == "Yes", "Off track",
                                NA_character_)
                    )
                )
            )
        )
    )) %>%
    mutate(ndvp_status_num = if_else(
        is.na(ndvp_target) | ndvp_target == 0,
        1,
        if_else(
            ndvp_goalmet == "Yes",
            2,
            if_else(
                is.na(ndvp_deadline),
                3,
                if_else(
                    ndvp_deadline < refresh_date,
                    4,
                    if_else(
                        ndvp_ontrack == "Yes",
                        5,
                        if_else(ndvp_offtrack == "Yes", 6,
                                NA_real_)
                    )
                )
            )
        )
    )) %>%
    mutate(ndvp_rate_needed =
        pmax((((a_pop * ndvp_target) - adm_fv_homo) /
            as.numeric(ndvp_deadline - refresh_date)),
        0)
    ) %>%
    mutate(ndvp_rate_needed_dose =
        pmax(((adm_pv + (
            pmax(a_pop_ndvp - adm_pv - adm_fv_homo,
                0) *
            (2 - if_else(
                is.na(jj_policy),
                0,
                if_else(
                    jj_policy == "One dose",
                    del_dose_jj_prop,
                    NA_real_)))))
                / timeto_ndvp),
            0)
        ) %>%
    mutate(ndvp_scaleup = if_else(
        is.infinite(round((ndvp_rate_needed / dvr_4wk_fv), 2)),
        NA_real_,
        round(ndvp_rate_needed / dvr_4wk_fv), 2)) %>%
    mutate(ndvp_scaleup_dose = if_else(
        is.infinite(round((ndvp_rate_needed_dose / dvr_4wk_td), 2)),
        NA_real_,
        round(ndvp_rate_needed_dose / dvr_4wk_td), 2)) #%>%

    a_data$ndvp_scaleup_cat <- as.character(cut(
        a_data$ndvp_scaleup_dose,
        breaks = c(-Inf, 2, 5, 10, Inf),
        labels = c("3) <2x", "4) 3-5x", "5) 5-10x", "6) >10x"),
        include.lowest = TRUE,
        right = TRUE
    ))

    a_data$ndvp_scaleup_cat[a_data$ndvp_target == 0 |
        is.na(a_data$ndvp_target)] <- "1) Not captured"

    a_data$ndvp_scaleup_cat[a_data$ndvp_status == "Goal met"] <- "2) Goal met"


    # Progress against mid year targets
    print(" >>> Getting progress against mid year targets...")
    a_data <- a_data %>%
    mutate(ndvp_mid_rem = pmax(
        ndvp_mid_target - cov_total_fv,
        0)) %>%
    mutate(ndvp_mid_peratpace =
        ((adm_fv_homo + (dvr_4wk_fv * timeto_t70)) / a_pop)) %>%
    mutate(ndvp_mid_pertogo = pmax(
        ndvp_mid_target - ndvp_mid_peratpace,
        0))  %>%
    mutate(ndvp_mid_rate_needed =
        pmax(((a_pop * ndvp_mid_target) - adm_fv_homo) / timeto_t70,
        0)) %>%
    mutate(ndvp_mid_rate_needed_dose =
        pmax(((adm_pv + (
            pmax(a_pop_ndvp_mid - adm_pv - adm_fv_homo,
                0) *
            (2 - if_else(
                is.na(jj_policy),
                0,
                if_else(
                    jj_policy == "One dose",
                    del_dose_jj_prop,
                    NA_real_)))))
                / timeto_t70),
            0)) %>%
    mutate(ndvp_mid_scaleup = if_else(
        is.infinite(round((ndvp_mid_rate_needed / dvr_4wk_fv), 2)),
        NA_real_,
        round(ndvp_mid_rate_needed / dvr_4wk_fv), 2)) %>%
    mutate(ndvp_mid_scaleup_dose = if_else(
        is.infinite(round((ndvp_mid_rate_needed_dose / dvr_4wk_td), 2)),
        NA_real_,
        round(ndvp_mid_rate_needed_dose / dvr_4wk_td), 2)) %>%
    mutate(ndvp_mid_rate_needed_homo = if_else(
        is.na(ndvp_mid_rep_rate), 
        ndvp_mid_rate_needed_dose, ndvp_mid_rep_rate)) %>%
    mutate(ndvp_scaleup_dose_homo = if_else(
        is.infinite(round((ndvp_mid_rate_needed_homo / dvr_4wk_td), 2)), 
        NA_real_,
        round(ndvp_mid_rate_needed_homo / dvr_4wk_td), 2))

    a_data$ndvp_scaleup_cat_kpi <- cut(
        a_data$ndvp_mid_scaleup_dose,
        breaks = c(-Inf, 2, 10, Inf),
        labels = c("High", "Medium", "Low"),
        include.lowest = TRUE,
        right = FALSE
    )
    return(a_data)
}

course_add_notes <- function(a_data, refresh_date) {
    # Add notes
    a_data <- a_data %>%
    mutate(note_highcov = if_else(
        cov_total_fv > 0.5,
        "High fully vaccinated coverage",
        "No")) %>%
    mutate(note_recent_rollout = if_else(
        intro_date > (refresh_date - 60),
        "Recent rollout", "No")) %>%
    mutate(note_reporting_date = if_else(
        adm_date < (refresh_date - 10),
        "Likely reporting issue",
        NA_character_)) %>%
    mutate(
        note_drivers_auto = if_else(
            (note_reporting_date == "Likely reporting issue") |
                (note_nochange == 1) |
                (dvr_4wk_td_per < 0),
            "Likely reporting issue",
            if_else(
                note_highcov == "High fully vaccinated coverage",
                "High fully vaccinated coverage",
                if_else(
                    note_recent_rollout == "Recent rollout",
                    "Recent rollout",
                    NA_character_
                )
            )
        )
    ) %>%
    mutate(note_supplyconstraint = if_else(
        rem_cour_del_per < 0.05 & pu_used_per > 0.8,
        1,
        0)) %>%
    mutate(note_supplyneed = if_else(
        rem_cour_del_per < 0.05 & pu_used_per > 0.8,
        "Yes",
        "No"))

    a_data_temp <- select(
        a_data, c("a_iso", "adm_fv", "a_pop", "cov_total_fv", "t10_status", "t40_status") #nolint
    )
    a_data <- a_data %>%
    mutate(note_drivers = if_else(
        is.na(if_else(
            is.na(note_drivers_auto),
            note_ss_drivers,
            if_else(
                is.na(note_ss_drivers),
                note_drivers_auto,
                paste(note_drivers_auto, note_ss_drivers, sep = ", ")
            )
        )
    ),
    "None",
    if_else(
        is.na(note_drivers_auto),
        note_ss_drivers,
        if_else(
            is.na(note_ss_drivers),
            note_drivers_auto,
            paste(note_drivers_auto, note_ss_drivers, sep = ", ")
            )
        )
    ))

    return(a_data)

}