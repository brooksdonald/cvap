# Product utilization

dose_utilization <- function(a_data) {
    ## Calculate remaining doses, absolute and % pop.
    print(" >>> Computing remaining doses, absolute and % pop...")
    a_data <- a_data %>%
        mutate(pu_del_rem = if_else(
            (if_else(
                is.na(del_dose_total) | del_dose_total == 0,
                NA_real_,
                ((del_dose_total * 0.9) - adm_td)
            )) < 0, 0, 
            (if_else(
                is.na(del_dose_total) | del_dose_total == 0,
                NA_real_,
                ((del_dose_total * 0.9) - adm_td)
            ))))%>%
        
        mutate(pu_del_rem_wast = if_else(
            (if_else(
                is.na(del_dose_total) | del_dose_total == 0,
                NA_real_,
                (del_dose_total - adm_td)
            )) < 0, 0, 
            (if_else(
                is.na(del_dose_total) | del_dose_total == 0,
                NA_real_,
                (del_dose_total - adm_td)
            ))))%>%
        
        mutate(pu_del_rem_wast_lm = if_else(
            (if_else(
                is.na(del_dose_total_lm) | del_dose_total_lm == 0,
                NA_real_,
                (del_dose_total_lm - adm_td_lm)
            )) < 0, 0, 
            (if_else(
                is.na(del_dose_total_lm) | del_dose_total_lm == 0,
                NA_real_,
                (del_dose_total_lm - adm_td_lm)
            ))))%>%
        
        mutate(del_dose_wast_per = del_dose_wast / a_pop) %>%
        
        mutate(del_dose_total_per = del_dose_total / a_pop) %>%
        
        mutate(pu_del_rem_per = pu_del_rem / a_pop) %>%
        
        mutate(pu_del_rem_wast_per = pu_del_rem_wast / a_pop) %>%
        
        mutate(pu_del_rem_timeto = if_else(is.infinite(pu_del_rem / dvr_4wk_td), NA_real_,
                                           (pu_del_rem / dvr_4wk_td))) %>%
        
        mutate(pu_del_rem_timeto_date = as.Date(refresh_date + pu_del_rem_timeto))
    
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
    mutate(sec_tobedel = if_else((sec_total - del_cour_total) < 0, 0, (sec_total - del_cour_total))) %>%
    mutate(sec_tobedel_per = sec_tobedel / a_pop) %>%
    mutate(sec_tobedel_dose = sec_total_dose - del_dose_total) %>%
    mutate(sec_tobedel_dose_lm = sec_total_dose_lm - del_dose_total_lm) %>%
    mutate(sec_tobedel_dose_per = sec_tobedel_dose / a_pop) %>%
    # TODO Test pmax() on code below
    # pmax((del_cour_total - del_cour_wast - adm_fv_homo - (((adm_a1d_homo - adm_fv_homo) + adm_booster)  * 0.5)), 0)
    mutate(rem_cour_del = if_else((del_cour_total - del_cour_wast - adm_fv_homo - (((adm_a1d_homo - adm_fv_homo) + adm_booster)  * 0.5)) < 0, 0,
                                    (del_cour_total - del_cour_wast - adm_fv_homo - (((adm_a1d_homo - adm_fv_homo) + adm_booster)  * 0.5)))) %>%
    mutate(rem_cour_del_wast = if_else((del_cour_total - adm_fv_homo - (((adm_a1d_homo - adm_fv_homo) + adm_booster)  * 0.5)) < 0, 0,
                                           (del_cour_total - adm_fv_homo - (((adm_a1d_homo - adm_fv_homo) + adm_booster)  * 0.5)))) %>% 
    mutate(rem_cour_del_per = rem_cour_del / a_pop) %>%
    mutate(rem_cour_del_wast_per = rem_cour_del_wast / a_pop) %>%
    mutate(rem_cour_del_prop = rem_cour_del / del_cour_total)
    return(a_data)
}

course_sufficiency <- function(a_data) {
    # Calculate proportions of courses of total
    print(" >>> Computing proportions of courses of total...")
    a_data <- a_data %>%
        mutate(sec_del_prop = del_cour_total / sec_total)

    # Calculate if courses secured, received, and administered sufficient to reach targets
    print(" >>> Computing if courses secured, received, & administered sufficient to reach targets...")
    a_data <- a_data %>%
        mutate(t20_suff_sec = if_else(sec_total_per >= 0.2, "Yes", "No")) %>%
        mutate(t20_suff_del = if_else(del_cour_total_per >= 0.2, "Yes", "No")) %>%
        mutate(t40_suff_sec = if_else(sec_total_per >= 0.4, "Yes", "No")) %>%
        mutate(t40_suff_del = if_else(del_cour_total_per >= 0.4, "Yes", "No")) %>%
        mutate(t70_suff_sec = if_else(sec_total_per >= 0.7, "Yes", "No")) %>%
        mutate(t70_suff_del = if_else(del_cour_total_per >= 0.7, "Yes", "No"))    
 
    # Calculate absolute courses needed for reach targets
    print(" >>> Computing absolute courses needed for reach targets...")
    a_data <- a_data %>%
        mutate(t20_cour_req = round((a_pop * 0.2) * 1.1)) %>%
        mutate(t40_cour_req = round((a_pop * 0.4) * 1.1)) %>%
        mutate(t70_cour_req = round((a_pop * 0.7) * 1.1))

    # Calculate remaining secured, received, and admnistered courses required for targets
    print(" >>> Computing remaining secured, received, and admnistered courses required for targets...")
    a_data <- a_data %>%
        mutate(t20_cour_need_sec = round(if_else((t20_cour_req - sec_total) < 0, 0,
        (t20_cour_req - sec_total)))) %>%
        mutate(t20_cour_need_del = round(if_else((t20_cour_req - del_cour_total) < 0, 0,
        (t20_cour_req - del_cour_total)))) %>%
        mutate(t20_cour_need_adm = round(if_else((t20_cour_req - adm_fv_homo) < 0, 0,
        (t20_cour_req - adm_fv_homo)))) %>%
        mutate(t40_cour_need_sec = round(if_else((t40_cour_req - sec_total) < 0, 0,
        (t40_cour_req - sec_total)))) %>%
        mutate(t40_cour_need_del = round(if_else((t40_cour_req - del_cour_total) < 0, 0,
        (t40_cour_req - del_cour_total)))) %>%
        mutate(t40_cour_need_adm = round(if_else((t40_cour_req - adm_fv_homo) < 0, 0,
        (t40_cour_req - adm_fv_homo)))) %>%
        mutate(t70_cour_need_sec = round(if_else((t70_cour_req - sec_total) < 0, 0,
        (t70_cour_req - sec_total)))) %>%
        mutate(t70_cour_need_del = round(if_else((t70_cour_req - del_cour_total) < 0,0,
        (t70_cour_req - del_cour_total)
        ))) %>%
        mutate(t70_cour_need_adm = round(if_else((t70_cour_req - adm_fv_homo) < 0, 0,
        (t70_cour_req - adm_fv_homo))))
    return(a_data)
}

course_progress <- function(a_data, b_smartsheet) {
    
    # Homogenize country coverage targets
    print(" >>>> Homogenizing country coverage targets...")
    a_data <- a_data %>%
    mutate(dp_deadline = as.Date(dp_deadline)) %>%
    mutate(ss_deadline = as.Date(ss_deadline))
    
    a_data <- a_data %>%
    mutate(ndvp_target = if_else(is.na(dp_target) == FALSE, dp_target, ss_target)) %>%
    mutate(ndvp_target_source = if_else(is.na(dp_target) == FALSE, "DP","SS")) %>%
    mutate(ndvp_deadline = if_else(ndvp_target_source == "DP", dp_deadline, ss_deadline))
    
    a_data <- a_data %>%
    mutate(ndvp_deadline = as.Date(ndvp_deadline)) %>%
    mutate(timeto_ndvp = ndvp_deadline - refresh_date) %>%
    mutate(a_pop_ndvp = a_pop * ndvp_target) %>%
    mutate(a_pop_ndvp_mid = a_pop * ndvp_mid_target)

    # Calculate progress against country coverage targets
    print(" >>> Computing progress against country coverage targets...")
    a_data <- a_data %>%
    mutate(ndvp_goalmet = if_else(cov_total_fv >= ndvp_target, "Yes", "No")) %>%
    mutate(ndvp_target_active = if_else(as.Date(ndvp_deadline) < Sys.Date(), NA_real_, ndvp_target)) %>%
    mutate(ndvp_rem = if_else((ndvp_target - cov_total_fv) < 0, 0, (ndvp_target - cov_total_fv))) %>%
    mutate(ndvp_peratpace = ((adm_fv_homo + (
        dvr_4wk_fv * as.numeric(as.Date(ndvp_deadline) - Sys.Date())
    )) / a_pop)) %>%
    mutate(ndvp_pertogo = if_else((ndvp_target - ndvp_peratpace) < 0, 0, (ndvp_target - ndvp_peratpace))) %>%
    mutate(ndvp_willmeet = if_else(ndvp_peratpace >= ndvp_target, "Yes", "No")) %>%
    mutate(ndvp_timeto = round(if_else(
        ((adm_pv + ((a_pop_ndvp - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td) < 0, 0,
        if_else(is.infinite(((adm_pv + ((a_pop_ndvp - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td)), NA_real_,
                ((adm_pv + ((a_pop_ndvp - adm_pv - adm_fv_homo) * 2)) / dvr_4wk_td))))) %>%
    mutate(ndvp_ontrack = if_else(ndvp_peratpace >= ndvp_target & cov_total_fv <= ndvp_target, "Yes", "No")) %>%
    mutate(ndvp_offtrack = if_else(ndvp_goalmet != "Yes" &
                                    ndvp_willmeet != "Yes", "Yes", "No")) %>%
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
                    ndvp_deadline < Sys.Date(),
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
                    ndvp_deadline < Sys.Date(),
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
    
    mutate(ndvp_rate_needed = if_else(
        (((a_pop * ndvp_target) - adm_fv_homo) / as.numeric(ndvp_deadline - as.Date("2022-02-02"))) < 0, 0,
        (((a_pop * ndvp_target) - adm_fv_homo) / as.numeric(ndvp_deadline - as.Date("2022-02-02"))))) %>%
    mutate(ndvp_rate_needed_dose = if_else(
            ((adm_pv + ((a_pop_ndvp - adm_pv - adm_fv_homo) * 2)) / as.numeric(ndvp_deadline - refresh_date)) < 0, 0,
            ((adm_pv + ((a_pop_ndvp - adm_pv - adm_fv_homo) * 2)) / as.numeric(ndvp_deadline - refresh_date)))) %>%
    mutate(ndvp_scaleup = if_else(is.infinite(round((ndvp_rate_needed / dvr_4wk_fv),2)),NA_real_,
                                  round(ndvp_rate_needed / dvr_4wk_fv),2)) %>%
    mutate(ndvp_scaleup_dose = if_else(is.infinite(round((ndvp_rate_needed_dose / dvr_4wk_td),2)), NA_real_,
                                       round(ndvp_rate_needed_dose / dvr_4wk_td),2))
    #FIXME -1 & 100 set as min and max value. Find a way to get min & max value than hard coding it
    breaks <- c(-1, 0, 2, 5, 10, 100)
    tags <- c("1) Goal met", "2) <2x", "3) 3-5x", "4) 5-10x", "5) >10x")
    a_data$ndvp_scaleup_cat <- cut(
        a_data$ndvp_scaleup_dose,
        breaks = breaks,
        include.lowest = TRUE,
        right = FALSE,
        labels = tags
    )
    
    # Progress against mid year targets
    print(" >>> Getting progress against mid year targets...")
    a_data <- a_data %>%
    mutate(ndvp_mid_rem = if_else((ndvp_mid_target - cov_total_fv) < 0, 0, (ndvp_mid_target - cov_total_fv))) %>%
    mutate(ndvp_mid_peratpace = ((adm_fv_homo + (
    dvr_4wk_fv * as.numeric(as.Date("2022-06-30") - refresh_date)
    )) / a_pop)) %>%
    mutate(ndvp_mid_pertogo = if_else((ndvp_mid_target - ndvp_mid_peratpace) < 0, 0, (ndvp_mid_target - ndvp_mid_peratpace)))  %>%
    mutate(ndvp_mid_rate_needed = if_else(
        (((a_pop * ndvp_mid_target) - adm_fv_homo) / timeto_t70) < 0, 0,
        (((a_pop * ndvp_mid_target) - adm_fv_homo) / timeto_t70))) %>%
    mutate(ndvp_mid_rate_needed_dose = if_else(is.na(jj_policy),
    if_else(
    ((adm_pv + (if_else((a_pop_ndvp_mid - adm_pv - adm_fv_homo) < 0, 0, (a_pop_ndvp_mid - adm_pv - adm_fv_homo)) * 2)) / timeto_t70) < 0, 0,
    ((adm_pv + (if_else((a_pop_ndvp_mid - adm_pv - adm_fv_homo) < 0, 0, (a_pop_ndvp_mid - adm_pv - adm_fv_homo)) * 2)) / timeto_t70)
    ),
    if_else(jj_policy == "One",
    if_else(
    ((adm_pv + (if_else((a_pop_ndvp_mid - adm_pv - adm_fv_homo) < 0, 0, (a_pop_ndvp_mid - adm_pv - adm_fv_homo)) * 2 * (1 - del_dose_jj_prop)) + (if_else((a_pop_ndvp_mid - adm_pv - adm_fv_homo) < 0, 0, (a_pop_ndvp_mid - adm_pv - adm_fv_homo)) * del_dose_jj_prop)) / timeto_t70) < 0, 0,
    ((adm_pv + (if_else((a_pop_ndvp_mid - adm_pv - adm_fv_homo) < 0, 0, (a_pop_ndvp_mid - adm_pv - adm_fv_homo)) * 2 * (1 - del_dose_jj_prop)) + (if_else((a_pop_ndvp_mid - adm_pv - adm_fv_homo) < 0, 0, (a_pop_ndvp_mid - adm_pv - adm_fv_homo)) * del_dose_jj_prop)) / timeto_t70) 
    ),
    NA_real_)
    )) %>%
    mutate(ndvp_mid_scaleup = if_else(is.infinite(round((ndvp_mid_rate_needed / dvr_4wk_fv),2)),NA_real_,
    round(ndvp_mid_rate_needed / dvr_4wk_fv),2)) %>%
    mutate(ndvp_mid_scaleup_dose = if_else(is.infinite(round((ndvp_mid_rate_needed_dose / dvr_4wk_td),2)), NA_real_,
        round(ndvp_mid_rate_needed_dose / dvr_4wk_td),2)) %>%
    mutate(ndvp_mid_rate_needed_homo = if_else(is.na(ndvp_mid_rep_rate), ndvp_mid_rate_needed_dose, ndvp_mid_rep_rate)) %>%
    mutate(ndvp_scaleup_dose_homo = if_else(is.infinite(round((ndvp_mid_rate_needed_homo / dvr_4wk_td),2)), NA_real_,
        round(ndvp_mid_rate_needed_homo / dvr_4wk_td),2)) %>%
    mutate(ndvp_scaleup_cat_kpi = if_else(is.na(ndvp_mid_scaleup_dose), NA_character_,
    if_else(ndvp_mid_scaleup_dose < 2,
    "High",
    if_else(
    ndvp_mid_scaleup_dose < 10,
    "Medium",
    if_else(
    ndvp_mid_scaleup_dose >= 10,
    "Low",
    NA_character_)
    ))))
    return(a_data)
}

course_add_notes <- function(a_data, b_csl) {
    # Add notes
    a_data <- a_data %>%
    mutate(note_highcov = if_else(cov_total_fv > 0.5, "High fully vaccinated coverage", "No")) %>%
    mutate(note_recent_rollout = if_else(intro_date > (Sys.Date() - 60), "Recent rollout", "No")) %>%
    mutate(note_reporting_date = if_else(
        adm_date < (refresh_date - 10),
        "Likely reporting issue",
        NA_character_
    )) %>%
    mutate(
        note_drivers_auto = if_else(
            note_reporting_date == "Likely reporting issue",
            "Likely reporting issue",
            if_else(
                note_nochange == 1,
                "Likely reporting issue",
                if_else(
                    dvr_4wk_td_per < 0,
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
                )
            )
        ) %>%
    mutate(note_supplyconstraint = if_else(rem_cour_del_per < 0.05 &
                                           pu_used_per > 0.8, 1, 0)) %>%
    
    mutate(note_supplyneed = if_else(rem_cour_del_per < 0.05 &
                                             pu_used_per > 0.8, "Yes", "No"))    
    
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

    # Sort columns
    a_data <- a_data %>%
    select("a_iso", sort(colnames(.)))

    # Add refresh date
    refresh_date <- as.Date("2022-03-30")
    a_data$a_refresh_date <- refresh_date

    return(a_data)

}

# Create AMC summary table
amc_covax_status <- function(a_data) {
    a_data_amc <- filter(a_data, a_covax_status == "AMC")
    return(a_data_amc)
}

hic_income_group <- function(a_data) {
    a_data_hic <- filter(a_data, a_income_group == "HIC")
    return(a_data_hic)
}

covdp_csl_status <- function(a_data) {
    a_data_csl <- filter(a_data, a_csl_status == "Concerted support country")
    return(a_data_csl)
}

covdp_ifc_status <- function(a_data) {
    a_data_ifc <- filter(a_data, a_ifc_status == "Immediate focus")
    return(a_data_ifc)
}

africa_continent <- function(a_data) {
    a_data_africa <- filter(a_data, a_continent == "Africa" & intro_status == "Product introduced")
    return(a_data_africa)
}