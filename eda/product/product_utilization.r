# Product utilization


dose_utilization <- function(a_data) {
    ## Calculate remaining doses, absolute and % pop.
    a_data <- a_data %>%
    mutate(pu_del_rem = if_else(
        (if_else(
            is.na(del_dose_total) | del_dose_total == 0,
            NA_real_,
            ((del_dose_total * 0.9) - adm_td))) < 0, 0, 
                (if_else(
                    is.na(del_dose_total) | del_dose_total == 0,
                    NA_real_,
                    ((del_dose_total * 0.9) - adm_td)
                )
            )
        )
    )%>%
    
    mutate(pu_del_rem_per = pu_del_rem / a_pop) %>%
  
    mutate(pu_del_rem_timeto = if_else(is.infinite(pu_del_rem / dvr_4wk_td), NA_real_,
                                                    (pu_del_rem / dvr_4wk_td)))
        
    ## Calculate percent of doses received utilized
    a_data <- a_data %>%
    mutate(pu_del_rem_prop = if_else(
    pu_del_rem > 0,
    (pu_del_rem / del_dose_total),
    if_else(
        is.na(pu_del_rem) | pu_del_rem == 0,
        NA_real_,
            if_else(pu_del_rem <= 0, 0,
            NA_real_)
        )
    )) %>%
    
    mutate(pu_used_per = 1 - pu_del_rem_prop) 
  
    ## Assign percent utilization categories
    a_data <- a_data %>%
    mutate(pu_used_per_cat = if_else(
        pu_used_per < 0.25,
        "0) <25%",
            if_else(
                pu_used_per < 0.5,
                "1) 25-49%",
                    if_else(
                        pu_used_per < 0.75,
                        "2) 50-74%",
                            if_else(pu_used_per <= 1, "3) 75-100%",
                            NA_character_
                        )
                    )
                )
            )
        )

    return(a_data)
}

supply_pending <- function(a_data) {
    # Calculate supply secured not yet delivered, supply received not yet administered
    a_data <- a_data %>%
    mutate(sec_tobedel = if_else((sec_total - del_cour_total) < 0, 0, (sec_total - del_cour_total))) %>%
  
    mutate(sec_tobedel_per = sec_tobedel / a_pop) %>%
    
    mutate(rem_cour_del = if_else((del_cour_total - del_cour_wast - adm_fv_homo - (((adm_a1d_homo - adm_fv_homo) + adm_booster)  * 0.5)) < 0, 0,
                                    (del_cour_total - del_cour_wast - adm_fv_homo - (((adm_a1d_homo - adm_fv_homo) + adm_booster)  * 0.5)))) %>%
    
    mutate(rem_cour_del_per = rem_cour_del / a_pop) %>%
    
    mutate(rem_cour_del_prop = rem_cour_del / del_cour_total)

    return(a_data)
}

course_sufficiency <- function(a_data) {
    # Calculate proportions of courses of total
    a_data <- a_data %>%
    mutate(sec_del_prop = del_cour_total / sec_total)


    # Calculate if courses secured, received, and administered sufficient to reach targets
    a_data <- a_data %>%
    mutate(t20_suff_sec = if_else(sec_total_per >= 0.2, "Yes", "No")) %>%
    
    mutate(t20_suff_del = if_else(del_cour_total_per >= 0.2, "Yes", "No")) %>%
    
    mutate(t40_suff_sec = if_else(sec_total_per >= 0.4, "Yes", "No")) %>%
    
    mutate(t40_suff_del = if_else(del_cour_total_per >= 0.4, "Yes", "No")) %>%
    
    mutate(t70_suff_sec = if_else(sec_total_per >= 0.7, "Yes", "No")) %>%
    
    mutate(t70_suff_del = if_else(del_cour_total_per >= 0.7, "Yes", "No"))


    # Calculate absolute courses needed for reach targets
    a_data <- a_data %>%
    mutate(t20_cour_req = round((a_pop * 0.2) * 1.1)) %>%
    
    mutate(t40_cour_req = round((a_pop * 0.4) * 1.1)) %>%
    
    mutate(t70_cour_req = round((a_pop * 0.7) * 1.1))


    # Calculate remaining secured, received, and admnistered courses required for targets
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
    # Merge IMR Smartsheet data
    # TODO Implement helper function to this join
    a_data <- 
        left_join(a_data, b_smartsheet, by = c("a_iso" = "iso"))

    # Calculate progress against country coverage targets
    a_data <- a_data %>%
    mutate(ndvp_goalmet = if_else(cov_total_fv >= ndvp_target, "Yes", "No")) %>%
    
    mutate(ndvp_rem = if_else((ndvp_target - cov_total_fv) < 0, 0, (ndvp_target - cov_total_fv))) %>%
    
    mutate(ndvp_peratpace = ((adm_fv_homo + (
        dvr_4wk_fv * as.numeric(as.Date(ndvp_deadline) - Sys.Date())
    )) / a_pop)) %>%
    
    mutate(ndvp_pertogo = if_else((ndvp_target - ndvp_peratpace) < 0, 0, (ndvp_target - ndvp_peratpace)))  %>%
    
    mutate(ndvp_willmeet = if_else(ndvp_peratpace >= ndvp_target, "Yes", "No")) %>%
    
    mutate(ndvp_timeto = round(if_else((((
        ndvp_target * a_pop
    ) - adm_fv_homo) / (dvr_4wk_fv)) < 0 , 0,
    if_else(is.infinite((((ndvp_target * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
    )), NA_real_,
    (((ndvp_target * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
    ))))) %>%
    
    mutate(ndvp_ontrack = if_else(ndvp_peratpace >= ndvp_target & cov_total_fv <= ndvp_target, "Yes", "No")) %>%
    
    mutate(ndvp_offtrack = if_else(ndvp_goalmet != "Yes" &
                                    ndvp_willmeet != "Yes", "Yes", "No")) %>%
    
    mutate(ndvp_status = if_else(
        is.na(ndvp_target) | ndvp_target == 0,
        "Not captured",
        if_else(
        is.na(ndvp_deadline),
        "No timeline",
        if_else(
            ndvp_goalmet == "Yes",
            "Goal met",
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
        is.na(ndvp_deadline),
        2,
        if_else(
            ndvp_goalmet == "Yes",
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
    
    mutate(ndvp_scaleup = round((ndvp_rate_needed / dvr_4wk_fv),2)) %>%
    
    mutate(ndvp_scaleup_cat = if_else(
        ndvp_scaleup == 0,
        "1) Goal met",
        if_else(
        ndvp_scaleup <= 2,
        "2) <2x",
        if_else(
            ndvp_scaleup <= 5,
            "3) 3-5x",
            if_else(
            ndvp_scaleup <= 10,
            "4) 5-10x",
            if_else(ndvp_scaleup > 10, "5) >10x",
                    NA_character_)
            )
        )
        )
    ))

    return(a_data)

}

course_add_notes <- function(a_data, b_csl) {
    # Merge concerted support list
    a_data <- left_join(a_data, b_csl, by = c("a_iso" = "iso"))

    a_data <- a_data %>% 
    mutate(csl_status = if_else(is.na(csl_status), "Other country", csl_status)) %>% 
    mutate(csl_status_numb = if_else(csl_status == "Concerted support country", 1, NA_real_))

    # Add notes
    a_data <- a_data %>%
    mutate(note_highcov = if_else(cov_total_fv > 0.5, "High fully vaccinated coverage", "No")) %>%
    
    mutate(note_recent_rollout = if_else(intro_date > (Sys.Date() - 60), "Recent rollout", "No")) %>%
    
    mutate(note_reporting_date = if_else(
        adm_date < (as.Date("2022-01-18") - 10),
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
    pu_used_per > 0.5, 1, 0))
    
    a_data_temp <- select(
        a_data, c("a_iso","adm_fv","a_pop", "cov_total_fv","t10_status","t40_status")
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

    a_data <- a_data %>%
    mutate(
        cats_a = if_else(
            cov_total_fv < 0.1 &
            t70_willmeet == "No",
            "Country for concerted support",
            "Other country"
            )
        )
    
    # Sort columns
    a_data <- a_data %>%
    select("a_iso", sort(colnames(.)))

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
