course_progress <- function(a_data, entity_characteristics, date_refresh) {
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
    mutate(a_pop_ndvp = a_pop * ndvp_target)

    # Calculate progress against country coverage targets
    print(" >>> Computing progress against country coverage targets...")
    a_data <- a_data %>%
    mutate(ndvp_goalmet = if_else(
        cov_total_fv >= ndvp_target, 
        "Yes", 
        "No")) %>%
    mutate(ndvp_target_active = if_else(
        as.Date(ndvp_deadline) < date_refresh,
        NA_real_,
        ndvp_target)) %>%
    mutate(ndvp_rem =
        pmax(ndvp_target - cov_total_fv, 0)) %>%
    mutate(ndvp_peratpace = 
        ((adm_fv_homo + (dvr_4wk_fv * 
            as.numeric(as.Date(ndvp_deadline) - date_refresh)
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
                    ndvp_deadline < date_refresh,
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
                    ndvp_deadline < date_refresh,
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
    )) 
}
