helper_tr_add_suffix_to_list <- function(l, suffix) {
    return(sprintf(paste0("%s", suffix), l))
}

helper_replace_values_with_map <- function(data, values, map,
                                           drop_rest = TRUE, na_fill = "") {
    if (!drop_rest) {
        unique_values <- unique(data)
        for (uv in unique_values) {
            if (!(uv %in% values)) {
                values <- append(values, uv)
                map <- append(map, uv)
            }
        }
    }

    dict <- data.frame(
        val = values,
        map = map
    )

    data <- dict$map[match(data, dict$val)]

    if (na_fill != "") {
        data[is.na(data)] <- na_fill
    }

    return(data)
}

helper_add_char_to_list <- function(l, char = "Y") {
    return(sprintf(paste0(char, "%s"), l))
}

helper_goal_target_groups <- function(a_data, group) {
    a_pop_var <- paste0("a_pop_", group)
    appendix <- if (group == 10 | group == "10") {
        "sep"
    } else {
        if (group == 70 | group == "70") {
            "jun"
        } else {
            "dec"
        }
    }
    # txx_rate_needed <- paste0("t", group, "_rate_needed", deadline_suffix)
    # a_data <- a_data %>%
    #     mutate(!!as.name(paste0("t", group, "_timeto")) := round(if_else(
    #         is.infinite(((adm_pv + (((!!as.name(paste0("a_pop_", group)))
    #             - adm_pv - adm_fv_homo) * 2)) /
    #             dvr_4wk_td)) &
    #             ((adm_pv + ((!!as.name(paste0("a_pop_", group))
    #                 - adm_pv - adm_fv_homo) * 2)) /
    #             dvr_4wk_td > 0),
    #         NA_real_,
    #         pmax((adm_pv + ((!!as.name(paste0("a_pop_", group)) -
    #                 adm_pv - adm_fv_homo) * 2)) /
    #                 dvr_4wk_td,
    #             0))))
    # a_data <- a_data %>%
    #     mutate(
    #         !!as.name(paste0("t", group, "_rate_needed", deadline_suffix)) :=
    #             pmax(((!!as.name(paste0("a_pop_", group)) -
    #                 adm_fv_homo) / timeto_t70) * 2,
    #         0))
    # a_data <- a_data %>%
    #     mutate(
    #         !!as.name(paste0("t", group, "_scaleup", deadline_suffix)) :=
    #         if_else(
    #             is.infinite(round(
    #                 !!as.name(txx_rate_needed) /
    #                     dvr_4wk_td,
    #                 2)),
    #             0,
    #             round(!!as.name(txx_rate_needed) /
    #                 dvr_4wk_td,
    #                 2)))
    a_data <- a_data %>%
        mutate(!!as.name(paste0("t", group, "_status")) := if_else(
            !!as.name(paste0("t", group, "_goalmet_", appendix)) == "Yes" |
                is.na(!!as.name(paste0("t", group, "_goalmet_", appendix))) &
                adm_td > 0,
            "1) Goal met by deadline",
            if_else(
                !!as.name(paste0("t", group, "_goalmet_after")) == "Yes",
                "2) Goal met after deadline",
                if_else(
                    !!as.name(paste0("t", group, "_notmet")) == "Yes",
                    "3) Goal not yet met",
                    NA_character_))))
    if (group == "70" | group == 70) {
        a_data$t70_status_vis <- a_data$t70_status
        a_data$t70_status <- substring(a_data$t70_status, first = 4)
    }
    return(a_data)
}

helper_mapping_months <- function(data, last_month, first_month = "2021-01") {
    input_months <- c(as.Date(paste(first_month,"-01",sep="")), 
        as.Date(paste(last_month,"-01",sep="")))
    month <- input_months[1]
    month_vector <- c(substr(as.character(month), 1, 7))
    number_of_months <- 1
    month <- add_with_rollback(month, months(1))
    while (month <= input_months[2]) {
        month_vector <- c(month_vector, substr(as.character(month), 1, 7))
        month <- add_with_rollback(month, months(1))
        number_of_months <- number_of_months + 1
    }
    month_names <- helper_replace_values_with_map(
        data = data,
        values = 1:number_of_months,
        map = month_vector
    )
  return(month_names)
}

helper_calculate_cov_total_fv <- function(data) {
    data <- data %>%
    mutate(
        cov_total_fv = if_else(
        adm_a1d == 0 & adm_fv == 0 & adm_booster == 0,
            (adm_td / 2) / a_pop,
            if_else(
                adm_a1d == 0 & adm_fv == 0 & adm_booster != 0,
                ((adm_td - adm_booster) / 2) / a_pop,
                if_else(
                    adm_a1d != 0 & adm_fv == 0 & adm_booster == 0,
                    (adm_td - adm_a1d) / a_pop,
                    if_else(
                        adm_a1d != 0 & adm_fv == 0 & adm_booster != 0,
                        (adm_td - adm_a1d - adm_booster) / a_pop,
                        adm_fv / a_pop
                )
            )
            )
        )
        )
    return(data)
}

helper_check_for_duplicates <- function(data) {
    data <- data %>%
        group_by(a_iso, target_group) %>%
        mutate(max_n_vacc = max(adm_fv, na.rm = T)) %>%
        arrange(a_iso, data, target_group) %>%
        filter(adm_fv == max_n_vacc | is.na(adm_fv)) %>%
        distinct(a_iso, target_group, .keep_all = TRUE) %>%
        mutate(max_n_vacc = if_else(
            max_n_vacc == -Inf,
            NA_real_,
            max_n_vacc)) %>%
        mutate(adm_fv = max_n_vacc) %>%
        select(-max_n_vacc)
    return(data)
}

helper_check_if_two_rows_were_deleted <- function(data) {
    if (colnames(data)[1] != "Countries.and.areas") {
        print("ERROR: Please do not delete the first two rows in supply_secured xlsx sheets.")#
        print("       The first two rows are skipped automatically.")
    }
}

helper_rename_KOS_to_XKX <- function(data, iso_column_name) {
    data <- data %>%
        mutate(
            !!as.name(iso_column_name) := if_else(
                !!as.name(iso_column_name) == "KOS",
                "XKX",
                !!as.name(iso_column_name)
        ))
    
    return(data)
}