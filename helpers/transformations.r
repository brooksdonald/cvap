
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

helper_goal_target_groups <- function(a_data, group, timeto_t70) {
    a_pop_var <- paste0("a_pop_", group)
    appendix <- if (group == 10 | group == "10") {
        "sep"
    } else {
        "dec"
    }
    txx_rate_needed_30jun <- paste0("t", group, "_rate_needed_30jun")
    a_data <- a_data %>%
        mutate("t{{group}}_timeto" := round(if_else(is.infinite(
                ((adm_pv + (((!!as.name(paste0("a_pop_", group)))
                    - adm_pv - adm_fv_homo) * 2)) /
                dvr_4wk_td)) &
                ((adm_pv + ((!!as.name(paste0("a_pop_", group))
                    - adm_pv - adm_fv_homo) * 2)) /
                dvr_4wk_td > 0),
            NA_real_,
            pmax((adm_pv + ((!!as.name(paste0("a_pop_", group)) -
                    adm_pv - adm_fv_homo) * 2)) /
                    dvr_4wk_td,
                0)))) %>%
        mutate(
            "t{{group}}_rate_needed_30jun" := pmax(
                ((!!as.name(paste0("a_pop_", group)) -
                    adm_fv_homo) / timeto_t70) * 2,
            0)) %>%
        mutate("t{{group}}_scaleup_30jun" := if_else(
            is.infinite(round(
                !!as.name(txx_rate_needed_30jun) /
                    dvr_4wk_td,
                2)),
            0,
            round(!!as.name(txx_rate_needed_30jun) /
                dvr_4wk_td,
                2))) %>%
        mutate("t{{group}}_status" := if_else(
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
                    NA_character_)
        )))
    return(a_data)
}