course_add_notes <- function(a_data) {
    a_data_temp <- select(
        a_data, c("a_iso", "adm_fv", "a_pop", "cov_total_fv", "t10_status", "t40_status") #nolint
    )
    a_data <- a_data %>%
        mutate(note_drivers = note_drivers_auto)

    datalist <- list("a_data_temp" = a_data_temp,
        "a_data" = a_data)
    return(datalist)
}