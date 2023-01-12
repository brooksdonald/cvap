merge_a_data_details <- function(a_data, delivery_courses_doses) {
  # Calculate delivered courses as percent of population
  a_data <- a_data %>%
    mutate(del_cour_total_per = del_cour_total / a_pop) %>%
    mutate(del_cour_since_lm_per = del_cour_since_lm / a_pop) %>%
    mutate(del_wast_per = del_cour_wast / a_pop)

  # Assign received courses category
  breaks <- c(0, 0.25, 0.5, 0.75, 1, 10)
  tags <- c("0) <25%", "1) 25-49%", "2) 50-74%", "3) 75-100%", "4) >100%")
  a_data$del_cour_total_per_cat <- cut(
    a_data$del_cour_total_per,
    breaks = breaks,
    include.lowest = TRUE,
    right = FALSE,
    labels = tags
  )

  # Calculate supply received proportions of total
  a_data <- a_data %>%
    mutate(del_cour_covax_prop = del_cour_covax / del_cour_total) %>%
    mutate(del_cour_since_lm_prop = del_cour_since_lm / del_cour_total)

  # Calculate supply influx status
  a_data <- a_data %>%
    mutate(del_influx_status = if_else(del_cour_since_lm_per >= 0.1,
    "Yes", "No"))

  return(a_data)
}
