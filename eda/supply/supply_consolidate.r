merge_a_data_details <- function(a_data) {
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

merge_supply_received_by_product <- function(a_data, supply_received_by_product) {
  a_data_temp <-
    select(a_data,
      c("a_iso",
        "a_income_group",
        "a_income_group_vis"
      )
    )
  
  a_data_temp <- a_data_temp %>%
    mutate(a_income_group_ind = 
             ifelse(a_income_group_vis == "2) LMIC",
                    ifelse(a_iso == "IDN" | a_iso == "IND",
                           "2) LMIC - India & Indonesia",
                           "2) LMIC excl. India & Indonesia"),
                    a_income_group_vis))
           
  supply_received_by_product<-merge(x=supply_received_by_product,y=a_data_temp, by="a_iso",all.x=TRUE)
  
 return(supply_received_by_product) 
}
  
  
  
  
  