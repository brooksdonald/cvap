financing_overview <- function(a_data) {
  
  a_data <- a_data %>%
    mutate(fund_percapita = fund_total / a_pop) %>%
    
    mutate(fund_percapita_cat = if_else(
      fund_percapita < 1,
      "1) < 1 USD",
      if_else(
        fund_percapita < 2,
        "2) 1-1.9 USD",
        if_else(
          fund_percapita < 5,
          "3) 2-4.9 USD",
          if_else(
            fund_percapita >= 5,
            "4) 5+ USD",
            NA_character_)
        )
      )
    ))
    
    # Sort columns
    a_data <- a_data %>%
    select("a_iso", sort(colnames(.)))
  
  return(a_data)

}

# Create AMC summary table
amc_covax_status <- function(a_data) {
  a_data_amc <- filter(a_data, a_status_covax == "AMC" & adm_status_intro == "Product introduced")
  return(a_data_amc)
}

hic_income_group <- function(a_data) {
  a_data_hic <- filter(a_data, a_income_group == "HIC")
  return(a_data_hic)
}

covdp_csc_status <- function(a_data) {
  a_data_csc <- filter(a_data, a_status_csc == "Concerted support country")
  return(a_data_csc)
}

africa_continent <- function(a_data) {
  a_data_africa <- filter(a_data, a_continent == "Africa" & adm_status_intro == "Product introduced")
  return(a_data_africa)
}

