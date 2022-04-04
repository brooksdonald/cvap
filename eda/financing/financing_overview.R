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
  
  return(a_data)

}