
load_pin_data <- function() {
  print(" >> Loading people in need data...")
  population_pin <- data.frame(
    read_excel("data/input/base_humanitarian.xlsx",
               sheet = "Subnational Data"
    )
  )
  
  print(" >> Selecting relevant columns...")
  population_pin <- select(
    population_pin,
    c("Country.ISO.code",
      "Region",	
      "Population",
      "Total.People.in.Need",
      "Vaccination.data.as.of",
      "Total.doses.administered",	
      "Persons.vaccinated.with.1..dose",
      "Persons.fully.vaccinated",
      "Persons.received.booster.dose"
    )
  )
  
  print(" >> Renaming columns...")
  colnames(population_pin) <- c(
    "a_iso",
    "a_name_sub",
    "a_pop_hum",
    "a_pop_pin",
    "adm_date_hum",
    "adm_td_hum",
    "adm_a1d_hum",
    "adm_fv_hum",
    "adm_booster_hum"
  )
  return(population_pin)
}

transform_pin_data <- function(population_pin) {

  print(" >> Calculating aggregate sum for people in need variables...")
  population_pin <- population_pin %>%
    group_by(a_iso) %>%
    summarize(adm_td_hum = sum(adm_td_hum),
              a_pop_hum = sum(a_pop_hum),
              a_pop_pin = sum(a_pop_pin),
              adm_a1d_hum = sum(adm_a1d_hum),
              adm_booster_hum = sum(adm_booster_hum),
              adm_fv_hum = sum(adm_fv_hum),
              adm_date_hum = max(adm_date_hum)) %>%
    mutate(adm_date_hum = as.Date(adm_date_hum))

    
  print(" >> Function 'calculate_pin' done")
    
  return(population_pin)
  
}



