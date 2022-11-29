
load_pin_data <- function() {
  print(" >> Loading people in need data...")
  population_pin <- data.frame(
    read_excel("data/input/static/base_population_pin.xlsx",
               sheet = "Data"
    )
  )
  
  print(" >> Selecting relevant columns...")
  population_pin <- select(
    population_pin,
    c("ISO",
      "Country",
      "Region",	
      "Number.of.Health.Cluster.partners",
      "Total.People.in.Need",
      "Vaccination.data.as.of",
      "Total.doses.administered",	
      "Total.doses.administered.per.100.pop.",
      "Persons.fully.vaccinated.per.100.pop."
    )
  )
  
  print(" >> Renaming columns...")
  colnames(population_pin) <- c(
    "a_iso",
    "a_name_short",
    "a_district_sub",
    "num_healthclusterpartners",
    "a_pop_pin",
    "ghc_date",
    "adm_td_pin",
    "adm_td_pin_per",
    "cov_pin_fv"
  )
  return(population_pin)
}

transform_pin_data <- function(population_pin) {
  temp_pin_hcw <- entity_env$population_hcw
  
  temp_pin_hcw <- select(
    temp_pin_hcw,
    c("a_iso","a_pop_hcw")
  )
  
  population_pin <- left_join(population_pin, temp_pin_hcw, by = "a_iso")
  
  population_pin$ghc_date_max <- max(population_pin$ghc_date)
    
  return(population_pin)
  
}



