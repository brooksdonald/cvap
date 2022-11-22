
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
      "Total.doses.administered",	
      "Total.doses.administered.per.100.pop.",
      "Persons.fully.vaccinated.per.100.pop.",
      "Total.People.in.Need"
    )
  )
  
  print(" >> Renaming columns...")
  colnames(population_pin) <- c(
    "a_iso",
    "a_name_short",
    "a_district_sub",
    "num_healthclusterpartners",
    "adm_td_pin",
    "adm_td_pin_per",
    "cov_pin_fv",
    "a_pop_pin"
  )
  return(population_pin)
}

load_pin_data <- function(population_pin) {
  temp_pin_hcw <- rank_bin_env$a_data
  
  
  temp_pin_hcw <- select(
    temp_pin_hcw,
    c("a_iso","a_pop_hcw")
  )
  
  population_pin <- left_join(population_pin, temp_pin_hcw, by = "a_iso")
  
  return(population_pin)
  
}



