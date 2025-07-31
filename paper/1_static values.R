# Static value calculation - total ----------------------------------------

#WHO MS total population 
pop_world_total <- raw_data_end %>%
  filter(a_status_who == "Member State") %>%
  select(a_pop) %>%
  summarize(pop_world_total = sum(a_pop)) %>%
  pull(pop_world_total)

#WHO regional total population
pop_total_region <- raw_data_end %>%
  filter(a_status_who == "Member State") %>%
  select(a_region_who,
         a_pop) %>%
  group_by(a_region_who) %>%
  summarize(pop_total = sum(a_pop)) %>%
  deframe()

pop_total_afr <- unname(pop_total_region["AFR"])
pop_total_amr <- unname(pop_total_region["AMR"])
pop_total_emr <- unname(pop_total_region["EMR"])
pop_total_eur <- unname(pop_total_region["EUR"])
pop_total_sear <- unname(pop_total_region["SEAR"])
pop_total_wpr <- unname(pop_total_region["WPR"])

#Income group total population
pop_total_income <- raw_data_end %>%
  filter(a_status_who == "Member State") %>%
  select(a_income_group,
         a_pop) %>%
  group_by(a_income_group) %>%
  summarize(pop_total = sum(a_pop)) %>%
  deframe()

pop_total_hic <- unname(pop_total_income["HIC"])
pop_total_umic <- unname(pop_total_income["UMIC"])
pop_total_lmic <- unname(pop_total_income["LMIC"])
pop_total_lic <- unname(pop_total_income["LIC"])

#COVAX total population
pop_total_covax <- raw_data_end %>%
  filter(is.na(a_status_covax) == FALSE) %>%
  select(a_status_covax,
         a_pop) %>%
  group_by(a_status_covax) %>%
  summarize(pop_total = sum(a_pop)) %>%
  deframe()

pop_total_amc <- unname(pop_total_covax["AMC"])
pop_total_sfp <- unname(pop_total_covax["Self-financing"])
pop_total_noncovax <- unname(pop_total_covax["No"])


# MS total count
count_world <- raw_data_end %>%
  filter(a_status_who == "Member State") %>%
  summarize(count_world = n()) %>%
  pull(count_world)

#WHO regional counts
count_region <- raw_data_end %>%
  filter(a_status_who == "Member State") %>%
  select(a_region_who) %>%
  group_by(a_region_who) %>%
  summarize(count = n()) %>%
  deframe()

count_afr <- unname(count_region["AFR"])
count_amr <- unname(count_region["AMR"])
count_emr <- unname(count_region["EMR"])
count_eur <- unname(count_region["EUR"])
count_sear <- unname(count_region["SEAR"])
count_wpr <- unname(count_region["WPR"])

#Income group counts
count_income <- raw_data_end %>%
  filter(a_status_who == "Member State") %>%
  select(a_income_group) %>%
  group_by(a_income_group) %>%
  summarize(count = n()) %>%
  deframe()

count_lic <- unname(count_income["LIC"])
count_lmic <- unname(count_income["LMIC"])
count_umic <- unname(count_income["UMIC"])
count_hic <- unname(count_income["HIC"])

#COVAX counts
count_covax <- raw_data_end %>%
  select(a_status_covax) %>%
  group_by(a_status_covax) %>%
  summarize(count = n()) %>%
  deframe()

count_amc <- unname(count_covax["AMC"])
count_sfp <- unname(count_covax["Self-financing"])
count_noncovax <- unname(count_covax["No"])

# Static value calculation - older adults ---------------------------------

#WHO MS older adult population 
pop_world_old <- raw_data_end %>%
  filter(a_status_who == "Member State") %>%
  select(a_pop_old) %>%
  summarize(pop_world_old = sum(a_pop_old)) %>%
  pull(pop_world_old)


# Static value calculation - hcw ------------------------------------------

#WHO MS healthcare workers
pop_world_hcw <- raw_data_end %>%
  filter(a_status_who == "Member State") %>%
  select(a_pop_hcw) %>%
  summarize(pop_world_hcw = sum(a_pop_hcw)) %>%
  pull(pop_world_hcw)