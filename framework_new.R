# COVID-19 Vaccination Analysis
# Author: BROOKS, Donald J.

# Setup -------------------------------------------------------------------

# Clear environment
rm(list = ls())
gc()
options(scipen = 999)

# Set working directory
setwd("C:/Users/brooksd/OneDrive - World Health Organization/Documents/GitHub/cvap") #Donald

# Load packages
library("tidyverse")
library("readxl")
library("writexl")
library("lubridate")
library("httr")
library("jsonlite")
library("data.table")


# Extract -----------------------------------------------------------------

# Load base data

#General
raw_data_pop <- data.frame(
  read.csv("data/input/data_export_WIISE_MT_REF_POPULATIONS_AGEGROUPS.csv"))

raw_data_pop_groups <- data.frame(
  read.csv("data/data_export_WIISE_MT_REF_POPULATIONS_AGEGROUPS.csv"))

raw_data_entity <- data.frame(
  read_excel("data/input/static/base_entitydetails.xlsx",
             sheet = "REF_COUNTRIES"))

raw_data_adhoc <- data.frame(
  read_excel("data/input/static/base_adhoc.xlsx",
             sheet = "data"))

raw_data_product_names <- data.frame(
  read.csv("data/input/data_export_WIISE_REF_COV_VACCINES.csv"))


#Quarterly
raw_data_uptake <- data.frame(
  read.csv("data/data_export_WIISE_COV_UPTAKE.csv"))

raw_data_indicators <- data.frame(
  read.csv("data/input/data_export_WIISE_COV_INDICATORS.csv"))


#Annual
raw_data_annual_coverage <- data.frame(
  read.csv("data/data_export_WIISE_AD_COVERAGES.csv"))

raw_data_policy_annual <- data.frame(
  read.csv("data/data_export_WIISE_AD_INDICATORS_POLICY.csv"))


#Miscellaneous
raw_data_archive <- data.frame(
  read_excel("data/output/output_master.xlsx",
             sheet = "1_stock"))

manual_uptake_total <- data.frame(
  read_excel("data/output/uptake_mod.xlsx", sheet = "data"))

manual_uptake_old <- data.frame(
  read_excel("data/output/uptake_old_mod.xlsx", sheet = "data"))

manual_uptake_hcw <- data.frame(
  read_excel("data/output/uptake_hcw_mod.xlsx", sheet = "data"))



# Transform - entity details ----------------------------------------------

data_entity <- raw_data_entity %>%
  select(
    CODE,
    NAMEWORKEN,
    ABREVPUBLEN,
    WHOREGIONC,
    UNICEFREGION,
    WHO_LEGAL_STATUS_TITLE,
    WBINCOMESTATUS,
  ) %>%
  rename(
    iso = CODE,
    a_name_long = NAMEWORKEN,
    a_name_short = ABREVPUBLEN,
    a_region_who = WHOREGIONC,
    a_region_unicef = UNICEFREGION,
    a_status_who = WHO_LEGAL_STATUS_TITLE,
    a_income_group = WBINCOMESTATUS
  ) %>%
  mutate(a_region_who = case_when(
    a_region_who == "AFRO" ~ "AFR",
    a_region_who == "AMRO" ~ "AMR",
    a_region_who == "EMRO" ~ "EMR",
    a_region_who == "EURO" ~ "EUR",
    a_region_who == "SEARO" ~ "SEAR",
    a_region_who == "WPRO" ~ "WPR",
    TRUE ~ NA
  ),
  a_income_group = case_when(
    a_income_group == "High income" ~ "HIC",
    a_income_group == "High income: nonOECD" ~ "HIC",
    a_income_group == "High income: OECD" ~ "HIC",
    a_income_group == "Low income" ~ "LIC",
    a_income_group == "Lower middle income" ~ "LMIC",
    a_income_group == "Upper middle income" ~ "UMIC",
    TRUE ~ NA
  ))

# Transform - adhoc details -----------------------------------------------

data_adhoc <- raw_data_adhoc %>%
  select(iso,
         pop_hcw,
         pol_old,
         pol_old_source) 

data_adhoc_long <- data_adhoc %>%
  select(iso,
         pop_hcw) %>%
  mutate(GROUP = "hcw") %>%
  filter(is.na(pop_hcw) == FALSE) 

data_adhoc_older <- data_adhoc %>%
  filter(pol_old_source == "eJRF" | 
           pol_old_source == "eJRF annual" | 
           pol_old_source == "eJRF - manual" | 
           pol_old_source == "Ministry of Health (via PAHO)" | 
           pol_old_source == "Region" | 
           pol_old_source == "Region database" | 
           pol_old_source == "Region/IST" | 
           pol_old_source == "TESSY" | 
           pol_old_source == "TESSY - multiple") %>%
  select(iso,
         pol_old)

# Transform - population --------------------------------------------------

data_pop_older <- raw_data_pop_groups %>%
  select(-WHO_REGION,
         -YEAR,
         -GENDER_FK) %>%
  spread(key = AGEGROUP_FK, value = VALUE) %>%
  left_join(., data_adhoc_older, by = c("COUNTRY_FK" = "iso")) %>%
  mutate(pop_older = case_when(
    pol_old == "45 and older" ~ GTEQ_50_YEARS,
    pol_old == "50 and older" ~ GTEQ_50_YEARS,
    pol_old == "55 and older" ~ GTEQ_55_YEARS,
    pol_old == "60 and older" ~ GTEQ_60_YEARS,
    pol_old == "65 and older" ~ GTEQ_65_YEARS,
    pol_old == "75 and older" ~ GTEQ_75_YEARS,
    TRUE ~ GTEQ_60_YEARS
  )) %>%
  select(COUNTRY_FK,
         pop_older) %>%
  rename(iso = COUNTRY_FK)

data_pop <- raw_data_pop %>%
  filter(AGEGROUP_FK == "ALL") %>%
  select(COUNTRY_FK,
         AGEGROUP_FK,
         VALUE) %>%
  spread(key = AGEGROUP_FK,
         value = VALUE) %>%
  rename(
    pop_total = ALL,
    iso = COUNTRY_FK
  ) %>% 
  left_join(., data_pop_older, by = "iso")

data_pop_long <- data_pop %>%
  gather(key = "GROUP",
         value = "population",
         -iso) %>%
  mutate(GROUP = substr(GROUP, 5, nchar(GROUP)),
         GROUP = case_when(
           GROUP == "older" ~ "old",
           GROUP == "total" ~ "all",
           TRUE ~ GROUP
         )) 

# Merge - support details -------------------------------------------------

entity <- data_entity %>%
  left_join(., data_adhoc, by = "iso") %>%
  left_join(., data_pop, by = "iso")

entity_short <- entity %>%
  select(iso,
         a_name_long,
         a_status_who,
         a_region_who,
         a_income_group)


# Transform - uptake, annual ----------------------------------------------

data_annual_entity <- raw_data_adhoc %>%
  select(iso,
         pop_hcw) %>%
  left_join(., data_pop, by = "iso")

data_archive_cov_2023 <- raw_data_archive %>%
  filter(month_name == as.Date("2023-12-01")) %>%
  select(a_iso,
         adm_tot_a1d,
         cov_total_a1d,
         adm_a1d_old_cap,
         cov_old_a1d)

data_annual_uptake <- raw_data_annual_coverage %>%
  select(COUNTRY,
         TARGETDEFINITION,
         DOSES,
         PERCENTAGE) %>%
  mutate(TARGETDEFINITION = case_when(
    TARGETDEFINITION == "POLICY_RECOMMENDATION_ELDERLY" ~ "old",
    TARGETDEFINITION == "OTHER_GROUPS" ~ "other",
    TARGETDEFINITION == "POLICY_RECOMMENDATION_ADULTS_CHRONIC" ~ "ad_chronic",
    TARGETDEFINITION == "POLICY_RECOMMENDATION_HCW" ~ "hcw",
    TARGETDEFINITION == "TOTAL" ~ "all",
    TARGETDEFINITION == "MALE" ~ "male",
    TARGETDEFINITION == "FEMALE" ~ "female",
    TRUE ~ TARGETDEFINITION
  )) %>%
  # filter(TARGETDEFINITION == "hcw" |
  #        TARGETDEFINITION == "old" |
  #        TARGETDEFINITION == "all" |
  #        TARGETDEFINITION == "male" |
  #        TARGETDEFINITION == "female") %>%
  filter(!(is.na(DOSES) & is.na(PERCENTAGE)),
         !(DOSES == -2222 & PERCENTAGE == -2222),
         !(DOSES == -4444 & PERCENTAGE == -4444),
         !(DOSES == -2222 & PERCENTAGE == -4444),
         !(DOSES == -4444 & PERCENTAGE == -2222)) %>%
  filter(COUNTRY != "UKR",
         COUNTRY != "PNG",
         COUNTRY != "GHA",
         COUNTRY != "COG",
         COUNTRY != "LAO",
         COUNTRY != "SGP") %>%
  rename(GROUP= TARGETDEFINITION,
         COVID_VACCINE_ADM_1D = DOSES) %>%
  select(-PERCENTAGE) %>%
  mutate(DATE = as.Date("2025-01-01")) %>%
  spread(key = GROUP, value = COVID_VACCINE_ADM_1D) %>%
  mutate(all = if_else(is.na(all), male + female, all)) %>%
  rename(iso = COUNTRY,
         date = DATE,
         adm_total = all,
         adm_male = male,
         adm_female = female,
         adm_hcw = hcw,
         adm_old = old,
         adm_pw = PW,
         adm_other = other,
         adm_ad_chronic = ad_chronic) %>%
  mutate(source = "EJRF")

  # rename(iso = COUNTRY,
  #        cumulative_total_q4 = ALL,
  #        cumulative_old_q4 = OLD,
  #        cumulative_hcw_q4 = HCW) %>%
  # left_join(., data_annual_entity, by = c("iso" = "iso")) %>%
  # mutate(cov_hcw = cumulative_hcw_q4 / pop_hcw,
  #        cov_old = cumulative_old_q4 / pop_older,
  #        cov_total = cumulative_total_q4 / pop_total)

data_annual_uptake_euro <- raw_data_uptake %>%
  filter(YEAR == "2025" & REPORTING_PERIOD == "1") %>%
  select(COUNTRY,
         SOURCE,
         REPORTING_PERIOD,
         VACCINE_TYPE,
         AGEGROUP,
         TARGET_GROUP,
         N_VACC_DOSE1,
         COMMENTS) %>%
  rename(
    iso = COUNTRY,
    source = SOURCE,
    vaccine = VACCINE_TYPE,
    group = TARGET_GROUP,
    adm_dose = N_VACC_DOSE1,
    comment = COMMENTS
  ) %>%
  mutate(date = case_when(
    REPORTING_PERIOD == "1" ~ as.Date(paste0("2025","-","0", REPORTING_PERIOD,"-", "01"))
  )) %>%
  filter(adm_dose != "-2222",
         adm_dose != "-4444") %>%
  mutate(group = case_when(
    AGEGROUP == "GTEQ_60_YEARS" ~ "OLD",
    AGEGROUP == "ALL" ~ "ALL",
    TRUE ~ group
  )) %>%
  filter(AGEGROUP != "UNKNOWN") %>%
  mutate(group = case_when(
    group == "FEMALE" ~ "FEMALE",
    group == "MALE" ~ "MALE",
    group == "POLICY_RECOMMENDATION_ELDERLY_CHRONIC" ~ "older_chronic",
    group == "POLICY_RECOMMENDATION_ADULTS_CHRONIC" ~ "CHRONIC",
    group == "POLICY_RECOMMENDATION_HCW" ~ "HCW",
    group == "HW" ~ "HCW",
    group == "PW" ~ "PW",
    group == "TOTAL" ~ "ALL",
    group == "OTHER_GROUPS" ~ "OTHER",
    AGEGROUP == "GTEQ_80_YEARS" ~ "GTEQ_80_YEARS",
    AGEGROUP == "70_79_YEARS" ~ "70_79_YEARS",
    AGEGROUP == "60_69_YEARS" ~ "60_69_YEARS",
    TRUE ~ group
  )) %>%
  # Remove AGE GROUP and vaccine filed prior to summary
  select(-AGEGROUP,
         -vaccine) %>%
  
  # Summarize by country, group, and time point
  group_by(iso, group, date, source) %>%
  summarize(adm_dose = sum(adm_dose)) %>%
  ungroup() %>%
  group_by(iso, group, date) %>%
  ungroup()



data_annual_uptake_euro_iso <- data_annual_uptake_euro %>%
  filter(source == 'TESSY') %>%
  group_by(iso) %>%
  filter(!any(group == 'OLD') & any(group == '60_69_YEARS')) %>%
  ungroup() %>%
  distinct(iso) %>%
  pull(iso)

data_annual_uptake_euro_age <- data_annual_uptake_euro %>%
  filter(source == 'TESSY',
         iso %in% data_annual_uptake_euro_iso) %>%
  select(-group) %>%
  group_by(iso, date) %>%
  summarize(source = first(source),
            adm_dose = sum(adm_dose)) %>%
  mutate(group = "OLD") %>%
  ungroup()

data_annual_uptake_euro_clean <- data_annual_uptake_euro %>%
  filter(group != "60_69_YEARS",
         group != "70_79_YEARS",
         group != "GTEQ_80_YEARS") %>%
  rbind(., data_annual_uptake_euro_age) %>%
  select(-date,
         -source) %>%
  rename(COUNTRY = iso,
         TARGETDEFINITION = group,
         DOSES = adm_dose)

# Transform - uptake  -----------------------------------------------------

data_uptake <- raw_data_uptake %>%
  
  # Filter for 2024 data
  filter(YEAR == "2024" & REPORTING_PERIOD == "4" |
           YEAR == "2024" & REPORTING_PERIOD == "7" |
           YEAR == "2024" & REPORTING_PERIOD == "10" |
           YEAR == "2025" & REPORTING_PERIOD == "1") %>%
  
  # Select & rename only columns needed for further analysis
  select(COUNTRY,
         SOURCE,
         REPORTING_PERIOD,
         VACCINE_TYPE,
         AGEGROUP,
         TARGET_GROUP,
         N_VACC_DOSE1,
         COMMENTS) %>%
  rename(
    iso = COUNTRY,
    source = SOURCE,
    vaccine = VACCINE_TYPE,
    group = TARGET_GROUP,
    adm_dose = N_VACC_DOSE1,
    comment = COMMENTS
  ) %>%
  
  # Create date field based on reporting period
  mutate(date = case_when(
    REPORTING_PERIOD == "4" ~ as.Date(paste0("2024","-","0", REPORTING_PERIOD,"-", "01")),
    REPORTING_PERIOD == "7" ~ as.Date(paste0("2024","-","0", REPORTING_PERIOD,"-", "01")),
    REPORTING_PERIOD == "10" ~ as.Date(paste0("2024","-", REPORTING_PERIOD,"-", "01")),
    REPORTING_PERIOD == "1" ~ as.Date(paste0("2025","-","0", REPORTING_PERIOD,"-", "01"))
    )) %>%
  
  # Remove 'No data' or 'Not applicable' reports
  filter(adm_dose != "-2222",
         adm_dose != "-4444") %>%
  
  # Add 'EJRF' label
  mutate(source = case_when(
    source == "" ~ "EJRF",
    TRUE ~ source
  ),
  
  # Add 'group' field based on age group field
  group = case_when(
    AGEGROUP == "GTEQ_60_YEARS" ~ "older",
    AGEGROUP == "ALL" ~ "total",
    TRUE ~ group
  )) %>%
  
  # Remove UNKNOWN age group field
  filter(AGEGROUP != "UNKNOWN") %>%
  
  # Modify group field to simpler terms 
  mutate(group = case_when(
    group == "FEMALE" ~ "total_female",
    group == "MALE" ~ "total_male",
    group == "POLICY_RECOMMENDATION_ELDERLY" ~ "older",
    group == "POLICY_RECOMMENDATION_ELDERLY_CHRONIC" ~ "older_chronic",
    group == "POLICY_RECOMMENDATION_ADULTS_CHRONIC" ~ "adult_chronic",
    group == "POLICY_RECOMMENDATION_HCW" ~ "hcw",
    group == "HW" ~ "hcw",
    group == "PW" ~ "pw",
    group == "TOTAL" ~ "total",
    group == "OTHER_GROUPS" ~ "other",
    AGEGROUP == "GTEQ_80_YEARS" ~ "GTEQ_80_YEARS",
    AGEGROUP == "70_79_YEARS" ~ "70_79_YEARS",
    AGEGROUP == "60_69_YEARS" ~ "60_69_YEARS",
    TRUE ~ group
  )) %>%
  
  # Remove AGE GROUP and vaccine filed prior to summary
  select(-AGEGROUP,
         -vaccine) %>%
  
  # Summarize by country, group, and time point
  group_by(iso, group, date, source) %>%
  summarize(adm_dose = sum(adm_dose)) %>%
  ungroup() %>%
  group_by(iso, group, date) %>%
  
  # Select TESSY data over EJRF data when both are available
  filter(if (any(source == "TESSY") & any(source == "EJRF")) 
    source == "TESSY" else TRUE) %>%
  ungroup()

# Summarize EURO countries that do not report GTEQ_60_YEARS data
data_uptake_euro_iso <- data_uptake %>%
  filter(source == 'TESSY') %>%
  group_by(iso) %>%
  filter(!any(group == 'older') & any(group == '60_69_YEARS')) %>%
  ungroup() %>%
  distinct(iso) %>%
  pull(iso)

data_uptake_euro <- data_uptake %>%
  filter(source == 'TESSY',
         iso %in% data_uptake_euro_iso) %>%
  select(-group) %>%
  group_by(iso, date) %>%
  summarize(source = first(source),
            adm_dose = sum(adm_dose)) %>%
  mutate(group = "older")

# Merge uptake dataframe with summarized EURO data
data_uptake_clean <- data_uptake %>%
  filter(group != "60_69_YEARS",
         group != "70_79_YEARS",
         group != "GTEQ_80_YEARS") %>%
  rbind(., data_uptake_euro)

# Create wide-form uptake dataframe
data_uptake_wide <- data_uptake_clean %>%
  spread(key = group,
         value = adm_dose) %>%
  rename(
    adm_female = total_female,
    adm_male = total_male,
    adm_old = older,
    adm_old_chronic = older_chronic,
    adm_ad_chronic = adult_chronic,
    adm_hcw = hcw,
    adm_pw = pw,
    adm_total = total,
    adm_other = other
  ) %>%
  
  # Copy adm_old_chronic to adm_ad_chornic and remove adm_old_chronic field
  mutate(adm_ad_chronic = if_else(is.na(adm_ad_chronic), 
                                  adm_old_chronic, 
                                  adm_ad_chronic)) %>%
  select(-adm_old_chronic) %>%
  
  # Calculate 'total' from 'male' and 'female' if reported (if one or both)
  mutate(adm_total = if_else(is.na(adm_total), 
                             rowSums(across(c(adm_female, adm_male)), 
                                     na.rm = TRUE), 
                             adm_total)) %>%
  
  # Exclude countries whose data are unreasonable
  filter(iso != "USA") %>%
  
  # Combine with q4 (annual) uptake wide dataframe
  rbind(., data_annual_uptake)

# Create long-form uptake dataframe
data_uptake_long <- data_uptake_wide %>%
  group_by(iso, date) %>%
  filter(!(all(c("WPR", "EJRF") %in% source) & source == "EJRF"),
         !(all(c("TESSY", "EJRF") %in% source) & source == "EJRF")) %>%
  ungroup() %>%
  select(-source) %>%
  gather(key = "GROUP",
         value = "COVID_VACCINE_ADM_1D",
         -iso, -date) %>%
  rename(COUNTRY = "iso",
         DATE = "date") %>%
  mutate(GROUP = substr(GROUP, 5, nchar(GROUP))) %>%
  filter(is.na(COVID_VACCINE_ADM_1D) == FALSE) %>%
  mutate(GROUP = case_when(
    GROUP == "total" ~ "all",
    TRUE ~ GROUP
  ))


# Manipulate - cumulative uptake, total ------------------------------------------

data_uptake_wide_cum_total_pre_mod <- data_uptake_wide %>%
  select(iso,
         source,
         date, 
         adm_total) %>%
  spread(key = date,
         value = adm_total) %>%
  rename(adm_total_q1 = '2024-04-01',
         adm_total_q2 = '2024-07-01',
         adm_total_q3 = '2024-10-01',
         adm_total_q4 = '2025-01-01')

data_uptake_wide_cum_total <- data_uptake_wide_cum_total_pre_mod %>%
  group_by(iso) %>%
  mutate(adm_total_q1 = case_when(
    iso == "UKR" & source == "TESSY" ~ 6044,
    TRUE ~ adm_total_q1
  ),
  adm_total_q2 = case_when(
    iso == "LVA" & source == "TESSY" ~ 110,
    TRUE ~ adm_total_q2
  )) %>%
  filter(if (any(source == "WPR") & any(source == "EJRF")) source == "EJRF" else TRUE,
         !(iso == "MDA" & source == "TESSY"),
         !(iso == "PRT" & source == "EJRF"),
         !(iso == "UKR" & source == "EJRF"),
         !(iso == "LVA" & source == "EJRF")) %>%
  mutate(test_cum_q21 = adm_total_q2 - adm_total_q1,
         test_cum_q32 = adm_total_q3 - adm_total_q2,
         test_cum_q43 = adm_total_q4 - adm_total_q3,
         test_cum_q31 = adm_total_q3 - adm_total_q1,
         test_cum_q42 = adm_total_q4 - adm_total_q2,
         test_cum_q41 = adm_total_q4 - adm_total_q1,
         test_cum_q312 = test_cum_q31 - adm_total_q2) %>%
  mutate(q2_stat = case_when(
           source == "TESSY" ~ "cum",
           test_cum_q21 < 0 ~ "add",
           test_cum_q21 >= 0 ~ "cum",
           TRUE ~ "cum"
         ),
         q3_stat = case_when(
           source == "TESSY" ~ "cum",
           test_cum_q31 < 0 ~ "add",
           test_cum_q31 >= 0 & test_cum_q21 >= 0 ~ "cum",
           test_cum_q31 > 0 & is.na(test_cum_q32) ~ "cum",
           is.na(adm_total_q1) & is.na(adm_total_q2) & is.na(adm_total_q3) ~ "cum",
           TRUE ~ "cum"
         ),
         q4_stat = "cum",
         adm_total_q1 = case_when(test_cum_q41 < 0 ~ NA, TRUE ~ adm_total_q1),
         adm_total_q2 = case_when(test_cum_q41 < 0 ~ NA, TRUE ~ adm_total_q2),
         adm_total_q2 = case_when(test_cum_q312 < 0 &
                                    q2_stat == "add" ~ NA, TRUE ~ adm_total_q2),
         adm_total_q3 = case_when(test_cum_q43 < 0 ~ NA, TRUE ~ adm_total_q3)) %>%
  filter(!(iso == "ISL" & adm_total_q4 == 930),
         !(iso == "NLD" & adm_total_q4 == 2535842))

rep_total_q4 <- data_uptake_wide_cum_total %>%
  select(iso,
         adm_total_q4) %>%
  filter(!is.na(adm_total_q4)) %>%
  mutate(stat_total_q4 = "rep") %>%
  select(-adm_total_q4)
  
  #   test_cum_q2 < 0 & test_cum_q3 < 0 ~ "add",
  #   test_cum_q2 > 0 & test_cum_tot > 0 ~ "cum",
  #   test_cum_q2 < 0 & test_cum_q3 > 0 & test_cum_tot > 0 ~ "cum",
  #   is.na(test_cum_q2) & test_cum_q3 > 0 ~ "cum",
  #   test_cum_q2 > 0 & is.na(test_cum_q3) ~ "cum",
  #   is.na(adm_total_q2) & is.na(adm_total_q3) ~ "cum",
  #   test_cum_q3 == 0 & is.na(test_cum_tot) ~ "cum",
  #   test_cum_q2 == 0 & test_cum_tot == 0 ~ "cum",
  #   test_cum_tot < 0 ~ "add",
  #   is.na(adm_total_q1) & is.na(adm_total_q2) ~ "cum",
  #   test_cum_q2 < 0 & is.na(test_cum_tot) ~ "add",
  #   adm_total_q1 == 0 & adm_total_q2 == 0 & is.na(adm_total_q3) ~ "cum",
  #   is.na(adm_total_q1) & adm_total_q2 == 0 & is.na(adm_total_q3) ~ "cum",
  #   test_cum_q2 == 0 & is.na(test_cum_q3) ~ "add",
  #   iso == "CUB" ~ "cum",
  #   TRUE ~ NA
  # ))

data_uptake_cum_total <- data_uptake_wide_cum_total %>%
  mutate(cumulative_total_q2 = case_when(
    q2_stat == "add" ~ sum(adm_total_q1, adm_total_q2, na.rm = TRUE),
    is.na(adm_total_q2) & !is.na(adm_total_q1) ~ adm_total_q1,
    TRUE ~ adm_total_q2)) %>%
  mutate(cumulative_total_q3 = case_when(
      q3_stat == "add" ~ sum(cumulative_total_q2, adm_total_q3, na.rm = TRUE),
      is.na(adm_total_q3) & !is.na(cumulative_total_q2) ~ cumulative_total_q2,
      q3_stat == "cum" ~ adm_total_q3,
      TRUE ~ NA
    )) %>%
  mutate(cumulative_total_q4 = case_when(
    is.na(adm_total_q4) & !is.na(cumulative_total_q3) ~ cumulative_total_q3,
    TRUE ~ adm_total_q4
  )) %>%
  filter(!(is.na(adm_total_q1) & is.na(adm_total_q2) & is.na(adm_total_q3) & is.na(adm_total_q4))) %>%
  select(iso,
         adm_total_q1,
         cumulative_total_q2,
         cumulative_total_q3,
         cumulative_total_q4) %>%
  mutate(
    adm_total_q4 = case_when(
      !is.na(cumulative_total_q4) & !is.na(cumulative_total_q3) ~ cumulative_total_q4 - cumulative_total_q3,
      !is.na(cumulative_total_q4) & is.na(cumulative_total_q3) ~ cumulative_total_q4,
      is.na(cumulative_total_q4) & !is.na(cumulative_total_q3) ~ -cumulative_total_q3,
      TRUE ~ NA_real_
    ),
    adm_total_q3 = case_when(
      !is.na(cumulative_total_q3) & !is.na(cumulative_total_q2) ~ cumulative_total_q3 - cumulative_total_q2,
      !is.na(cumulative_total_q3) & is.na(cumulative_total_q2) ~ cumulative_total_q3,
      is.na(cumulative_total_q3) & !is.na(cumulative_total_q2) ~ -cumulative_total_q2,
      TRUE ~ NA_real_
    ),
    adm_total_q2 = case_when(
      !is.na(cumulative_total_q2) & !is.na(adm_total_q1) ~ cumulative_total_q2 - adm_total_q1,
      !is.na(cumulative_total_q2) & is.na(adm_total_q1) ~ cumulative_total_q2,
      is.na(cumulative_total_q2) & !is.na(adm_total_q1) ~ -adm_total_q1,
      TRUE ~ NA_real_
    )
  ) 
  
  
  
  
  # filter(label == "cum") %>%
  # mutate(adm_total_q3 = case_when(
  #   is.na(adm_total_q3) & is.na(adm_total_q2) == FALSE ~ adm_total_q2,
  #   is.na(adm_total_q2) & is.na(adm_total_q3) & is.na(adm_total_q1) == FALSE ~ adm_total_q1,
  #   TRUE ~ adm_total_q3
  # )) %>%
  # rename(cumulative_total_q3 = adm_total_q3) %>%
  # select(-test_cum_q2,
  #        -test_cum_q3,
  #        -test_cum_tot,
  #        -label)

# data_uptake_add_total <- data_uptake_wide_cum_total %>%
#   filter(label == "add") %>%
#   mutate(cumulative_total_q3 = sum(adm_total_q1, adm_total_q2, adm_total_q3, na.rm = TRUE)) %>%
#   select(-test_cum_q2,
#          -test_cum_q3,
#          -test_cum_tot,
#          -label)
# 
# data_uptake_wide_total_clean <- rbind(data_uptake_add_total, data_uptake_cum_total) %>%
#   # mutate(cumulative_total_q2 = case_when(
#   #   is.na(cumulative_total_q2) == TRUE ~ sum(adm_total_q1, adm_total_q2, na.rm = TRUE),
#   #   TRUE ~ cumulative_total_q2
#   # )) %>%
#   select(-source)
#   


# Manipulate - cumulative uptake, older -----------------------------------

data_uptake_wide_cum_old_premod <- data_uptake_wide %>%
  select(iso,
         source,
         date, 
         adm_old) %>%
  spread(key = date,
         value = adm_old) %>%
  rename(adm_old_q1 = '2024-04-01',
         adm_old_q2 = '2024-07-01',
         adm_old_q3 = '2024-10-01',
         adm_old_q4 = '2025-01-01')

data_uptake_wide_cum_old <- data_uptake_wide_cum_old_premod %>%
  group_by(iso) %>%
  mutate(adm_old_q1 = case_when(
    iso == "MDA" & source == "TESSY" ~ 488,
    TRUE ~ adm_old_q1
  ),
  adm_old_q2 = case_when(
    iso == "LVA" & source == "TESSY" ~ 31,
    TRUE ~ adm_old_q2
  )) %>%
  filter(if (any(source == "WPR") & any(source == "EJRF")) source == "EJRF" else TRUE,
         !(iso == "MDA" & source == "EJRF"),
         !(iso == "LVA" & source == "EJRF"),
         !(iso == "PRT" & source == "EJRF"),
         !(iso == "UKR" & source == "EJRF")) %>%
  mutate(test_cum_q21 = adm_old_q2 - adm_old_q1,
         test_cum_q32 = adm_old_q3 - adm_old_q2,
         test_cum_q43 = adm_old_q4 - adm_old_q3,
         test_cum_q31 = adm_old_q3 - adm_old_q1,
         test_cum_q42 = adm_old_q4 - adm_old_q2,
         test_cum_q41 = adm_old_q4 - adm_old_q1,
         test_cum_q312 = test_cum_q31 - adm_old_q2) %>%
  mutate(q2_stat = case_when(
           source == "TESSY" ~ "cum",
           test_cum_q21 < 0 ~ "add",
           test_cum_q21 >= 0 ~ "cum",
           TRUE ~ "cum"
         ),
         q3_stat = case_when(
           source == "TESSY" ~ "cum",
           test_cum_q31 < 0 ~ "add",
           test_cum_q31 >= 0 & test_cum_q21 >= 0 ~ "cum",
           test_cum_q31 > 0 & is.na(test_cum_q32) ~ "cum",
           is.na(adm_old_q1) & is.na(adm_old_q2) & is.na(adm_old_q3) ~ "cum",
           TRUE ~ "cum"
         ),
         q4_stat = "cum",
         adm_old_q1 = case_when(test_cum_q41 < 0 ~ NA, TRUE ~ adm_old_q1),
         adm_old_q2 = case_when(test_cum_q312 < 0 & 
                                  q2_stat == "add" ~ NA, TRUE ~ adm_old_q2),
         adm_old_q3 = case_when(test_cum_q43 < 0 ~ NA, TRUE ~ adm_old_q3),
         adm_old_q2 = case_when(test_cum_q42 < 0 ~ NA, TRUE ~ adm_old_q2)) %>%
  filter(
    !(iso == "DNK" & adm_old_q4 == 1569052),
    !(iso == "ISL" & adm_old_q4 == 1613),
    !(iso == "NLD" & adm_old_q4 == 2342939))
  
test_test <- data_uptake_wide_cum_old %>%
  select(iso,
         adm_old_q4)

rep_old_q4 <- data_uptake_wide_cum_old %>%
  select(iso,
         adm_old_q4) %>%
  filter(!is.na(adm_old_q4)) %>%
  mutate(stat_old_q4 = "rep") %>%
  select(-adm_old_q4)

rep_old_q1 <- data_uptake_wide_cum_old %>%
  select(iso,
         adm_old_q1) %>%
  filter(!is.na(adm_old_q1)) %>%
  mutate(stat_old_q1 = "rep") %>%
  select(-adm_old_q1) %>%
  mutate(month_name = "2024-1") %>%
  rename(stat = stat_old_q1)

rep_old_q2 <- data_uptake_wide_cum_old %>%
  select(iso,
         adm_old_q2) %>%
  filter(!is.na(adm_old_q2)) %>%
  mutate(stat_old_q2 = "rep") %>%
  select(-adm_old_q2) %>%
  mutate(month_name = "2024-2") %>%
  rename(stat = stat_old_q2)

rep_old_q3 <- data_uptake_wide_cum_old %>%
  select(iso,
         adm_old_q3) %>%
  filter(!is.na(adm_old_q3)) %>%
  mutate(stat_old_q3 = "rep") %>%
  select(-adm_old_q3) %>%
  mutate(month_name = "2024-3") %>%
  rename(stat = stat_old_q3)

rep_old_q4_2 <- data_uptake_wide_cum_old %>%
  select(iso,
         adm_old_q4) %>%
  filter(!is.na(adm_old_q4)) %>%
  mutate(stat_old_q4 = "rep") %>%
  select(-adm_old_q4) %>%
  mutate(month_name = "2024-4")%>%
  rename(stat = stat_old_q4)

rep_old <- rbind(rep_old_q1, rep_old_q2, rep_old_q3, rep_old_q4_2)  

  # mutate(test_cum_q2 = adm_old_q2 - adm_old_q1,
  #        test_cum_q3 = adm_old_q3 - adm_old_q2,
  #        test_cum_tot = adm_old_q3 - adm_old_q1) %>%
  # mutate(label = case_when(
  #   test_cum_q2 < 0 & test_cum_q3 < 0 ~ "add",
  #   test_cum_q2 > 0 & test_cum_tot > 0 ~ "cum",
  #   test_cum_q2 < 0 & test_cum_q3 > 0 & test_cum_tot > 0 ~ "cum",
  #   is.na(test_cum_q2) & test_cum_q3 > 0 ~ "cum",
  #   test_cum_q2 > 0 & is.na(test_cum_q3) ~ "cum",
  #   is.na(adm_old_q2) & is.na(adm_old_q3) ~ "cum",
  #   test_cum_q3 == 0 & is.na(test_cum_tot) ~ "cum",
  #   test_cum_q2 == 0 & test_cum_tot == 0 ~ "cum",
  #   test_cum_tot < 0 ~ "add",
  #   is.na(adm_old_q1) & is.na(adm_old_q2) ~ "cum",
  #   test_cum_q2 < 0 & is.na(test_cum_tot) ~ "add",
  #   adm_old_q1 == 0 & adm_old_q2 == 0 & is.na(adm_old_q3) ~ "cum",
  #   is.na(adm_old_q1) & adm_old_q2 == 0 & is.na(adm_old_q3) ~ "cum",
  #   test_cum_q2 >= 0 & is.na(test_cum_q3) ~ "add",
  #   is.na(adm_old_q1) == FALSE & is.na(adm_old_q2) & is.na(adm_old_q3) == FALSE ~ "add",
  #   TRUE ~ NA
  # ))



data_uptake_cum_old <- data_uptake_wide_cum_old %>%
  mutate(cumulative_old_q2 = case_when(
    q2_stat == "add" ~ sum(adm_old_q1, adm_old_q2, na.rm = TRUE),
    is.na(adm_old_q2) & !is.na(adm_old_q1) ~ adm_old_q1,
    TRUE ~ adm_old_q2)) %>%
  mutate(cumulative_old_q3 = case_when(
    q3_stat == "add" ~ sum(cumulative_old_q2, adm_old_q3, na.rm = TRUE),
    is.na(adm_old_q3) & !is.na(cumulative_old_q2) ~ cumulative_old_q2,
    q3_stat == "cum" ~ adm_old_q3,
    TRUE ~ NA
  )) %>%
  mutate(cumulative_old_q4 = case_when(
    is.na(adm_old_q4) & !is.na(cumulative_old_q3) ~ cumulative_old_q3,
    TRUE ~ adm_old_q4
  )) %>%
  filter(!(is.na(adm_old_q1) & is.na(adm_old_q2) & is.na(adm_old_q3) & is.na(adm_old_q4))) %>%
  select(iso,
         adm_old_q1,
         cumulative_old_q2,
         cumulative_old_q3,
         cumulative_old_q4) %>%
  mutate(
    adm_old_q4 = case_when(
      !is.na(cumulative_old_q4) & !is.na(cumulative_old_q3) ~ cumulative_old_q4 - cumulative_old_q3,
      !is.na(cumulative_old_q4) & is.na(cumulative_old_q3) ~ cumulative_old_q4,
      is.na(cumulative_old_q4) & !is.na(cumulative_old_q3) ~ -cumulative_old_q3,
      TRUE ~ NA_real_
    ),
    adm_old_q3 = case_when(
      !is.na(cumulative_old_q3) & !is.na(cumulative_old_q2) ~ cumulative_old_q3 - cumulative_old_q2,
      !is.na(cumulative_old_q3) & is.na(cumulative_old_q2) ~ cumulative_old_q3,
      is.na(cumulative_old_q3) & !is.na(cumulative_old_q2) ~ -cumulative_old_q2,
      TRUE ~ NA_real_
    ),
    adm_old_q2 = case_when(
      !is.na(cumulative_old_q2) & !is.na(adm_old_q1) ~ cumulative_old_q2 - adm_old_q1,
      !is.na(cumulative_old_q2) & is.na(adm_old_q1) ~ cumulative_old_q2,
      is.na(cumulative_old_q2) & !is.na(adm_old_q1) ~ -adm_old_q1,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(cumulative_old_q4 != -4444,
         iso != "TLS")

test_old <- raw_data_archive %>%
  select(a_iso,
         month_name,
         adm_a1d_old_cap,
         adm_fv_old_cap) %>%
  filter(month_name == as.Date("2023-12-01")) %>%
  select(-month_name)

test1<- data_uptake_cum_old %>%
  left_join(., test_old, by = c("iso" = "a_iso")) %>%
  mutate(test = cumulative_old_q4/ adm_fv_old_cap)

testtest <- data_uptake_cum_old %>%
  select(iso,
         cumulative_old_q4) %>%
  full_join(., test_test, by = "iso") %>%
  mutate(verif = adm_old_q4 == cumulative_old_q4)
  
  
#   filter(label == "cum") %>%
#   mutate(adm_old_q3 = case_when(
#     is.na(adm_old_q3) & is.na(adm_old_q2) == FALSE ~ adm_old_q2,
#     is.na(adm_old_q2) & is.na(adm_old_q3) & is.na(adm_old_q1) == FALSE ~ adm_old_q1,
#     TRUE ~ adm_old_q3
#   )) %>%
#   rename(cumulative_old_q3 = adm_old_q3) %>%
#   filter(!(is.na(adm_old_q1) & is.na(adm_old_q2) & is.na(cumulative_old_q3))) %>%
#   select(-test_cum_q2,
#          -test_cum_q3,
#          -test_cum_tot,
#          -label)
# 
# data_uptake_add_old <- data_uptake_wide_cum_old %>%
#   filter(label == "add") %>%
#   mutate(cumulative_old_q3 = sum(adm_old_q1, adm_old_q2, adm_old_q3, na.rm = TRUE)) %>%
#   select(-test_cum_q2,
#          -test_cum_q3,
#          -test_cum_tot,
#          -label)
# 
# data_uptake_wide_old_clean <- rbind(data_uptake_add_old, data_uptake_cum_old) %>%
#   # mutate(cumulative_old_q2 = case_when(
#   #   is.na(cumulative_old_q2) == TRUE ~ sum(adm_old_q1, adm_old_q2, na.rm = TRUE),
#   #   TRUE ~ cumulative_old_q2
#   # )) %>%
#   # filter(!(is.na(adm_old_q1) & is.na(adm_old_q2))) %>%
#   select(-source)


# Manipulate - cumulative uptake, hcw -------------------------------------

data_uptake_wide_cum_hcw_premod <- data_uptake_wide %>%
  select(iso,
         source,
         date, 
         adm_hcw) %>%
  spread(key = date,
         value = adm_hcw) %>%
  rename(adm_hcw_q1 = '2024-04-01',
         adm_hcw_q2 = '2024-07-01',
         adm_hcw_q3 = '2024-10-01',
         adm_hcw_q4 = '2025-01-01')

data_uptake_wide_cum_hcw <- data_uptake_wide_cum_hcw_premod %>%
  group_by(iso) %>%
  mutate(adm_hcw_q1 = case_when(
    iso == "MDA" & source == "TESSY" ~ 179,
    TRUE ~ adm_hcw_q1
  ),
  adm_hcw_q2 = case_when(
    iso == "LVA" & source == "TESSY" ~ 3,
    TRUE ~ adm_hcw_q2
  )) %>%
  filter(if (any(source == "WPR") & any(source == "EJRF")) source == "EJRF" else TRUE,
         !(iso == "MDA" & source == "EJRF"),
         !(iso == "LVA" & source == "EJRF"),
         !(iso == "PRT" & source == "EJRF"),
         !(iso == "UKR" & source == "EJRF")) %>%
  mutate(test_cum_q21 = adm_hcw_q2 - adm_hcw_q1,
         test_cum_q32 = adm_hcw_q3 - adm_hcw_q2,
         test_cum_q43 = adm_hcw_q4 - adm_hcw_q3,
         test_cum_q31 = adm_hcw_q3 - adm_hcw_q1,
         test_cum_q42 = adm_hcw_q4 - adm_hcw_q2,
         test_cum_q41 = adm_hcw_q4 - adm_hcw_q1,
         test_cum_q312 = test_cum_q31 - adm_hcw_q2) %>%
  mutate(q2_stat = case_when(
    source == "TESSY" ~ "cum",
    test_cum_q21 < 0 ~ "add",
    test_cum_q21 >= 0 ~ "cum",
    TRUE ~ "cum"
  ),
  q3_stat = case_when(
    source == "TESSY" ~ "cum",
    test_cum_q31 < 0 ~ "add",
    test_cum_q31 >= 0 & test_cum_q21 >= 0 ~ "cum",
    test_cum_q31 > 0 & is.na(test_cum_q32) ~ "cum",
    is.na(adm_hcw_q1) & is.na(adm_hcw_q2) & is.na(adm_hcw_q3) ~ "cum",
    TRUE ~ "cum"
  ),
  q4_stat = "cum",
  adm_hcw_q1 = case_when(test_cum_q41 < 0 ~ NA, TRUE ~ adm_hcw_q1),
  adm_hcw_q2 = case_when(test_cum_q312 < 0 & 
                           q2_stat == "add" ~ NA, TRUE ~ adm_hcw_q2),
  adm_hcw_q3 = case_when(test_cum_q43 < 0 ~ NA, TRUE ~ adm_hcw_q3),
  adm_hcw_q2 = case_when(test_cum_q42 < 0 ~ NA, TRUE ~ adm_hcw_q2))
  
rep_hcw_q4 <- data_uptake_wide_cum_hcw %>%
  select(iso,
         adm_hcw_q4) %>%
  filter(!is.na(adm_hcw_q4)) %>%
  mutate(stat_hcw_q4 = "rep") %>%
  select(-adm_hcw_q4)

  # 
  # 
  # 
  # mutate(test_cum_q2 = adm_hcw_q2 - adm_hcw_q1,
  #        test_cum_q3 = adm_hcw_q3 - adm_hcw_q2,
  #        test_cum_tot = adm_hcw_q3 - adm_hcw_q1) %>%
  # mutate(label = case_when(
  #   test_cum_q2 < 0 & test_cum_q3 < 0 ~ "add",
  #   test_cum_q2 > 0 & test_cum_tot > 0 ~ "cum",
  #   test_cum_q2 < 0 & test_cum_q3 > 0 & test_cum_tot > 0 ~ "cum",
  #   is.na(test_cum_q2) & test_cum_q3 > 0 ~ "cum",
  #   test_cum_q2 > 0 & is.na(test_cum_q3) ~ "cum",
  #   is.na(adm_hcw_q2) & is.na(adm_hcw_q3) ~ "cum",
  #   test_cum_q3 == 0 & is.na(test_cum_tot) ~ "cum",
  #   test_cum_q2 == 0 & test_cum_tot == 0 ~ "cum",
  #   test_cum_tot < 0 ~ "add",
  #   is.na(adm_hcw_q1) & is.na(adm_hcw_q2) ~ "cum",
  #   test_cum_q2 < 0 & is.na(test_cum_tot) ~ "add",
  #   adm_hcw_q1 == 0 & adm_hcw_q2 == 0 & is.na(adm_hcw_q3) ~ "cum",
  #   is.na(adm_hcw_q1) & adm_hcw_q2 == 0 & is.na(adm_hcw_q3) ~ "cum",
  #   test_cum_q2 >= 0 & is.na(test_cum_q3) ~ "add",
  #   is.na(adm_hcw_q1) == FALSE & is.na(adm_hcw_q2) & is.na(adm_hcw_q3) == FALSE ~ "add",
  #   TRUE ~ NA
  # ))

data_uptake_cum_hcw <- data_uptake_wide_cum_hcw %>%
  mutate(cumulative_hcw_q2 = case_when(
    q2_stat == "add" ~ sum(adm_hcw_q1, adm_hcw_q2, na.rm = TRUE),
    is.na(adm_hcw_q2) & !is.na(adm_hcw_q1) ~ adm_hcw_q1,
    TRUE ~ adm_hcw_q2)) %>%
  mutate(cumulative_hcw_q3 = case_when(
    q3_stat == "add" ~ sum(cumulative_hcw_q2, adm_hcw_q3, na.rm = TRUE),
    is.na(adm_hcw_q3) & !is.na(cumulative_hcw_q2) ~ cumulative_hcw_q2,
    q3_stat == "cum" ~ adm_hcw_q3,
    TRUE ~ NA
  )) %>%
  mutate(cumulative_hcw_q4 = case_when(
    is.na(adm_hcw_q4) & !is.na(cumulative_hcw_q3) ~ cumulative_hcw_q3,
    TRUE ~ adm_hcw_q4
  )) %>%
  filter(!(is.na(adm_hcw_q1) & is.na(adm_hcw_q2) & is.na(adm_hcw_q3) & is.na(adm_hcw_q4))) %>%
  select(iso,
         adm_hcw_q1,
         cumulative_hcw_q2,
         cumulative_hcw_q3,
         cumulative_hcw_q4) %>%
  mutate(
    adm_hcw_q4 = case_when(
      !is.na(cumulative_hcw_q4) & !is.na(cumulative_hcw_q3) ~ cumulative_hcw_q4 - cumulative_hcw_q3,
      !is.na(cumulative_hcw_q4) & is.na(cumulative_hcw_q3) ~ cumulative_hcw_q4,
      is.na(cumulative_hcw_q4) & !is.na(cumulative_hcw_q3) ~ -cumulative_hcw_q3,
      TRUE ~ NA_real_
    ),
    adm_hcw_q3 = case_when(
      !is.na(cumulative_hcw_q3) & !is.na(cumulative_hcw_q2) ~ cumulative_hcw_q3 - cumulative_hcw_q2,
      !is.na(cumulative_hcw_q3) & is.na(cumulative_hcw_q2) ~ cumulative_hcw_q3,
      is.na(cumulative_hcw_q3) & !is.na(cumulative_hcw_q2) ~ -cumulative_hcw_q2,
      TRUE ~ NA_real_
    ),
    adm_hcw_q2 = case_when(
      !is.na(cumulative_hcw_q2) & !is.na(adm_hcw_q1) ~ cumulative_hcw_q2 - adm_hcw_q1,
      !is.na(cumulative_hcw_q2) & is.na(adm_hcw_q1) ~ cumulative_hcw_q2,
      is.na(cumulative_hcw_q2) & !is.na(adm_hcw_q1) ~ -adm_hcw_q1,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(cumulative_hcw_q4 != -4444,
         iso != "TLS")

# 
# data_uptake_cum_hcw <- data_uptake_wide_cum_hcw %>%
#   filter(label == "cum") %>%
#   mutate(adm_hcw_q3 = case_when(
#     is.na(adm_hcw_q3) & is.na(adm_hcw_q2) == FALSE ~ adm_hcw_q2,
#     is.na(adm_hcw_q2) & is.na(adm_hcw_q3) & is.na(adm_hcw_q1) == FALSE ~ adm_hcw_q1,
#     TRUE ~ adm_hcw_q3
#   )) %>%
#   rename(cumulative_hcw_q3 = adm_hcw_q3) %>%
#   filter(!(is.na(adm_hcw_q1) & is.na(adm_hcw_q2) & is.na(cumulative_hcw_q3))) %>%
#   select(-test_cum_q2,
#          -test_cum_q3,
#          -test_cum_tot,
#          -label)
# 
# data_uptake_add_hcw <- data_uptake_wide_cum_hcw %>%
#   filter(label == "add") %>%
#   mutate(cumulative_hcw_q3 = sum(adm_hcw_q1, adm_hcw_q2, adm_hcw_q3, na.rm = TRUE)) %>%
#   select(-test_cum_q2,
#          -test_cum_q3,
#          -test_cum_tot,
#          -label)
# 
# data_uptake_wide_hcw_clean <- rbind(data_uptake_add_hcw, data_uptake_cum_hcw) %>%
#   # mutate(cumulative_hcw_q2 = case_when(
#   #   is.na(cumulative_hcw_q2) == TRUE ~ sum(adm_hcw_q1, adm_hcw_q2, na.rm = TRUE),
#   #   TRUE ~ cumulative_hcw_q2
#   # )) %>%
#   # filter(!(is.na(adm_hcw_q1) & is.na(adm_hcw_q2))) %>%
#   select(-source)
# 
# 

# Merge - cleaned uptake --------------------------------------------------

data_uptake_wide_cleaned <- full_join(data_uptake_cum_total, data_uptake_cum_old, by = "iso") %>%
  full_join(., data_uptake_cum_hcw, by = "iso")

# data_uptake_wide_cleaned <- full_join(manual_uptake_total, manual_uptake_old, by = "iso") %>%
  # full_join(., manual_uptake_hcw, by = "iso")


# Transform - archive uptake ----------------------------------------------

data_archive <- raw_data_archive %>%
  select(a_region_who,
         a_status_who,
         month_name,
         adm_tot_a1d_add) %>%
  filter(a_status_who == "Member State") %>%
  select(-a_status_who) %>%
  mutate(quarter = quarter(as.Date(month_name)),
         year = year(as.Date(month_name))) %>%
  select(-month_name) %>%
  group_by(a_region_who, quarter, year) %>%
  summarize(adm_a1d = sum(adm_tot_a1d_add, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date_quarter = paste(year, quarter, sep = "-")) %>%
  select(a_region_who,
         adm_a1d,
         date_quarter)

# Transform - policy, quarterly  ------------------------------------------

data_indicators <- raw_data_indicators %>%
  filter(YEAR == "2024",
         REPORTING_PERIOD == "4" | REPORTING_PERIOD == "7" |
           REPORTING_PERIOD == "10") %>%
  rename(
    iso = COUNTRY,
    source = SOURCE,
    indicator = INDCODE,
    dimension = DIMENSION2,
    value = VALUE
  ) %>%
  mutate(source = if_else(source == "", "EJRF", source))

data_policy_long = data_indicators %>%
  filter(indicator == "POLICY_RECOMMANDED_COMMENTS" |
           indicator == "POLICY_INTERVAL" |
           indicator == "POLICY_REVACCINATION" |
           indicator == "POLICY_VACCINATION") %>%
  rename(
    group = dimension
  ) %>%
  filter(value != "-2222",
         value != "-4444",
         value != "") %>%
  mutate(indicator = case_when(
    indicator == "POLICY_VACCINATION" ~ "policy_vacc",
    indicator == "POLICY_REVACCINATION" ~ "policy_revacc",
    indicator == "POLICY_INTERVAL" ~ "policy_revacc_interval",
    indicator == "POLICY_RECOMMANDED_COMMENTS" ~ "policy_comment", 
    TRUE ~ NA
  ),
  group = case_when(
    group == "POLICY_RECOMMENDATION_OTHER" ~ "Other groups",
    group == "POLICY_RECOMMENDATION_ELDERLY" ~ "Older adults",
    group == "POLICY_RECOMMENDATION_ELDERLY_CHRONIC" ~ "Older adults with chronic conditions",
    group == "POLICY_RECOMMENDATION_ADULTS" ~ "Adults",
    group == "POLICY_RECOMMENDATION_ADULTS_CHRONIC" ~ "Adults with chronic conditions",
    group == "PW" ~ "Pregnant women",
    group == "POLICY_RECOMMENDATION_HCW" ~ "Health and care workers",
    group == "POLICY_RECOMMENDATION_CA" ~ "Children & adolescents",
    group == "POLICY_RECOMMENDATION_CA_CHRONIC" ~ "Child. & ado. with chronic conditions",
    TRUE ~ NA
  ))

data_policy_wide <- data_policy_long %>%
  spread(key = indicator,
         value = value) %>%
  mutate(status_policy = case_when(
    policy_vacc == "YES" & policy_revacc == "YES" ~ "1) Periodic revaccination & primary vaccination recommended",
    is.na(policy_vacc) & policy_revacc == "YES" ~ "1) Periodic revaccination & primary vaccination recommended",
    policy_vacc == "YES" & policy_revacc == "NO" ~ "2) Vaccination recommended",
    policy_vacc == "NO" & policy_revacc == "NO" ~ "3) Vaccination not recommended",
    policy_vacc == "NO" & policy_revacc == "" ~ "3) Vaccination not recommended",
    policy_vacc == "NO" & is.na(policy_revacc) ~ "3) Vaccination not recommended",
    is.na(policy_vacc) & is.na(policy_revacc) ~ "4) No response provided",
    policy_vacc == "YES" & is.na(policy_revacc) ~ "2) Vaccination recommended",
    is.na(policy_vacc) & policy_revacc == "NO" ~ "3) Vaccination not recommended",
    TRUE ~ NA
  ),
  ,
  status_policy_visual = if_else(status_policy == "1) Periodic revaccination & primary vaccination recommended",
                                 "Yes", NA)) %>%
  left_join(., entity_short, by = "iso")




# Transform - policy, annual ----------------------------------------------

data_entity_policy <- data_entity %>%
  filter(a_status_who == "Member State") %>%
  select(iso) %>%
  crossing(indicator = c("policy_vacc",
                         "policy_revacc",
                         "policy_revacc_interval",
                         "policy_comment"),
           group = c("Older adults",
                     "Older adults with chronic conditions",
                     "Adults",
                     "Adults with chronic conditions",
                     "Health and care workers",
                     "Children & adolescents",
                     "Child. & ado. with chronic conditions",
                     "Pregnant women",
                     "Other groups")) %>%
  mutate(YEAR = 2024,
         REPORTING_PERIOD = 12,
         source = "EJRF")
  

data_policy_annual_long <- raw_data_policy_annual %>%
  select(COUNTRY,
         YEAR,
         INDCODE,
         DIMENSION2,
         VALUE,
         SOURCE) %>%
  rename(iso = COUNTRY,
         source = SOURCE,
         group = DIMENSION2,
         value = VALUE,
         indicator = INDCODE) %>%
  filter(value != "-2222",
         value != "-4444",
         value != "") %>%
  mutate(REPORTING_PERIOD = 12,
         indicator = case_when(
           indicator == "POLICY_VACCINATION" ~ "policy_vacc",
           indicator == "POLICY_REVACCINATION" ~ "policy_revacc",
           indicator == "POLICY_INTERVAL" ~ "policy_revacc_interval",
           indicator == "POLICY_VACCINATION_COM" ~ "policy_comment", 
           TRUE ~ indicator
         ),
         group = case_when(
           group == "OTHER_GROUPS" ~ "Other groups",
           group == "POLICY_RECOMMENDATION_ELDERLY" ~ "Older adults",
           group == "POLICY_RECOMMENDATION_ELDERLY_CHRONIC" ~ "Older adults with chronic conditions",
           group == "POLICY_RECOMMENDATION_ADULTS" ~ "Adults",
           group == "POLICY_RECOMMENDATION_ADULTS_CHRONIC" ~ "Adults with chronic conditions",
           group == "PW" ~ "Pregnant women",
           group == "POLICY_RECOMMENDATION_HCW" ~ "Health and care workers",
           group == "POLICY_RECOMMENDATION_CA" ~ "Children & adolescents",
           group == "POLICY_RECOMMENDATION_CA_CHRONIC" ~ "Child. & ado. with chronic conditions",
           TRUE ~ group
         )) %>%
  right_join(., data_entity_policy, by = c("iso" = "iso",
                                          "indicator" = "indicator",
                                          "group" = "group",
                                          "YEAR" = "YEAR",
                                          "REPORTING_PERIOD" = "REPORTING_PERIOD",
                                          "source" = "source"))

data_policy_annual_wide <- data_policy_annual_long %>%
  spread(key = indicator,
         value = value) %>%
  mutate(status_policy = case_when(
    policy_vacc == "YES" & policy_revacc == "YES" ~ "1) Periodic revaccination & primary vaccination recommended",
    is.na(policy_vacc) & policy_revacc == "YES" ~ "1) Periodic revaccination & primary vaccination recommended",
    policy_vacc == "NO" & policy_revacc == "YES" ~ "1) Periodic revaccination & primary vaccination recommended",
    policy_vacc == "YES" & policy_revacc == "NO" ~ "2) Vaccination recommended",
    policy_vacc == "NO" & policy_revacc == "NO" ~ "3) Vaccination not recommended",
    policy_vacc == "NO" & policy_revacc == "" ~ "3) Vaccination not recommended",
    policy_vacc == "NO" & is.na(policy_revacc) ~ "3) Vaccination not recommended",
    is.na(policy_vacc) & is.na(policy_revacc) ~ "4) No response provided",
    policy_vacc == "YES" & is.na(policy_revacc) ~ "2) Vaccination recommended",
    is.na(policy_vacc) & policy_revacc == "NO" ~ "3) Vaccination not recommended",
    TRUE ~ NA
  ),
  status_policy_visual = if_else(status_policy == "1) Periodic revaccination & primary vaccination recommended",
                                 "Yes", NA)) %>%
  left_join(., entity_short, by = "iso")


data_policy_wide <- rbind(
  data_policy_wide,
  data_policy_annual_wide
)  %>%
  mutate(order_inc = case_when(
    a_income_group == "HIC" ~ "1) HIC",
    a_income_group == "UMIC" ~ "2) UMIC", 
    a_income_group == "LMIC" ~ "3) LMIC", 
    a_income_group == "LIC" ~ "4) LIC", 
    TRUE ~ NA),
    order_group = case_when(
      group == "Older adults" ~ 1,
      group == "Health and care workers" ~ 2,
      group == "Adults with chronic conditions" ~ 3,
      group == "Pregnant women" ~ 4,
      TRUE ~ NA
    ),
    order_interval = case_when(
      policy_revacc_interval == "12_MONTHS" ~ "1) 12 months",
      policy_revacc_interval == "6_MONTHS" ~ "2) 6 months",
      policy_revacc_interval == "OTHER" ~ "3) Other",
      TRUE ~ NA
    ))

view_ind_wiise_dash <- data_policy_long %>%
  group_by(iso, YEAR, REPORTING_PERIOD) %>%
  filter(!(all(c("WPR", "EJRF") %in% source) & source == "EJRF")) %>%
  ungroup() %>%
  rbind(., data_policy_annual_long) %>%
  select(-source) %>%
  rename(COUNTRY = iso,
         INDICATOR = indicator,
         CATEGORY = group,
         VALUE = value) %>%
  mutate(QUARTER = if_else(REPORTING_PERIOD == "4", 1,
                           if_else(REPORTING_PERIOD == "7", 2,
                                   if_else(REPORTING_PERIOD == "10", 3,
                                           if_else(REPORTING_PERIOD == "12", 4,
                                           NA))))) %>%
  mutate(DATE = case_when(
    REPORTING_PERIOD == "4" ~ as.Date(paste0("2024","-","0", REPORTING_PERIOD,"-", "01")),
    REPORTING_PERIOD == "7" ~ as.Date(paste0("2024","-","0", REPORTING_PERIOD,"-", "01")),
    REPORTING_PERIOD == "10" ~ as.Date(paste0("2024","-", REPORTING_PERIOD,"-", "01")),
    REPORTING_PERIOD == "12" ~ as.Date(paste0("2025","-", "01","-", "01"))
  )) %>%
  select(-REPORTING_PERIOD)

# Transform - products in use ---------------------------------------------

data_product_name <- raw_data_product_names %>%
  select(CODE,
         DISPLAY_NAME) %>%
  rename(product_name = DISPLAY_NAME)

data_product_long = data_indicators %>%
  filter(indicator == "VACCINATION_START_DATE" |
           indicator == "VACCINE_INUSE" |
           indicator == "NRA_COMMENTS") %>%
  rename(
    product = dimension
  ) %>%
  filter(value != "-2222",
         value != "-4444",
         value != "") %>%
  mutate(indicator = case_when(
    indicator == "VACCINATION_START_DATE" ~ "date_product_start",
    indicator == "VACCINE_INUSE" ~ "status_product_use",
    indicator == "NRA_COMMENTS" ~ "comment",
    TRUE ~ NA
  )) %>%
  left_join(., data_product_name, by = c("product" = "CODE"))



data_product_long_wiise <- data_product_long %>%
  group_by(iso, YEAR, REPORTING_PERIOD) %>%
  filter(!(all(c("WPR", "EJRF") %in% source) & source == "EJRF")) %>%
  ungroup() %>%
  select(-source) %>%
  rename(COUNTRY = iso,
         INDICATOR = indicator,
         CATEGORY = product,
         VALUE = value) %>%
  mutate(QUARTER = if_else(REPORTING_PERIOD == "4", 1,
                           if_else(REPORTING_PERIOD == "7", 2,
                                   if_else(REPORTING_PERIOD == "10", 3,
                                   NA)))) %>%
  mutate(DATE = case_when(
    REPORTING_PERIOD == "4" ~ as.Date(paste0("2024","-","0", REPORTING_PERIOD,"-", "01")),
    REPORTING_PERIOD == "7" ~ as.Date(paste0("2024","-","0", REPORTING_PERIOD,"-", "01")),
    REPORTING_PERIOD == "10" ~ as.Date(paste0("2024","-", REPORTING_PERIOD,"-", "01"))
  )) %>%
  select(-REPORTING_PERIOD,
         -product_name)

data_product_wide <- data_product_long %>%
  spread(key = indicator,
         value = value) %>%
  mutate(date_product_start = as.Date(date_product_start),
         type_formulation = case_when(
           product == "PFI-COXBB-1" ~ "XBB.1.5-adapted",
           product == "MOC-SXBB-1" ~ "XBB.1.5-adapted",
           product == "NOV-NOXBB-1" ~ "XBB.1.5-adapted",
           product == "PFI-CBOOB1-2" ~ "Omicron BA.4/BA.5-adapted",
           product == "MOC-SBOOB1-2" ~ "Omicron BA.4/BA.5-adapted",
           product == "PFI-CBOO" ~ "Omicron-adapted",
           product == "PFI-CBOOB1-1" ~ "Omicron BA.1-adapted",
           product == "MOC-SBOOB1-2" ~ "Omicron BA.1-adapted",
           product == "Fosun_Omicron_XBB1_5" ~ "XBB.1.5-adapted",
           product == "MOC-SJN-1" ~ "JN.1-adapted",
           product == "MOC-SKP-1" ~ "KP.2-adapted",
           product == "NOV-NOJN-1" ~ "JN.1-adapted",
           product == "PFI-COJN-1" ~ "JN.1-adapted",
           product == "PFI-COKP-1" ~ "KP.2-adapted",
           TRUE ~ "Ancestral"
         ),
         type_adapt = case_when(
           product == "PFI-COXBB-1" ~ "Variant-adapted",
           product == "MOC-SXBB-1" ~ "Variant-adapted",
           product == "NOV-NOXBB-1" ~ "Variant-adapted",
           product == "PFI-CBOOB1-2" ~ "Variant-adapted",
           product == "MOC-SBOOB1-2" ~ "Variant-adapted",
           product == "PFI-CBOO" ~ "Variant-adapted",
           product == "PFI-CBOOB1-1" ~ "Variant-adapted",
           product == "MOC-SBOOB1-2" ~ "Variant-adapted",
           product == "Fosun_Omicron_XBB1_5" ~ "Variant-adapted",
           product == "MOC-SJN-1" ~ "Variant-adapted",
           product == "MOC-SKP-1" ~ "Variant-adapted",
           product == "NOV-NOJN-1" ~ "Variant-adapted",
           product == "PFI-COJN-1" ~ "Variant-adapted",
           product == "PFI-COKP-1" ~ "Variant-adapted",
           TRUE ~ "Ancestral"
         ))

data_product_wide <- data_product_wide %>%
  left_join(., entity_short, by = "iso")


# Merge - Product and policy indicator view -------------------------------

view_ind_wiise_dash <- rbind(view_ind_wiise_dash, data_product_long_wiise)


# Transform - integration -------------------------------------------------

data_integration_long = data_indicators %>%
  filter(indicator == "INTEGRATION_YES_ANSWER" |
           indicator == "INTEGRATION_OTHER") %>%
  rename(integ_area = dimension) %>%
  mutate(integ_area = case_when(
    indicator == "INTEGRATION_OTHER" ~ "integ_general",
    integ_area == "INTEGRATION_HIV" ~ "integ_hiv",
    integ_area == "INTEGRATION_WASH" ~ "integ_wash",
    integ_area == "INTEGRATION_DEWORMING" ~ "integ_deworm",
    integ_area == "INTEGRATION_MALARIA" ~ "integ_malaria",
    integ_area == "INTEGRATION_PLANNING" ~ "integ_fam_plan",
    integ_area == "INTEGRATION_NUTRITION" ~ "integ_nutrition",
    integ_area == "INTEGRATION_CATCHUP" ~ "integ_catchup",
    integ_area == "INTEGRATION_NCD" ~ "integ_ncd",
    integ_area == "POLICY_RECOMMENDATION_OTHER" ~ "integ_other",
    TRUE ~ integ_area
  ),
  value = case_when(
    iso == "HKG" & integ_area == "integ_general" ~ "YES",
    TRUE ~ value
  )) %>%
  select(-indicator) %>%
  filter(value != "",
         value != "-4444",
         value != "-2222") %>%
  left_join(., entity_short, by = "iso")

data_integration_wide = data_integration_long %>%
  spread(key = integ_area,
         value = value) %>%
  left_join(., entity_short, by = "iso")




# Calculate - overall pop stats -----------------------------------------------

region_pop <- entity %>%
  filter(a_status_who == "Member State") %>%
  group_by(a_region_who) %>%
  summarize(pop_total_all = sum(pop_total, na.rm = TRUE),
            pop_older_all = sum(pop_older, na.rm = TRUE),
            pop_hcw_all = sum(pop_hcw, na.rm = TRUE),
            count_all = n())

income_pop <- entity %>%
  filter(a_status_who == "Member State",
         is.na(a_income_group) == FALSE) %>%
  group_by(a_income_group) %>%
  summarize(pop_total_all = sum(pop_total, na.rm = TRUE),
            pop_older_all = sum(pop_older, na.rm = TRUE),
            pop_hcw_all = sum(pop_hcw, na.rm = TRUE),
            count_all = n())


# Merge - country, uptake view -----------------------------------------------

uptake <- entity %>%
  left_join(., data_uptake_wide_cleaned, by = "iso")

view_uptake <- uptake %>%
  mutate(a_income_group_vis = case_when(
    a_income_group == "HIC" ~ "1) HIC",
    a_income_group == "UMIC" ~ "2) UMIC",
    a_income_group == "LMIC" ~ "3) LMIC",
    a_income_group == "LIC" ~ "4) LIC",
    TRUE ~ NA
  )) %>%
  filter(a_status_who == "Member State") %>%
  mutate(cov_older = if_else((cumulative_old_q4 / pop_older) > 1, 1, cumulative_old_q4 / pop_older),
         cov_hcw = if_else((cumulative_hcw_q4 / pop_hcw) > 1, 1, cumulative_hcw_q4 / pop_hcw),
         cov_total = if_else((cumulative_total_q4 / pop_total) > 1, 1, cumulative_total_q4 / pop_total),
         cumulative_old_q4_adj = if_else(cov_older == 1, pop_older, cumulative_old_q4),
         cumulative_hcw_q4_adj = if_else(cov_hcw == 1, pop_hcw, cumulative_hcw_q4),
         cumulative_total_q4_adj = if_else(cov_total == 1, pop_total, cumulative_total_q4),
         adm_old_q4_adj = if_else(cov_older == 1, pop_older, adm_old_q4),
         adm_hcw_q4_adj = if_else(cov_hcw == 1, pop_hcw, adm_hcw_q4),
         adm_total_q4_adj = if_else(cov_total == 1, pop_total, adm_total_q4)) %>%
  left_join(., rep_total_q4, by = "iso") %>%
  left_join(., rep_old_q4, by = "iso") %>%
  left_join(., rep_hcw_q4, by = "iso")


# Manipulate - country, uptake view WIISEmart dashboard -------------------

view_uptake_wiise_dash <- data_uptake_long %>%
  mutate(POPULATION = NA) %>%
  left_join(., data_adhoc_long, by = c("COUNTRY" = "iso", "GROUP" = "GROUP")) %>%
  left_join(., data_pop_long, by = c("COUNTRY" = "iso", "GROUP" = "GROUP" )) %>%
  mutate(POPULATION = ifelse(!is.na(pop_hcw), pop_hcw, POPULATION),
         POPULATION = ifelse(!is.na(population), population, POPULATION)) %>%
  select(-pop_hcw,
         -population) %>%
  mutate(COVID_VACCINE_COV_1D = if_else(GROUP == "hcw" | GROUP == "old", 
                                        COVID_VACCINE_ADM_1D / POPULATION,
                                        NA),
         POPULATION = if_else(GROUP == "all", NA, POPULATION),
         QUARTER = case_when(
           DATE == "2024-04-01" ~ 1,
           DATE == "2024-07-01" ~ 2,
           DATE == "2024-10-01" ~ 3,
           DATE == "2025-01-01" ~ 4,
           NA ~ TRUE
         ),
         YEAR = 2024)
  
  


# Summarize - region, uptake view -----------------------------------------

# Prepare summary by region for total uptake
uptake_region_total <- view_uptake %>%
  filter(is.na(cumulative_total_q4) == FALSE,
         a_status_who == "Member State") %>%
  group_by(a_region_who) %>%
  summarize(adm_total_q1 = sum(adm_total_q1, na.rm = TRUE),
            adm_total_q2 = sum(adm_total_q2, na.rm = TRUE),
            adm_total_q3 = sum(adm_total_q3, na.rm = TRUE),
            adm_total_q4 = sum(adm_total_q4, na.rm = TRUE),
            cumulative_total_q4 = sum(cumulative_total_q4_adj, na.rm = TRUE),
            pop_total_rep = sum(pop_total),
            count_rep_total = n()) %>%
  ungroup()

uptake_rep_count_total_q1 <- view_uptake %>%
  filter(is.na(adm_total_q1) == FALSE,
         a_status_who == "Member State") %>%
  group_by(a_region_who) %>%
  summarize(count_rep_total_q1 = n()) %>%
  ungroup()

uptake_rep_count_total_q2 <- view_uptake %>%
  filter(is.na(adm_total_q2) == FALSE,
         a_status_who == "Member State") %>%
  group_by(a_region_who) %>%
  summarize(count_rep_total_q2 = n()) %>%
  ungroup()

uptake_rep_count_total_q3 <- view_uptake %>%
  filter(is.na(adm_total_q3) == FALSE,
         a_status_who == "Member State") %>%
  group_by(a_region_who) %>%
  summarize(count_rep_total_q3 = n()) %>%
  ungroup()

uptake_rep_count_total_q4 <- view_uptake %>%
  filter(is.na(adm_total_q4) == FALSE,
         a_status_who == "Member State") %>%
  group_by(a_region_who) %>%
  summarize(count_rep_total_q4 = n()) %>%
  ungroup()

uptake_region_total <- left_join(uptake_region_total, uptake_rep_count_total_q1, by = "a_region_who") %>%
  left_join(., uptake_rep_count_total_q2, by = "a_region_who") %>%
  left_join(., uptake_rep_count_total_q3, by = "a_region_who") %>%
  left_join(., uptake_rep_count_total_q4, by = "a_region_who")



uptake_region_hcw <- view_uptake %>%
  filter(is.na(cumulative_hcw_q4) == FALSE,
         a_status_who == "Member State") %>%
  group_by(a_region_who) %>%
  summarize(adm_hcw_q1 = sum(adm_hcw_q1, na.rm = TRUE),
            adm_hcw_q2 = sum(adm_hcw_q2, na.rm = TRUE),
            adm_hcw_q3 = sum(adm_hcw_q3, na.rm = TRUE),
            adm_hcw_q4 = sum(adm_hcw_q4, na.rm = TRUE),
            cumulative_hcw_q4 = sum(cumulative_hcw_q4_adj, na.rm = TRUE),
            pop_hcw_rep = sum(pop_hcw),
            count_rep_hcw = n()) %>%
  ungroup()

uptake_rep_count_hcw_q1 <- view_uptake %>%
  filter(is.na(adm_hcw_q1) == FALSE,
         a_status_who == "Member State") %>%
  group_by(a_region_who) %>%
  summarize(count_rep_hcw_q1 = n()) %>%
  ungroup()

uptake_rep_count_hcw_q2 <- view_uptake %>%
  filter(is.na(adm_hcw_q2) == FALSE,
         a_status_who == "Member State") %>%
  group_by(a_region_who) %>%
  summarize(count_rep_hcw_q2 = n()) %>%
  ungroup()

uptake_rep_count_hcw_q3 <- view_uptake %>%
  filter(is.na(adm_hcw_q3) == FALSE,
         a_status_who == "Member State") %>%
  group_by(a_region_who) %>%
  summarize(count_rep_hcw_q3 = n()) %>%
  ungroup()

uptake_rep_count_hcw_q4 <- view_uptake %>%
  filter(is.na(adm_hcw_q4) == FALSE,
         a_status_who == "Member State") %>%
  group_by(a_region_who) %>%
  summarize(count_rep_hcw_q4 = n()) %>%
  ungroup()

uptake_region_hcw <- left_join(uptake_region_hcw, uptake_rep_count_hcw_q1, by = "a_region_who") %>%
  left_join(., uptake_rep_count_hcw_q2, by = "a_region_who") %>%
  left_join(., uptake_rep_count_hcw_q3, by = "a_region_who") %>%
  left_join(., uptake_rep_count_hcw_q4, by = "a_region_who") 

uptake_region_old <- view_uptake %>%
  filter(is.na(cumulative_old_q4) == FALSE,
         a_status_who == "Member State") %>%
  group_by(a_region_who) %>%
  summarize(adm_old_q1 = sum(adm_old_q1, na.rm = TRUE),
            adm_old_q2 = sum(adm_old_q2, na.rm = TRUE),
            adm_old_q3 = sum(adm_old_q3, na.rm = TRUE),
            adm_old_q4 = sum(adm_old_q4, na.rm = TRUE),
            cumulative_old_q4 = sum(cumulative_old_q4),
            pop_old_rep = sum(pop_older),
            count_rep_older = n()) %>%
  ungroup()

uptake_rep_count_old_q1 <- view_uptake %>%
  filter(is.na(adm_old_q1) == FALSE,
         a_status_who == "Member State") %>%
  group_by(a_region_who) %>%
  summarize(count_rep_old_q1 = n()) %>%
  ungroup()

uptake_rep_count_old_q2 <- view_uptake %>%
  filter(is.na(adm_old_q2) == FALSE,
         a_status_who == "Member State") %>%
  group_by(a_region_who) %>%
  summarize(count_rep_old_q2 = n()) %>%
  ungroup()

uptake_rep_count_old_q3 <- view_uptake %>%
  filter(is.na(adm_old_q3) == FALSE,
         a_status_who == "Member State") %>%
  group_by(a_region_who) %>%
  summarize(count_rep_old_q3 = n()) %>%
  ungroup()

uptake_rep_count_old_q4 <- view_uptake %>%
  filter(is.na(adm_old_q4) == FALSE,
         a_status_who == "Member State") %>%
  group_by(a_region_who) %>%
  summarize(count_rep_old_q4 = n()) %>%
  ungroup()

uptake_region_old <- left_join(uptake_region_old, uptake_rep_count_old_q1, by = "a_region_who") %>%
  left_join(., uptake_rep_count_old_q2, by = "a_region_who") %>%
  left_join(., uptake_rep_count_old_q3, by = "a_region_who") %>%
  left_join(., uptake_rep_count_old_q4, by = "a_region_who")

# uptake_region_male <- view_uptake %>%
#   filter(is.na(adm_male) == FALSE,
#          a_status_who == "Member State") %>%
#   group_by(a_region_who) %>%
#   summarize(adm_male = sum(adm_male),
#             count_rep_male = n()) %>%
#   ungroup()
# 
# uptake_region_female <- view_uptake %>%
#   filter(is.na(adm_female) == FALSE,
#          a_status_who == "Member State") %>%
#   group_by(a_region_who) %>%
#   summarize(adm_female = sum(adm_female),
#             count_rep_female = n()) %>%
#   ungroup()
# 
# uptake_region_pw <- view_uptake %>%
#   filter(is.na(adm_pw) == FALSE,
#          a_status_who == "Member State") %>%
#   group_by(a_region_who) %>%
#   summarize(adm_pw = sum(adm_pw),
#             count_rep_pw = n()) %>%
#   ungroup()
# 
# uptake_region_ad_chronic <- view_uptake %>%
#   filter(is.na(adm_ad_chronic) == FALSE,
#          a_status_who == "Member State") %>%
#   group_by(a_region_who) %>%
#   summarize(adm_ad_chronic = sum(adm_ad_chronic),
#             count_rep_ad_chronic = n()) %>%
#   ungroup()

uptake_region <- uptake_region_total %>%
  full_join(., uptake_region_hcw, by = "a_region_who") %>%
  full_join(., uptake_region_old, by = "a_region_who") %>%
  # full_join(., uptake_region_male, by = "a_region_who") %>%
  # full_join(., uptake_region_female, by = "a_region_who") %>%
  # full_join(., uptake_region_pw, by = "a_region_who") %>%
  # full_join(., uptake_region_ad_chronic, by = "a_region_who") %>%
  full_join(., region_pop, by = "a_region_who") %>%
  mutate(cov_total = cumulative_total_q4 / pop_total_rep,
         cov_hcw = cumulative_hcw_q4 / pop_hcw_rep,
         cov_older = cumulative_old_q4 / pop_old_rep,
         prop_pop_total = pop_total_rep / pop_total_all,
         prop_pop_hcw = pop_hcw_rep / pop_hcw_all,
         prop_pop_old = pop_old_rep / pop_older_all) %>%
  rename(grouping = a_region_who)


# Summarize - archive and current uptake ----------------------------------

uptake_now_new <-  view_uptake %>%
  select(iso,
         a_region_who,
         adm_total_q1,
         adm_total_q2,
         adm_total_q3,
         adm_total_q4) %>%
  gather(key = "date_quarter",
         value = "adm_a1d",
         -iso,
         -a_region_who) %>%
  mutate(date_quarter = case_when(
    date_quarter == "adm_total_q1" ~ "2024-1",
    date_quarter == "adm_total_q2" ~ "2024-2",
    date_quarter == "adm_total_q3" ~ "2024-3",
    date_quarter == "adm_total_q4" ~ "2024-4",
    TRUE ~ date_quarter
  )) %>%
  filter(is.na(adm_a1d) == FALSE) %>%
  group_by(a_region_who, date_quarter) %>%
  summarize(adm_a1d = sum(adm_a1d, na.rm = TRUE))

uptake_time_old_1 <- view_uptake %>%
  select(iso,
         a_region_who,
         adm_old_q1,
         cumulative_old_q2,
         cumulative_old_q3,
         cumulative_old_q4_adj) %>%
  gather(key = "date_quarter",
         value = "adm_a1d",
         -iso,
         -a_region_who) %>%
  mutate(date_quarter = case_when(
    date_quarter == "adm_old_q1" ~ "2024-1",
    date_quarter == "cumulative_old_q2" ~ "2024-2",
    date_quarter == "cumulative_old_q3" ~ "2024-3",
    date_quarter == "cumulative_old_q4_adj" ~ "2024-4",
    TRUE ~ date_quarter
  )) %>%
  filter(is.na(adm_a1d) == FALSE) %>%
  left_join(., rep_old, by = c("date_quarter" = "month_name", "iso" = "iso"))

uptake_time_old <- view_uptake %>%
  select(iso,
         adm_old_q1,
         adm_old_q2,
         adm_old_q3,
         adm_old_q4_adj) %>%
  gather(key = "date_quarter",
         value = "adm_add_a1d",
         -iso) %>%
  mutate(date_quarter = case_when(
    date_quarter == "adm_old_q1" ~ "2024-1",
    date_quarter == "adm_old_q2" ~ "2024-2",
    date_quarter == "adm_old_q3" ~ "2024-3",
    date_quarter == "adm_old_q4_adj" ~ "2024-4",
    TRUE ~ date_quarter
  )) %>%
  right_join(., uptake_time_old_1, by = c("iso" = "iso", "date_quarter" = "date_quarter"))

uptake_time_hcw_1 <- view_uptake %>%
  select(iso,
         a_region_who,
         adm_hcw_q1,
         cumulative_hcw_q2,
         cumulative_hcw_q3,
         cumulative_hcw_q4_adj) %>%
  gather(key = "date_quarter",
         value = "adm_a1d",
         -iso,
         -a_region_who) %>%
  mutate(date_quarter = case_when(
    date_quarter == "adm_hcw_q1" ~ "2024-1",
    date_quarter == "cumulative_hcw_q2" ~ "2024-2",
    date_quarter == "cumulative_hcw_q3" ~ "2024-3",
    date_quarter == "cumulative_hcw_q4_adj" ~ "2024-4",
    TRUE ~ date_quarter
  )) %>%
  filter(is.na(adm_a1d) == FALSE) %>%
  left_join(., rep_old, by = c("date_quarter" = "month_name", "iso" = "iso"))

uptake_time_hcw <- view_uptake %>%
  select(iso,
         adm_hcw_q1,
         adm_hcw_q2,
         adm_hcw_q3,
         adm_hcw_q4_adj) %>%
  gather(key = "date_quarter",
         value = "adm_add_a1d",
         -iso) %>%
  mutate(date_quarter = case_when(
    date_quarter == "adm_hcw_q1" ~ "2024-1",
    date_quarter == "adm_hcw_q2" ~ "2024-2",
    date_quarter == "adm_hcw_q3" ~ "2024-3",
    date_quarter == "adm_hcw_q4_adj" ~ "2024-4",
    TRUE ~ date_quarter
  )) %>%
  right_join(., uptake_time_hcw_1, by = c("iso" = "iso", "date_quarter" = "date_quarter"))

# uptake_now <- uptake_region_total %>%
#   select(a_region_who,
#          adm_total) %>%
#   mutate(date_quarter = "2024-1") %>%
#   rename(adm_a1d = adm_total)

uptake_quarterly <- rbind(data_archive, uptake_now_new) %>%
  filter(date_quarter != "NA-NA")


# Summarize - income group, uptake view -----------------------------------

uptake_income_total <- view_uptake %>%
  filter(is.na(cumulative_total_q4) == FALSE,
         a_status_who == "Member State",
         is.na(a_income_group) == FALSE) %>%
  group_by(a_income_group) %>%
  summarize(adm_total_q1 = sum(adm_total_q1, na.rm = TRUE),
            adm_total_q2 = sum(adm_total_q2, na.rm = TRUE),
            adm_total_q3 = sum(adm_total_q3, na.rm = TRUE),
            adm_total_q4 = sum(adm_total_q4, na.rm = TRUE),
            cumulative_total_q4 = sum(cumulative_total_q4_adj, na.rm = TRUE),
            pop_total_rep = sum(pop_total),
            count_rep_total = n()) %>%
  ungroup()

uptake_inc_rep_count_total_q1 <- view_uptake %>%
  filter(is.na(adm_total_q1) == FALSE,
         a_status_who == "Member State",
         is.na(a_income_group) == FALSE) %>%
  group_by(a_income_group) %>%
  summarize(count_rep_total_q1 = n()) %>%
  ungroup()

uptake_inc_rep_count_total_q2 <- view_uptake %>%
  filter(is.na(adm_total_q2) == FALSE,
         a_status_who == "Member State",
         is.na(a_income_group) == FALSE) %>%
  group_by(a_income_group) %>%
  summarize(count_rep_total_q2 = n()) %>%
  ungroup()

uptake_inc_rep_count_total_q3 <- view_uptake %>%
  filter(is.na(adm_total_q3) == FALSE,
         a_status_who == "Member State",
         is.na(a_income_group) == FALSE) %>%
  group_by(a_income_group) %>%
  summarize(count_rep_total_q3 = n()) %>%
  ungroup()

uptake_inc_rep_count_total_q4 <- view_uptake %>%
  filter(is.na(adm_total_q4) == FALSE,
         a_status_who == "Member State",
         is.na(a_income_group) == FALSE) %>%
  group_by(a_income_group) %>%
  summarize(count_rep_total_q4 = n()) %>%
  ungroup()

uptake_income_total <- left_join(uptake_income_total, uptake_inc_rep_count_total_q1, by = "a_income_group") %>%
  left_join(., uptake_inc_rep_count_total_q2, by = "a_income_group") %>%
  left_join(., uptake_inc_rep_count_total_q3, by = "a_income_group") %>%
  left_join(., uptake_inc_rep_count_total_q4, by = "a_income_group")



uptake_income_hcw <- view_uptake %>%
  filter(is.na(cumulative_hcw_q4) == FALSE,
         a_status_who == "Member State",
         is.na(a_income_group) == FALSE) %>%
  group_by(a_income_group) %>%
  summarize(adm_hcw_q1 = sum(adm_hcw_q1, na.rm = TRUE),
            adm_hcw_q2 = sum(adm_hcw_q2, na.rm = TRUE),
            adm_hcw_q3 = sum(adm_hcw_q3, na.rm = TRUE),
            adm_hcw_q4 = sum(adm_hcw_q4, na.rm = TRUE),
            cumulative_hcw_q4 = sum(cumulative_hcw_q4_adj, na.rm = TRUE),
            pop_hcw_rep = sum(pop_hcw),
            count_rep_hcw = n()) %>%
  ungroup()

uptake_inc_rep_count_hcw_q1 <- view_uptake %>%
  filter(is.na(adm_hcw_q1) == FALSE,
         a_status_who == "Member State",
         is.na(a_income_group) == FALSE) %>%
  group_by(a_income_group) %>%
  summarize(count_rep_hcw_q1 = n()) %>%
  ungroup()

uptake_inc_rep_count_hcw_q2 <- view_uptake %>%
  filter(is.na(adm_hcw_q2) == FALSE,
         a_status_who == "Member State",
         is.na(a_income_group) == FALSE) %>%
  group_by(a_income_group) %>%
  summarize(count_rep_hcw_q2 = n()) %>%
  ungroup()

uptake_inc_rep_count_hcw_q3 <- view_uptake %>%
  filter(is.na(adm_hcw_q3) == FALSE,
         a_status_who == "Member State",
         is.na(a_income_group) == FALSE) %>%
  group_by(a_income_group) %>%
  summarize(count_rep_hcw_q3 = n()) %>%
  ungroup()

uptake_inc_rep_count_hcw_q4 <- view_uptake %>%
  filter(is.na(adm_hcw_q4) == FALSE,
         a_status_who == "Member State",
         is.na(a_income_group) == FALSE) %>%
  group_by(a_income_group) %>%
  summarize(count_rep_hcw_q4 = n()) %>%
  ungroup()

uptake_income_hcw <- left_join(uptake_income_hcw, uptake_inc_rep_count_hcw_q1, by = "a_income_group") %>%
  left_join(., uptake_inc_rep_count_hcw_q2, by = "a_income_group") %>%
  left_join(., uptake_inc_rep_count_hcw_q3, by = "a_income_group") %>%
  left_join(., uptake_inc_rep_count_hcw_q4, by = "a_income_group")



uptake_income_old <- view_uptake %>%
  filter(is.na(cumulative_old_q4) == FALSE,
         a_status_who == "Member State",
         is.na(a_income_group) == FALSE) %>%
  group_by(a_income_group) %>%
  summarize(adm_old_q1 = sum(adm_old_q1, na.rm = TRUE),
            adm_old_q2 = sum(adm_old_q2, na.rm = TRUE),
            adm_old_q3 = sum(adm_old_q3, na.rm = TRUE),
            adm_old_q4 = sum(adm_old_q4, na.rm = TRUE),
            cumulative_old_q4 = sum(cumulative_old_q4_adj, na.rm = TRUE),
            pop_old_rep = sum(pop_older),
            count_rep_older = n()) %>%
  ungroup()

uptake_inc_rep_count_old_q1 <- view_uptake %>%
  filter(is.na(adm_old_q1) == FALSE,
         a_status_who == "Member State",
         is.na(a_income_group) == FALSE) %>%
  group_by(a_income_group) %>%
  summarize(count_rep_old_q1 = n()) %>%
  ungroup()

uptake_inc_rep_count_old_q2 <- view_uptake %>%
  filter(is.na(adm_old_q2) == FALSE,
         a_status_who == "Member State",
         is.na(a_income_group) == FALSE) %>%
  group_by(a_income_group) %>%
  summarize(count_rep_old_q2 = n()) %>%
  ungroup()

uptake_inc_rep_count_old_q3 <- view_uptake %>%
  filter(is.na(adm_old_q3) == FALSE,
         a_status_who == "Member State",
         is.na(a_income_group) == FALSE) %>%
  group_by(a_income_group) %>%
  summarize(count_rep_old_q3 = n()) %>%
  ungroup()

uptake_inc_rep_count_old_q4 <- view_uptake %>%
  filter(is.na(adm_old_q4) == FALSE,
         a_status_who == "Member State",
         is.na(a_income_group) == FALSE) %>%
  group_by(a_income_group) %>%
  summarize(count_rep_old_q4 = n()) %>%
  ungroup()

uptake_income_old <- left_join(uptake_income_old, uptake_inc_rep_count_old_q1, by = "a_income_group") %>%
  left_join(., uptake_inc_rep_count_old_q2, by = "a_income_group") %>%
  left_join(., uptake_inc_rep_count_old_q3, by = "a_income_group") %>%
  left_join(., uptake_inc_rep_count_old_q4, by = "a_income_group")

# uptake_income_male <- view_uptake %>%
#   filter(is.na(adm_male) == FALSE,
#          a_status_who == "Member State") %>%
#   group_by(a_income_group) %>%
#   summarize(adm_male = sum(adm_male),
#             count_rep_male = n()) %>%
#   ungroup()
# 
# uptake_income_female <- view_uptake %>%
#   filter(is.na(adm_female) == FALSE,
#          a_status_who == "Member State") %>%
#   group_by(a_income_group) %>%
#   summarize(adm_female = sum(adm_female),
#             count_rep_female = n()) %>%
#   ungroup()
# 
# uptake_income_pw <- view_uptake %>%
#   filter(is.na(adm_pw) == FALSE,
#          a_status_who == "Member State") %>%
#   group_by(a_income_group) %>%
#   summarize(adm_pw = sum(adm_pw),
#             count_rep_pw = n()) %>%
#   ungroup()
# 
# uptake_income_ad_chronic <- view_uptake %>%
#   filter(is.na(adm_ad_chronic) == FALSE,
#          a_status_who == "Member State") %>%
#   group_by(a_income_group) %>%
#   summarize(adm_ad_chronic = sum(adm_ad_chronic),
#             count_rep_ad_chronic = n()) %>%
#   ungroup()

uptake_income <- uptake_income_total %>%
  full_join(., uptake_income_hcw, by = "a_income_group") %>%
  full_join(., uptake_income_old, by = "a_income_group") %>%
  # full_join(., uptake_income_male, by = "a_income_group") %>%
  # full_join(., uptake_income_female, by = "a_income_group") %>%
  # full_join(., uptake_income_pw, by = "a_income_group") %>%
  # full_join(., uptake_income_ad_chronic, by = "a_income_group") %>%
  full_join(., income_pop, by = "a_income_group") %>%
  mutate(cov_total = cumulative_total_q4 / pop_total_rep,
         cov_hcw = cumulative_hcw_q4 / pop_hcw_rep,
         cov_older = cumulative_old_q4 / pop_old_rep,
         prop_pop_total = pop_total_rep / pop_total_all,
         prop_pop_hcw = pop_hcw_rep / pop_hcw_all,
         prop_pop_old = pop_old_rep / pop_older_all) %>%
  rename(grouping = a_income_group)


# WIISE preparation -------------------------------------------------------


uptake_income_wiise <- uptake_income %>%
  gather(key = "indicator", value = "value", -grouping)

uptake_income_wiise_adm <- uptake_income_wiise %>%
  filter(indicator == "adm_hcw_q1" |
           indicator == "adm_hcw_q2" |
           indicator == "adm_hcw_q3" |
           indicator == "adm_hcw_q4" |           
           indicator == "adm_old_q1" |
           indicator == "adm_old_q2" |
           indicator == "adm_old_q3" |
           indicator == "adm_old_q4" |
           indicator == "adm_total_q1" |
           indicator == "adm_total_q2" |
           indicator == "adm_total_q3" |
           indicator == "adm_total_q4") %>%
  mutate(DATE = case_when(
    indicator == "adm_hcw_q1" ~ as.Date("2024-04-01"),
    indicator == "adm_hcw_q2" ~ as.Date("2024-07-01"),
    indicator == "adm_hcw_q3" ~ as.Date("2024-10-01"),
    indicator == "adm_hcw_q4" ~ as.Date("2025-01-01"),    
    indicator == "adm_old_q1" ~ as.Date("2024-04-01"),
    indicator == "adm_old_q2" ~ as.Date("2024-07-01"),
    indicator == "adm_old_q3" ~ as.Date("2024-10-01"),
    indicator == "adm_old_q4" ~ as.Date("2025-01-01"),
    indicator == "adm_total_q1" ~ as.Date("2024-04-01"),
    indicator == "adm_total_q2" ~ as.Date("2024-07-01"),
    indicator == "adm_total_q3" ~ as.Date("2024-10-01"),
    indicator == "adm_total_q4" ~ as.Date("2024-01-01")
  ),
         GROUP = case_when(
    indicator == "adm_hcw_q1" ~ "hcw",
    indicator == "adm_hcw_q2" ~ "hcw",
    indicator == "adm_hcw_q3" ~ "hcw",
    indicator == "adm_hcw_q4" ~ "hcw",
    indicator == "adm_old_q1" ~ "old",
    indicator == "adm_old_q2" ~ "old",
    indicator == "adm_old_q3" ~ "old",
    indicator == "adm_old_q4" ~ "old",
    indicator == "adm_total_q1" ~ "total",
    indicator == "adm_total_q2" ~ "total",
    indicator == "adm_total_q3" ~ "total",
    indicator == "adm_total_q4" ~ "total"
  ),
  TYPE = "additive") %>%
  rename(COVID_VACCINE_ADM_1D = value,
         INCOME_GROUP = grouping) %>%
  select(-indicator)

uptake_income_wiise_cum <- uptake_income_wiise %>%
  filter(indicator == "cumulative_hcw_q4" |
           indicator == "cumulative_old_q4" |
           indicator == "cumulative_total_q4") %>%
  mutate(DATE = case_when(
    indicator == "cumulative_hcw_q4" ~ as.Date("2025-01-01"),
    indicator == "cumulative_old_q4" ~ as.Date("2025-01-01"),
    indicator == "cumulative_total_q4" ~ as.Date("2025-01-01")
  ),
  GROUP = case_when(
    indicator == "cumulative_hcw_q4" ~ "hcw",
    indicator == "cumulative_old_q4" ~ "old",
    indicator == "cumulative_total_q4" ~ "total"
  ),
  TYPE = "cumulative") %>%
  rename(COVID_VACCINE_ADM_1D = value,
         INCOME_GROUP = grouping) %>%
  select(-indicator)

uptake_income_wiise_all <- rbind(uptake_income_wiise_adm, uptake_income_wiise_cum)
  

uptake_income_wiise_pop <- uptake_income_wiise %>%
  filter(indicator == "pop_old_rep" |
           indicator == "pop_hcw_rep" |
           indicator == "pop_total_rep") %>%
  mutate(GROUP = case_when(
    indicator == "pop_old_rep" ~ "old",
    indicator == "pop_hcw_rep" ~ "hcw",
    indicator == "pop_total_rep" ~ "total"
  )) %>%
  rename(POPULATION = value,
         INCOME_GROUP = grouping) %>%
  select(-indicator)

uptake_income_wiise_all <- left_join(uptake_income_wiise_all, uptake_income_wiise_pop,
                                     by = c("INCOME_GROUP", "GROUP"))

uptake_income_wiise_all <- uptake_income_wiise_all %>%
  mutate(COVID_VACCINE_COV_1D = COVID_VACCINE_ADM_1D / POPULATION)


uptake_region_wiise <- uptake_region %>%
  gather(key = "indicator", value = "value", -grouping)

uptake_region_wiise_adm <- uptake_region_wiise %>%
  filter(indicator == "adm_hcw_q1" |
           indicator == "adm_hcw_q2" |
           indicator == "adm_hcw_q3" |
           indicator == "adm_hcw_q4" |
           indicator == "adm_old_q1" |
           indicator == "adm_old_q2" |
           indicator == "adm_old_q3" |
           indicator == "adm_old_q4" |           
           indicator == "adm_total_q1" |
           indicator == "adm_total_q2" | 
           indicator == "adm_total_q3" |
           indicator == "adm_total_q4") %>%
  mutate(DATE = case_when(
    indicator == "adm_hcw_q1" ~ as.Date("2024-04-01"),
    indicator == "adm_hcw_q2" ~ as.Date("2024-07-01"),
    indicator == "adm_hcw_q3" ~ as.Date("2024-10-01"),   
    indicator == "adm_hcw_q4" ~ as.Date("2025-01-01"),   
    indicator == "adm_old_q1" ~ as.Date("2024-04-01"),
    indicator == "adm_old_q2" ~ as.Date("2024-07-01"),
    indicator == "adm_old_q3" ~ as.Date("2024-10-01"),
    indicator == "adm_old_q4" ~ as.Date("2025-01-01"),
    indicator == "adm_total_q1" ~ as.Date("2024-04-01"),
    indicator == "adm_total_q2" ~ as.Date("2024-07-01"),
    indicator == "adm_total_q3" ~ as.Date("2024-10-01"),
    indicator == "adm_total_q4" ~ as.Date("2025-01-01")
  ),
  GROUP = case_when(
    indicator == "adm_hcw_q1" ~ "hcw",
    indicator == "adm_hcw_q2" ~ "hcw",
    indicator == "adm_hcw_q3" ~ "hcw",
    indicator == "adm_hcw_q4" ~ "hcw",    
    indicator == "adm_old_q1" ~ "old",
    indicator == "adm_old_q2" ~ "old",
    indicator == "adm_old_q3" ~ "old",
    indicator == "adm_old_q4" ~ "old",
    indicator == "adm_total_q1" ~ "total",
    indicator == "adm_total_q2" ~ "total",
    indicator == "adm_total_q3" ~ "total",
    indicator == "adm_total_q4" ~ "total"
  ),
  TYPE = "additive") %>%
  rename(COVID_VACCINE_ADM_1D = value,
         REGION = grouping) %>%
  select(-indicator)

uptake_region_wiise_cum <- uptake_region_wiise %>%
  filter(indicator == "cumulative_hcw_q4" |
           indicator == "cumulative_old_q4" |
           indicator == "cumulative_total_q4") %>%
  mutate(DATE = case_when(
    indicator == "cumulative_hcw_q4" ~ as.Date("2025-01-01"),
    indicator == "cumulative_old_q4" ~ as.Date("2025-01-01"),
    indicator == "cumulative_total_q4" ~ as.Date("2025-01-01")
  ),
  GROUP = case_when(
    indicator == "cumulative_hcw_q4" ~ "hcw",
    indicator == "cumulative_old_q4" ~ "old",
    indicator == "cumulative_total_q4" ~ "total"
  ),
  TYPE = "cumulative") %>%
  rename(COVID_VACCINE_ADM_1D = value,
         REGION = grouping) %>%
  select(-indicator)

uptake_region_wiise_all <- rbind(uptake_region_wiise_adm, uptake_region_wiise_cum)


uptake_region_wiise_pop <- uptake_region_wiise %>%
  filter(indicator == "pop_old_rep" |
           indicator == "pop_hcw_rep" |
           indicator == "pop_total_rep") %>%
  mutate(GROUP = case_when(
    indicator == "pop_old_rep" ~ "old",
    indicator == "pop_hcw_rep" ~ "hcw",
    indicator == "pop_total_rep" ~ "total"
  )) %>%
  rename(POPULATION = value,
         REGION = grouping) %>%
  select(-indicator)

uptake_region_wiise_all <- left_join(uptake_region_wiise_all, uptake_region_wiise_pop,
                                     by = c("REGION", "GROUP"))

uptake_region_wiise_all <- uptake_region_wiise_all %>%
  mutate(COVID_VACCINE_COV_1D = COVID_VACCINE_ADM_1D / POPULATION)


# Combine - country groupings, uptake ---------------------------------------------

uptake_groupings <- rbind(uptake_region, uptake_income)

values_static <- data.frame(as.Date("2024-12-31")) %>%
  rename(date_period = `as.Date..2024.12.31..`) %>%
  mutate(date_asof = as.Date("2025-07-22"))

test <- uptake_region %>%
  summarize(pop_total = sum(pop_total_rep),
            pop_all = sum(pop_total_all)) %>%
  mutate(per = pop_total / pop_all)

# Export ------------------------------------------------------------------

export <- list(
  "uptake_country_wide" = view_uptake,
  "uptake_grouping_wide" = uptake_groupings,
  "uptake_region_time" = uptake_quarterly,
  "uptake_region_cum_time" = uptake_time_old,
  "uptake_region_cum_time_hcw" = uptake_time_hcw,
  "policy_country_long" = data_policy_long,
  "policy_country_wide" = data_policy_wide,
  "product_country_long" = data_product_long,
  "product_country_wide" = data_product_wide,
  "integration_country_long" = data_integration_long,
  "integration_country_wide" = data_integration_wide,
  "values_static" = values_static
)

export_wiise <- list(
  "COV_VAC_DASH_UPTAKE" = view_uptake_wiise_dash,
  "COV_VAC_DASH_IND" = view_ind_wiise_dash,
  "COV_VAC_DASH_UPTAKE_INCOME" = uptake_income_wiise_all,
  "COV_VAC_DASH_UPTAKE_REGION" = uptake_region_wiise_all
)

write_xlsx(export, "data/output/output_new.xlsx")
write_xlsx(data_uptake_wide_cum_total_pre_mod, "data/output/uptake.xlsx")
write_xlsx(data_uptake_wide_cum_old_premod, "data/output/uptake_old.xlsx")
write_xlsx(data_uptake_wide_cum_hcw_premod, "data/output/uptake_hcw.xlsx")
write_xlsx(export_wiise, "data/output/250718_output_wiise.xlsx")



