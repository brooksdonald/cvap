# COVID-19 Vaccination Analysis
# Author: BROOKS, Donald J.

# Setup -------------------------------------------------------------------

# Clear environment
rm(list = ls())
gc()

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

raw_data <- data.frame(
  read_excel("data/test.xlsx"))

raw_data_entity <- data.frame(
  read_excel("data/input/static/base_entitydetails.xlsx",
             sheet = "data"))

raw_data_uptake <- data.frame(
  read.csv("data/data_export_WIISE_COV_UPTAKE.csv"))

data_entity <- raw_data_entity %>%
  select(CODE,
         NAMEWORKEN,
         WHOREGIONC,
         WHO_LEGAL_STATUS_TITLE,
         WBINCOMESTATUS)

data <- raw_data %>%
  group_by(COUNTRY, YEAR) %>%
  filter(REPORTING_PERIOD == max(REPORTING_PERIOD)) %>%
  ungroup() %>%
  group_by(COUNTRY) %>%
  filter(YEAR == max(YEAR)) %>% 
  select(-SOURCE,
         -DIMENSION2) %>%
  filter(is.na(VALUE) == FALSE,
         VALUE != -2222) %>%
  left_join(., data_entity, by = c("COUNTRY" = "CODE"))

data_uptake <- raw_data_uptake %>%
  filter(TARGET_GROUP == "CO_MORBIDITY" |
           TARGET_GROUP == "RES_LONG_TERM" |
           TARGET_GROUP == "PW" |
           TARGET_GROUP == "CO_MORBIDITY_ADULTS" |
           TARGET_GROUP == "IMMUNOCOMPROMISED" |
           TARGET_GROUP == "POLICY_RECOMMENDATION_ADULTS_CHRONIC" | 
           TARGET_GROUP == "PEOPLE_W_COMORB"
  ) %>%
  mutate(TARGET_GROUP = case_when(
    TARGET_GROUP == "POLICY_RECOMMENDATION_ADULTS_CHRONIC" ~ "CO_MORBIDITY_ADULTS",
    TARGET_GROUP == "PEOPLE_W_COMORB" ~ "CO_MORBIDITY",
    TRUE ~ TARGET_GROUP
  )) %>%
  filter(is.na(N_VACC_DOSE1) == FALSE,
         N_VACC_DOSE1 != -2222,
         N_VACC_DOSE1 != -4444,
         N_VACC_DOSE1 != 0) %>%
  group_by(COUNTRY, YEAR, REPORTING_PERIOD, TARGET_GROUP) %>%
  summarise(N_VACC_DOSE1 = sum(N_VACC_DOSE1)) %>%
  ungroup() %>%
  group_by(COUNTRY, YEAR) %>%
  filter(REPORTING_PERIOD == max(REPORTING_PERIOD)) %>%
  ungroup() %>%
  group_by(COUNTRY) %>%
  filter(YEAR == max(YEAR)) %>%
  spread(TARGET_GROUP, N_VACC_DOSE1) %>%
  left_join(., data_entity, by = c("COUNTRY" = "CODE"))

export <- list(
  "data_pol_pw" = data,
  "data_uptake" = data_uptake
)

write_xlsx(export, "data/241217_COVID vx_additional data.xlsx")

