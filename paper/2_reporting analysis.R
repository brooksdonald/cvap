# Load datasets -----------------------------------------------------------

# Entity data
base_details <-
  data.frame(read_excel("data/input/static/base_entitydetails.xlsx",
                      sheet = "data"))

# Disaggregated data
raw_target <-
  GET(
    'https://xmart-api-public.who.int/WIISE/MT_COV_UPTAKE_TARGETGROUP_LONG'
  )

raw_target_text <- content(raw_target, "text")
raw_target_json <- fromJSON(raw_target_text, flatten = TRUE)
base_target <- as.data.frame(raw_target_json)
names(base_target) <- substring(names(base_target), 7)

# Aggregated data
raw_weekly_all <- 
  GET(
    'https://xmart-api-public.who.int/NCOV/VAC_REP_COUNTS'
  )

raw_weekly_all_text <- content(raw_weekly_all, "text")
raw_weekly_all_json <- fromJSON(raw_weekly_all_text, flatten = TRUE)
base_weekly_all <- as.data.frame(raw_weekly_all_json)
names(base_weekly_all) <- substring(names(base_weekly_all), 7)

raw_weekly_eur <- 
  GET(
    'https://xmart-api-public.who.int/NCOV/VAC_REP_COUNTS_EUR'
  )

raw_weekly_eur_text <- content(raw_weekly_eur, "text")
raw_weekly_eur_json <- fromJSON(raw_weekly_eur_text, flatten = TRUE)
base_weekly_eur <- as.data.frame(raw_weekly_eur_json)
names(base_weekly_eur) <- substring(names(base_weekly_eur), 7)


# Staging -----------------------------------------------------------------

# Merge aggregated count tables
red_weekly_all <- base_weekly_all %>%
  select(COUNTRY_FK,
         AS_OF_DATE,
         DOSES_ADMINISTERED,
         PERSONS_VACCINATED_ONE_PLUS_DOSE,
         PERSONS_VACCINATED_FULL,
         PERSONS_BOOSTER_ADD_DOSE)

red_weekly_eur <- base_weekly_eur %>%
  select(COUNTRY_FK,
         AS_OF_DATE,
         DOSES_ADMINISTERED,
         PERSONS_VACCINATED_ONE_PLUS_DOSE,
         PERSONS_VACCINATED_FULL,
         PERSONS_BOOSTER_ADD_DOSE)

base_total <- rbind(red_weekly_all, red_weekly_eur) %>%
  mutate(iso = countrycode(COUNTRY_FK,
                           origin = "country.name",
                           destination = "iso3c",
                           warn = FALSE)) %>%
  left_join(., base_details, by = c("iso"= "CODE"))


# HCW - monthly count -----------------------------------------------------

# Filter for healthcare workers & legal status
target_hcw <- base_target %>%
  filter(TARGET_GROUP == "HW" & WHO_LEGAL_STATUS_TITLE == "Member State") %>%
  mutate(hw_cps_repstat = if_else(
           is.na(N_VACC_LAST_DOSE) == FALSE & N_VACC_LAST_DOSE != 0,
           "reporting", NA_character_),
         hw_boost_repstat = if_else(
           is.na(N_VACC_BOOSTER_DOSE) == FALSE & N_VACC_BOOSTER_DOSE != 0,
           "reporting", NA_character_))

# Filter for cps reporting countries
target_hcw_cps <- target_hcw %>%
  filter(hw_cps_repstat == "reporting") %>%
  group_by(ISO_3_CODE, DATE) %>%
  top_n(1, N_VACC_LAST_DOSE) %>%
  ungroup()

## Summarize by month
count_time_hcw_cps <- target_hcw_cps %>%
  group_by(DATE) %>%
  summarize(count = n()) %>%
  mutate(type = "Complete primary series") %>%
  ungroup()

## Summarize by country
count_country_hcw_cps <- target_hcw_cps %>%
  group_by(ISO_3_CODE) %>%
  summarise(count = n()) %>%
  mutate(type = "Complete primary series") %>%
  rename(code = ISO_3_CODE) %>%
  ungroup()

# Filter for booster reporting countries
target_hcw_boost <- target_hcw %>%
  filter(hw_boost_repstat == "reporting") %>%
  group_by(ISO_3_CODE, DATE) %>%
  top_n(1, N_VACC_LAST_DOSE) %>%
  ungroup()

## Summarize by month
count_time_hcw_boost <- target_hcw_boost %>%
  group_by(DATE) %>%
  summarize(count = n()) %>%
  mutate(type = "Booster") %>%
  ungroup()

## Summarize by country
count_country_hcw_boost <- target_hcw_boost %>%
  group_by(ISO_3_CODE) %>%
  summarise(count = n()) %>%
  mutate(type = "Booster") %>%
  rename(code = ISO_3_CODE) %>%
  ungroup()

# Merge month count tables
count_time_hcw_month <- rbind(count_time_hcw_cps, count_time_hcw_boost) %>%
  spread(type, count) %>%
  rename(cps = `Complete primary series`) %>%
  mutate(`Complete primary series` = cps - `Booster`) %>%
  gather(key = "type", value = "count", -DATE) %>%
  rename(count_month = count)


# HCW - cumulative count --------------------------------------------------

# Create and merge blank receiving name and date tables
base_names <- base_details %>%
  select(CODE) %>%
  rename(iso = CODE)

base_dates <- data.frame(seq(from = ymd("2021-01-01"), 
                             to = ymd("2023-12-31"), 
                             by = "month"))
colnames(base_dates) <- "date"

base_blank <- merge(base_names, base_dates, all = TRUE)

# Select relevant variables, join data frames, fill forward, and remove zeros
mod_target_hcw <- target_hcw %>%
  select(ISO_3_CODE,
         DATE,
         WHO_LEGAL_STATUS_TITLE,
         WHO_REGION,
         WBINCOME_LONG_STATUS,
         N_VACC_LAST_DOSE,
         N_VACC_BOOSTER_DOSE) %>%
  mutate(DATE = as.Date(DATE)) %>%
  filter(is.na(N_VACC_LAST_DOSE) == FALSE | is.na(N_VACC_BOOSTER_DOSE) == FALSE)%>%
  group_by(ISO_3_CODE, DATE) %>%
  slice(1) %>%
  ungroup() %>%
  right_join(base_blank, by = c("DATE" = "date",  "ISO_3_CODE" = "iso")) %>%
  arrange(ISO_3_CODE, DATE) %>%
  group_by(ISO_3_CODE) %>%
  fill(N_VACC_LAST_DOSE,
       N_VACC_BOOSTER_DOSE,
       WHO_LEGAL_STATUS_TITLE,
       WHO_REGION,
       WBINCOME_LONG_STATUS) %>%
  mutate(N_VACC_LAST_DOSE = ifelse(N_VACC_LAST_DOSE == 0, NA, N_VACC_LAST_DOSE),
         N_VACC_BOOSTER_DOSE = ifelse(N_VACC_BOOSTER_DOSE == 0, NA, N_VACC_BOOSTER_DOSE)) %>%
  filter(WHO_LEGAL_STATUS_TITLE == "Member State")

# Filter for complete primary reports
sum_target_hcw_cps <- mod_target_hcw %>%
  filter(is.na(N_VACC_LAST_DOSE) == FALSE) %>%
  group_by(DATE) %>%
  summarize(count = n()) %>%
  mutate(type = "Complete primary series")

# Filter and summarize for booster reports
sum_target_hcw_boost <- mod_target_hcw %>%
  filter(is.na(N_VACC_BOOSTER_DOSE) == FALSE) %>%
  group_by(DATE) %>%
  summarize(count = n()) %>%
  mutate(type = "Booster")

# Combine count tables
count_time_hcw_cumul <- rbind(sum_target_hcw_cps, 
                    sum_target_hcw_boost) %>%
  rename(date = DATE,
         count_cumul = count)


# HCW - combine monthly and cumulative ------------------------------------

# Isolate monthly cps count
count_time_hcw <- count_time_hcw_month %>%
  filter(type !=  "cps")%>%
  mutate(DATE = as.Date(DATE)) %>%
  left_join(., count_time_hcw_cumul, by = c("DATE" = "date", "type" = "type")) %>%
  mutate(group = "hcw")


# HCW - cumulative, disaggregated -----------------------------------------

# Filter for complete primary reports
sum_target_hcw_cps_inc <- mod_target_hcw %>%
  filter(is.na(N_VACC_LAST_DOSE) == FALSE,
         WBINCOME_LONG_STATUS != "NA",
         is.na(WBINCOME_LONG_STATUS) == FALSE) %>%
  group_by(DATE, WBINCOME_LONG_STATUS) %>%
  summarize(count = n()) %>%
  mutate(type = "Complete primary series",
         count_total = case_when(
           WBINCOME_LONG_STATUS == "High income" ~ count_hic,
           WBINCOME_LONG_STATUS == "Upper middle income" ~ count_umic,
           WBINCOME_LONG_STATUS == "Lower middle income" ~ count_lmic,
           WBINCOME_LONG_STATUS == "Low income" ~ count_lic,
           NA ~ TRUE
         ),
         count_per = count / count_total) %>%
  ungroup()

# Filter and summarize for booster reports
sum_target_hcw_boost_inc <- mod_target_hcw %>%
  filter(is.na(N_VACC_BOOSTER_DOSE) == FALSE,
         WBINCOME_LONG_STATUS != "NA",
         is.na(WBINCOME_LONG_STATUS) == FALSE) %>%
  group_by(DATE, WBINCOME_LONG_STATUS) %>%
  summarize(count = n()) %>%
  mutate(type = "Booster",
         count_total = case_when(
           WBINCOME_LONG_STATUS == "High income" ~ count_hic,
           WBINCOME_LONG_STATUS == "Upper middle income" ~ count_umic,
           WBINCOME_LONG_STATUS == "Lower middle income" ~ count_lmic,
           WBINCOME_LONG_STATUS == "Low income" ~ count_lic,
           NA ~ TRUE
         ),
         count_per = count / count_total) %>%
  ungroup()

# Combine count tables
count_time_hcw_inc_cumul <- rbind(sum_target_hcw_cps_inc, 
                                  sum_target_hcw_boost_inc) %>%
  mutate(population = "Health and care workers")


# Older adults - monthly count -------------------------------------------

# Filter target for older adults & legal status
target_old <- base_target %>%
  filter(TARGET_GROUP == "OLDER_60" & WHO_LEGAL_STATUS_TITLE == "Member State") %>%
  mutate(old_cps_repstat = if_else(
           is.na(N_VACC_LAST_DOSE) == FALSE & N_VACC_LAST_DOSE != 0,
           "reporting", NA_character_),
         old_boost_repstat = if_else(
           is.na(N_VACC_BOOSTER_DOSE) == FALSE & N_VACC_BOOSTER_DOSE != 0,
           "reporting", NA_character_))

# Filter for cps reporting countries
target_old_cps <- target_old %>%
  filter(old_cps_repstat == "reporting") %>%
  group_by(ISO_3_CODE, DATE) %>%
  top_n(1, N_VACC_LAST_DOSE) %>%
  ungroup()

## Summarize by month
count_time_old_cps <- target_old_cps %>%
  group_by(DATE) %>%
  summarize(count = n()) %>%
  mutate(type = "Complete primary series") %>%
  ungroup()

## Summarize by country
count_country_old_cps <- target_old_cps %>%
  group_by(ISO_3_CODE) %>%
  summarise(count = n()) %>%
  mutate(type = "Complete primary series") %>%
  rename(code = ISO_3_CODE) %>%
  ungroup()

# Filter for booster reporting countries
target_old_boost <- target_old %>%
  filter(old_boost_repstat == "reporting") %>%
  group_by(ISO_3_CODE, DATE) %>%
  top_n(1, N_VACC_LAST_DOSE) %>%
  ungroup()

## Summarize by month
count_time_old_boost <- target_old_boost %>%
  group_by(DATE) %>%
  summarize(count = n()) %>%
  mutate(type = "Booster") %>%
  ungroup()

## Summarize by country
count_country_old_boost <- target_old_boost %>%
  group_by(ISO_3_CODE) %>%
  summarise(count = n()) %>%
  mutate(type = "Booster") %>%
  rename(code = ISO_3_CODE) %>%
  ungroup()

# Merge month count tables
count_time_old_month <- rbind(count_time_old_cps, count_time_old_boost) %>%
  spread(type, count) %>%
  rename(cps = `Complete primary series`) %>%
  mutate(`Complete primary series` = cps - `Booster`) %>%
  gather(key = "type", value = "count", -DATE) %>%
  rename(count_month = count)


# Older adults - cumulative count -----------------------------------------

# Select relevant variables, join data frames, fill forward, and remove zeros
mod_target_old <- target_old %>%
  select(ISO_3_CODE,
         DATE,
         WHO_LEGAL_STATUS_TITLE,
         WHO_REGION,
         WBINCOME_LONG_STATUS,
         N_VACC_LAST_DOSE,
         N_VACC_BOOSTER_DOSE) %>%
  mutate(DATE = as.Date(DATE)) %>%
  filter(is.na(N_VACC_LAST_DOSE) == FALSE | is.na(N_VACC_BOOSTER_DOSE) == FALSE)%>%
  group_by(ISO_3_CODE, DATE) %>%
  slice(1) %>%
  ungroup() %>%
  right_join(base_blank, by = c("DATE" = "date",  "ISO_3_CODE" = "iso")) %>%
  arrange(ISO_3_CODE, DATE) %>%
  group_by(ISO_3_CODE) %>%
  fill(N_VACC_LAST_DOSE,
       N_VACC_BOOSTER_DOSE,
       WHO_LEGAL_STATUS_TITLE,
       WHO_REGION,
       WBINCOME_LONG_STATUS) %>%
  mutate(N_VACC_LAST_DOSE = ifelse(N_VACC_LAST_DOSE == 0, NA, N_VACC_LAST_DOSE),
         N_VACC_BOOSTER_DOSE = ifelse(N_VACC_BOOSTER_DOSE == 0, NA, N_VACC_BOOSTER_DOSE)) %>%
  filter(WHO_LEGAL_STATUS_TITLE == "Member State")

# Filter for complete primary reports
sum_target_old_cps <- mod_target_old %>%
  filter(is.na(N_VACC_LAST_DOSE) == FALSE) %>%
  group_by(DATE) %>%
  summarize(count = n()) %>%
  mutate(type = "Complete primary series")

# Filter and summarize for booster reports
sum_target_old_boost <- mod_target_old %>%
  filter(is.na(N_VACC_BOOSTER_DOSE) == FALSE) %>%
  group_by(DATE) %>%
  summarize(count = n()) %>%
  mutate(type = "Booster")

# Combine count tables
count_time_old_cumul <- rbind(sum_target_old_cps, 
                              sum_target_old_boost) %>%
  rename(date = DATE,
         count_cumul = count)


# Older adults - combine monthly and cumulative ---------------------------

# Isolate monthly cps count
count_time_old <- count_time_old_month %>%
  filter(type !=  "cps")%>%
  mutate(DATE = as.Date(DATE)) %>%
  left_join(., count_time_old_cumul, by = c("DATE" = "date", "type" = "type")) %>%
  mutate(group = "old")


# Older adults - cumulative, disaggregated --------------------------------

# Filter for complete primary reports
sum_target_old_cps_inc <- mod_target_old %>%
  filter(is.na(N_VACC_LAST_DOSE) == FALSE,
         WBINCOME_LONG_STATUS != "NA",
         is.na(WBINCOME_LONG_STATUS) == FALSE) %>%
  group_by(DATE, WBINCOME_LONG_STATUS) %>%
  summarize(count = n()) %>%
  mutate(type = "Complete primary series",
         count_total = case_when(
           WBINCOME_LONG_STATUS == "High income" ~ count_hic,
           WBINCOME_LONG_STATUS == "Upper middle income" ~ count_umic,
           WBINCOME_LONG_STATUS == "Lower middle income" ~ count_lmic,
           WBINCOME_LONG_STATUS == "Low income" ~ count_lic,
           NA ~ TRUE
         ),
         count_per = count / count_total) %>%
  ungroup()

# Filter and summarize for booster reports
sum_target_old_boost_inc <- mod_target_old %>%
  filter(is.na(N_VACC_BOOSTER_DOSE) == FALSE,
         WBINCOME_LONG_STATUS != "NA",
         is.na(WBINCOME_LONG_STATUS) == FALSE) %>%
  group_by(DATE, WBINCOME_LONG_STATUS) %>%
  summarize(count = n()) %>%
  mutate(type = "Booster",
         count_total = case_when(
           WBINCOME_LONG_STATUS == "High income" ~ count_hic,
           WBINCOME_LONG_STATUS == "Upper middle income" ~ count_umic,
           WBINCOME_LONG_STATUS == "Lower middle income" ~ count_lmic,
           WBINCOME_LONG_STATUS == "Low income" ~ count_lic,
           NA ~ TRUE
         ),
         count_per = count / count_total) %>%
  ungroup()

# Combine count tables
count_time_old_inc_cumul <- rbind(sum_target_old_cps_inc, 
                                  sum_target_old_boost_inc) %>%
  mutate(population = "Older adults")


# Total - monthly count ----------------------------------------------------

total <- base_total %>%
  filter(WHO_LEGAL_STATUS_TITLE == "Member State") %>%
  mutate(year = year(AS_OF_DATE),
         month = month(AS_OF_DATE),
         date = as.Date(paste0(year, '-', month, '-01'), 
                        format = '%Y-%m-%d')) %>%
  group_by(iso, date) %>%
  top_n(1, AS_OF_DATE) %>%
  ungroup()

# Filter for cps reporting countries
total_cps <- total %>%
  filter(is.na(PERSONS_VACCINATED_FULL) == FALSE & PERSONS_VACCINATED_FULL != 0)

## Summarize by month
count_time_total_cps <- total_cps %>%
  group_by(date) %>%
  summarise(count = n()) %>%
  mutate(type = "Complete primary series") %>%
  ungroup()

## Summarize by country
count_country_total_cps <- total_cps %>%
  group_by(iso) %>%
  summarise(count = n()) %>%
  mutate(type = "Complete primary series") %>%
  rename(code = iso) %>%
  ungroup()

# Filter for booster reporting countries
total_boost <- total %>%
  filter(is.na(PERSONS_BOOSTER_ADD_DOSE) == FALSE & PERSONS_BOOSTER_ADD_DOSE != 0)

## Summarize by week
count_time_total_boost <- total_boost %>%
  group_by(date) %>%
  summarise(count = n()) %>%
  mutate(type = "Booster") %>%
  ungroup()

## Summarize by country
count_country_total_boost <- total_boost %>%
  group_by(iso) %>%
  summarise(count = n()) %>%
  mutate(type = "Booster") %>%
  rename(code = iso) %>%
  ungroup()

# Merge month count tables
count_time_total_month <- rbind(count_time_total_cps, count_time_total_boost) %>%
  spread(type, count) %>%
  rename(cps = `Complete primary series`) %>%
  mutate(Booster = if_else(is.na(Booster), 0, Booster)) %>%
  mutate(`Complete primary series` = cps - `Booster`) %>%
  gather(key = "type", value = "count", -date) %>%
  rename(count_month = count)

# Total - cumulative count ------------------------------------------------

mod_total <- total %>%
  select(iso,
         date,
         WHO_LEGAL_STATUS_TITLE,
         WHOREGIONC,
         WBINCOMESTATUS,
         PERSONS_VACCINATED_FULL,
         PERSONS_BOOSTER_ADD_DOSE) %>%
  mutate(WBINCOME_LONG_STATUS = if_else(WBINCOMESTATUS == "High income: nonOECD","High income",
                                        if_else(WBINCOMESTATUS == "High income: OECD", "High income",
                                                WBINCOMESTATUS))) %>%
  select(-WBINCOMESTATUS) %>%
  right_join(base_blank, by = c("date" = "date",  "iso" = "iso")) %>%
  arrange(iso, date) %>%
  group_by(iso) %>%
  fill(PERSONS_VACCINATED_FULL,
       PERSONS_BOOSTER_ADD_DOSE,
       WHO_LEGAL_STATUS_TITLE,
       WHOREGIONC,
       WBINCOME_LONG_STATUS) %>%
  mutate(PERSONS_VACCINATED_FULL = ifelse(PERSONS_VACCINATED_FULL == 0, NA, PERSONS_VACCINATED_FULL),
         PERSONS_BOOSTER_ADD_DOSE = ifelse(PERSONS_BOOSTER_ADD_DOSE == 0, NA, PERSONS_BOOSTER_ADD_DOSE)) %>%
  filter(WHO_LEGAL_STATUS_TITLE == "Member State")

# Filter for complete primary reports
sum_weekly_cps <- mod_total %>%
  filter(is.na(PERSONS_VACCINATED_FULL) == FALSE) %>%
  group_by(date) %>%
  summarize(count = n()) %>%
  mutate(type = "Complete primary series")

# Filter and summarize for booster reports
sum_weekly_boost <- mod_total %>%
  filter(is.na(PERSONS_BOOSTER_ADD_DOSE) == FALSE) %>%
  group_by(date) %>%
  summarize(count = n()) %>%
  mutate(type = "Booster")

# Combine count tables
count_time_total_cumul <- rbind(sum_weekly_cps, 
                                sum_weekly_boost) %>%
  rename(count_cumul = count)


# Total - cumulative, disaggregated ---------------------------------------

# Filter for complete primary reports
sum_total_cps_inc <- mod_total %>%
  filter(is.na(PERSONS_VACCINATED_FULL) == FALSE,
         is.na(WBINCOME_LONG_STATUS) == FALSE) %>%
  group_by(date, WBINCOME_LONG_STATUS) %>%
  summarize(count = n()) %>%
  mutate(type = "Complete primary series",
         count_total = case_when(
           WBINCOME_LONG_STATUS == "High income" ~ count_hic,
           WBINCOME_LONG_STATUS == "Upper middle income" ~ count_umic,
           WBINCOME_LONG_STATUS == "Lower middle income" ~ count_lmic,
           WBINCOME_LONG_STATUS == "Low income" ~ count_lic,
           NA ~ TRUE
         ),
         count_per = count / count_total) %>%
  ungroup()

# Filter and summarize for booster reports
sum_total_boost_inc <- mod_total %>%
  filter(is.na(PERSONS_BOOSTER_ADD_DOSE) == FALSE,
         is.na(WBINCOME_LONG_STATUS) == FALSE) %>%
  group_by(date, WBINCOME_LONG_STATUS) %>%
  summarize(count = n()) %>%
  mutate(type = "Booster",
         count_total = case_when(
           WBINCOME_LONG_STATUS == "High income" ~ count_hic,
           WBINCOME_LONG_STATUS == "Upper middle income" ~ count_umic,
           WBINCOME_LONG_STATUS == "Lower middle income" ~ count_lmic,
           WBINCOME_LONG_STATUS == "Low income" ~ count_lic,
           NA ~ TRUE
         ),
         count_per = count / count_total) %>%
  ungroup()

# Combine count tables
count_time_total_inc_cumul <- rbind(sum_total_cps_inc, 
                                    sum_total_boost_inc) %>%
  mutate(population = "Total population") %>%
  rename(DATE = date)


# Total - combine monthly and cumulative ----------------------------------

count_time_total <- count_time_total_month %>%
  filter(type !=  "cps") %>%
  left_join(., count_time_total_cumul, by = c("date" = "date", "type" = "type")) %>%
  mutate(group = "total") %>%
  rename(DATE = date)


# Combined ----------------------------------------------------------------

count_time <- rbind(count_time_hcw, count_time_old, count_time_total) %>%
  rename(date = DATE) %>%
  filter(date < as.Date("2024-01-01") & date > as.Date("2020-12-01"))

count_time_inc <- rbind(count_time_hcw_inc_cumul, 
                        count_time_old_inc_cumul,
                        count_time_total_inc_cumul)


# End ---------------------------------------------------------------------
