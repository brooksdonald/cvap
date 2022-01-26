

# COVID-19 vaccine implementation analysis & insights



# Structure ---------------------------------------------------------------

# General
# Entity characteristics
# Daily vaccination rate
# Supply secured and/or expected
# Supply received
# Additional data
# Ranking and binning
# Consolidate master summary
# Condense supplementary dataframes
# Export



# General -----------------------------------------------------------------

# Load packages
library("readxl")
library("writexl")
library("countrycode")
library("dplyr")
library("lubridate")
library("tidyr")
library("data.table")



# Entity characteristics --------------------------------------------------

# Load entity base data
b_details <-
  data.frame(read_excel("input/static/base_entitydetails.xlsx",
                        sheet = "data"))

# Reduce number of rows/rename columns
b_details_red <-
  select(
    b_details,
    c(
      "CODE",
      "NAMEWORKEN",
      "ABREVPUBLEN",
      "CONTINENT",
      "WHOREGIONC",
      "WHO14SUBREGIONS",
      "UNICEFREGION",
      "WHO_LEGAL_STATUS_TITLE",
      "COVAX",
      "WBINCOMESTATUS"
    )
  )

colnames(b_details_red) <-
  c(
    "a_iso",
    "a_name_long",
    "a_name_short",
    "a_continent",
    "a_who_region",
    "a_who_subregion",
    "a_unicef_region",
    "a_who_status",
    "a_covax_status",
    "a_income_group"
  )

# Rework WHO region and income group
b_details_red <- b_details_red %>%
  mutate (a_who_region = if_else(
    a_who_region == "AMRO",
    "AMR",
    if_else(
      a_who_region == "AFRO",
      "AFR",
      if_else(
        a_who_region == "EMRO",
        "EMR",
        if_else(
          a_who_region == "EURO",
          "EUR",
          if_else(
            a_who_region == "SEARO",
            "SEAR",
            if_else(a_who_region == "WPRO", "WPR",
                    "Other")
          )
        )
      )
    )
  ))

b_details_red <- b_details_red %>%
  mutate(a_income_group = if_else(
    grepl("High income", a_income_group),
    "HIC",
    if_else(
      a_income_group == "Upper middle income",
      "UMIC",
      if_else(
        a_income_group == "Lower middle income",
        "LMIC",
        if_else(a_income_group == "Low income", "LIC",
                "Other")
      )
    )
  ))



# Daily vaccination rate --------------------------------------------------

# Load current, lm, and 2m daily vxrate datasets
b_vxrate <-
  data.frame(read_excel("input/base_dvr_current.xlsx",
                        sheet = "data"))

b_vxrate_lw_sum <-
  data.frame(read_excel("input/base_dvr_lastweek.xlsx",
                        sheet = "data_summary"))

b_vxrate_lm_sum <-
  data.frame(read_excel("input/base_dvr_lastmonth.xlsx",
                        sheet = "data_summary"))

b_vxrate_2m_sum <-
  data.frame(read_excel("input/base_dvr_twomonth.xlsx",
                        sheet = "data_summary"))

# Current dataset
## Select relevant columns and rename
b_vxrate <-
  select(
    b_vxrate,
    c(
      "iso_code",
      "population",
      "date",
      "total_doses",
      "at_least_one_dose",
      "fully_vaccinated",
      "persons_booster_add_dose",
      "rolling_4_week_avg_td",
      "rolling_4_week_avg_fv",
      "max_rolling_4_week_avg_td_per100",
      "rolling_4_week_avg_td_lastmonth",
      "no_change_from_previous"
    )
  )

colnames(b_vxrate) <-
  c(
    "a_iso",
    "a_pop",
    "adm_date",
    "adm_td",
    "adm_a1d",
    "adm_fv",
    "adm_booster",
    "dvr_4wk_td",
    "dvr_4wk_fv",
    "dvr_4wk_td_max_per",
    "dvr_4wk_td_lm",
    "note_nochange"
  )

## Change population field type to numeric
b_vxrate$a_pop <- as.numeric(b_vxrate$a_pop)

## Add entity base data
b_vxrate <- left_join(b_vxrate, b_details_red, by = "a_iso")

## Add year, month, and week numbers
b_vxrate <- b_vxrate %>%
  mutate(adm_date_year = if_else(
    year(adm_date) == 2021,
    2021,
    if_else(year(adm_date) == 2022, 2022,
            NA_real_)
  )) %>%
  
  mutate(adm_date_month = ifelse(
    year(adm_date) == 2021,
    month(adm_date),
    ifelse(year(adm_date) == 2022, month(adm_date) + 12,
           NA)
  )) %>%
  
  mutate(adm_date_week = if_else(
    year(adm_date) == 2021 | year(adm_date) == 2022,
    isoweek(adm_date),
    NA_integer_
  ))

## Remove pre-2021 entries
b_vxrate <-
  filter(b_vxrate, adm_date_year == 2021 | adm_date_year == 2022)

## Indicate latest entry per ISO code
b_vxrate <- b_vxrate %>%
  group_by(a_iso) %>%
  mutate (adm_latest = if_else(adm_date == max(adm_date), "Yes", "No"))

## Indicate if latest entry is reported within the current or past week
b_vxrate$adm_is_current <- ifelse(
  b_vxrate$adm_latest == "Yes" &
    (
      b_vxrate$adm_date_week == isoweek(Sys.Date())
      |
        b_vxrate$adm_date_week == isoweek(Sys.Date()) - 1
    ),
  "Yes",
  NA
)

b_vxrate <- b_vxrate %>%
  group_by(a_iso, adm_date_week) %>%
  mutate (adm_date_maxweek = if_else(adm_date == max(adm_date), "Yes", "No"))

## Indicate if end of month
b_vxrate <- b_vxrate %>%
  group_by(a_iso, adm_date_month) %>%
  mutate (adm_date_eom = if_else(adm_date == max(adm_date), "Yes", "No"))

## Indicate most recent month
b_vxrate <- b_vxrate %>%
  group_by(a_iso) %>%
  mutate (adm_date_lastmonth = if_else(adm_date_week == (max(adm_date_week) -
                                                           4) &
                                         adm_date_maxweek == "Yes", "Yes", "No"))



## Create clean long form subsets and select relevant columns
b_vxrate_pub <-
  select(
    b_vxrate,
    c(
      "a_iso",
      "a_name_short",
      "a_pop",
      "adm_date",
      "adm_td",
      "adm_a1d",
      "adm_fv",
      "adm_booster",
      "dvr_4wk_td",
      "a_who_region",
      "a_covax_status",
      "a_income_group"
    )
  )

b_vxrate_amc <- filter(b_vxrate, a_covax_status == "AMC")
b_vxrate_amc <-
  select(
    b_vxrate_amc,
    c(
      "a_iso",
      "a_name_short",
      "a_pop",
      "adm_date",
      "adm_td",
      "adm_a1d",
      "adm_fv",
      "adm_booster",
      "dvr_4wk_td",
      "a_who_region",
      "a_covax_status",
      "a_income_group"
    )
  )

### Calculate dvr_4wk_td_per in long form subsets
b_vxrate_pub <- b_vxrate_pub %>%
  mutate(dvr_4wk_td_per = dvr_4wk_td / a_pop)

b_vxrate_amc <- b_vxrate_amc %>%
  mutate(dvr_4wk_td_per = dvr_4wk_td / a_pop)



## Create population target deadline tables
### Create end of September 2021 table
c_vxrate_sept <-
  filter(b_vxrate,
         adm_date_eom == "Yes" &
           adm_date_month == 9 & adm_date_year == 2021)

#### Calculate cov_total_fv at end of September 2021
c_vxrate_sept <- c_vxrate_sept %>%
  mutate(cov_total_fv = if_else(
    adm_a1d == 0 & adm_fv == 0 & adm_booster == 0, ((adm_td / 2) / a_pop),
    if_else(adm_a1d == 0 & adm_fv == 0 & adm_booster != 0, (((adm_td - adm_booster)/ 2) / a_pop),
            if_else(adm_a1d != 0 & adm_fv == 0 & adm_booster == 0, ((adm_td - adm_a1d) / a_pop),
                    if_else(adm_a1d != 0 & adm_fv == 0 & adm_booster != 0, ((adm_td - adm_a1d - adm_booster) / a_pop),
                            (adm_fv / a_pop))))))

#### Indicate if cov_total_fv is greater than or equal to 10%
c_vxrate_sept <- c_vxrate_sept %>%
  mutate(t10_goalmet_sep = if_else(cov_total_fv >= .1, "Yes", "No"))

#### Reduce to a_iso and t10_goalmet_sep
c_vxrate_sept_t10 <-
  select(c_vxrate_sept, c("a_iso", "t10_goalmet_sep"))



### Create end of December 2021 table
c_vxrate_dec <-
  filter(b_vxrate,
         adm_date_eom == "Yes" &
           adm_date_month == 12 & adm_date_year == 2021)

#### Calculate cov_total_fv at end of December 2021
c_vxrate_dec <- c_vxrate_dec %>%
  mutate(cov_total_fv = if_else(
    adm_a1d == 0 & adm_fv == 0 & adm_booster == 0, ((adm_td / 2) / a_pop),
    if_else(adm_a1d == 0 & adm_fv == 0 & adm_booster != 0, (((adm_td - adm_booster)/ 2) / a_pop),
            if_else(adm_a1d != 0 & adm_fv == 0 & adm_booster == 0, ((adm_td - adm_a1d) / a_pop),
                    if_else(adm_a1d != 0 & adm_fv == 0 & adm_booster != 0, ((adm_td - adm_a1d - adm_booster) / a_pop),
                            (adm_fv / a_pop))))))

#### Indicate if cov_total_fv is greater than or equal to 20%
c_vxrate_dec <- c_vxrate_dec %>%
  mutate(t20_goalmet_dec = if_else(cov_total_fv > .2, "Yes", "No"))

#### Indicate if cov_total_fv is greater than or equal to 40%
c_vxrate_dec <- c_vxrate_dec %>%
  mutate(t40_goalmet_dec = if_else(cov_total_fv > .4, "Yes", "No"))

#### Reduce to a_iso, t20_goalmet_dec, t40_goalmet_dec
c_vxrate_dec_t2040 <-
  select(c_vxrate_dec,
         c("a_iso", "t20_goalmet_dec", "t40_goalmet_dec"))



## Create absorption by month table
c_vxrate_eom <- filter(b_vxrate, adm_date_eom == "Yes")

### Select relevant columns
c_vxrate_eom <-
  select(
    c_vxrate_eom,
    c(
      "a_iso",
      "a_who_region",
      "a_continent",
      "a_covax_status",
      "a_income_group",
      "adm_date_month",
      "adm_td"
    )
  )

### Create temporary previous month data frame to allow calculation
z_vxrate_eom_temp <-
  select(c_vxrate_eom, c("a_iso", "adm_date_month", "adm_td"))
z_vxrate_eom_temp <- z_vxrate_eom_temp %>%
  mutate(adm_date_month = adm_date_month + 1)
colnames(z_vxrate_eom_temp) <-
  c("a_iso", "adm_date_month", "adm_td_lm")

### Calculate change by month
c_vxrate_eom <-
  left_join(
    c_vxrate_eom,
    z_vxrate_eom_temp,
    by = c("a_iso" = "a_iso", "adm_date_month" = "adm_date_month")
  )

c_vxrate_eom <- c_vxrate_eom %>%
  mutate(adm_td_absorbed = adm_td - adm_td_lm)

c_vxrate_eom <- c_vxrate_eom %>%
  mutate(adm_td_absorbed = if_else(is.na(adm_td_absorbed), adm_td, adm_td_absorbed))

## Summarize absorption by grouping by month
### COVAX participation = AMC
c_vxrate_eom_amc <- filter(c_vxrate_eom, a_covax_status == "AMC")
d_absorption_amc <- c_vxrate_eom_amc %>%
  group_by(adm_date_month) %>%
  summarize(absorption_amc = sum(adm_td_absorbed))

#### COVAX participation = AMC91
c_vxrate_eom_amc91 <-
  filter(c_vxrate_eom, a_covax_status == "AMC" & a_iso != "IND")
d_absorption_amc91 <- c_vxrate_eom_amc91 %>%
  group_by(adm_date_month) %>%
  summarize(absorption_amc91 = sum(adm_td_absorbed))

#### Continent = Africa
c_vxrate_eom_africa <- filter(c_vxrate_eom, a_continent == "Africa")
d_absorption_africa <- c_vxrate_eom_africa %>%
  group_by(adm_date_month) %>%
  summarize(absorption_africa = sum(adm_td_absorbed))

#### WHO region = EMR
c_vxrate_eom_emr <- filter(c_vxrate_eom, a_who_region == "EMR")
d_absorption_emr <- c_vxrate_eom_emr %>%
  group_by(adm_date_month) %>%
  summarize(absorption_emr = sum(adm_td_absorbed))

#### WHO region = AFR
c_vxrate_eom_afr <- filter(c_vxrate_eom, a_who_region == "AFR")
d_absorption_afr <- c_vxrate_eom_afr %>%
  group_by(adm_date_month) %>%
  summarize(absorption_afr = sum(adm_td_absorbed))

#### WHO region = SEAR
c_vxrate_eom_sear <- filter(c_vxrate_eom, a_who_region == "SEAR")
d_absorption_sear <- c_vxrate_eom_sear %>%
  group_by(adm_date_month) %>%
  summarize(absorption_sear = sum(adm_td_absorbed))

#### WHO region = WPR
c_vxrate_eom_wpr <- filter(c_vxrate_eom, a_who_region == "WPR")
d_absorption_wpr <- c_vxrate_eom_wpr %>%
  group_by(adm_date_month) %>%
  summarize(absorption_wpr = sum(adm_td_absorbed))

#### WHO region = EUR
c_vxrate_eom_eur <- filter(c_vxrate_eom, a_who_region == "EUR")
d_absorption_eur <- c_vxrate_eom_eur %>%
  group_by(adm_date_month) %>%
  summarize(absorption_eur = sum(adm_td_absorbed))

#### WHO region = AMR
c_vxrate_eom_amr <- filter(c_vxrate_eom, a_who_region == "AMR")
d_absorption_amr <- c_vxrate_eom_amr %>%
  group_by(adm_date_month) %>%
  summarize(absorption_amr = sum(adm_td_absorbed))

#### Additional groupings as needed
#### ...

### Merge groupings monthly absorption data
d_absorption <-
  left_join(d_absorption_amc, d_absorption_africa, by = "adm_date_month") %>%
  left_join(., d_absorption_emr, by = "adm_date_month") %>%
  left_join(., d_absorption_amr, by = "adm_date_month") %>%
  left_join(., d_absorption_afr, by = "adm_date_month") %>%
  left_join(., d_absorption_sear, by = "adm_date_month") %>%
  left_join(., d_absorption_wpr, by = "adm_date_month") %>%
  left_join(., d_absorption_eur, by = "adm_date_month") %>%
  left_join(., d_absorption_amc91, by = "adm_date_month")

### Add full date names for visualization
d_absorption <- d_absorption %>%
  mutate(adm_date_month_name = if_else(
    adm_date_month == 1,
    "2021-01",
    if_else(
      adm_date_month == 2,
      "2021-02",
      if_else(
        adm_date_month == 3,
        "2021-03",
        if_else(
          adm_date_month == 4,
          "2021-04",
          if_else(
            adm_date_month == 5,
            "2021-05",
            if_else(
              adm_date_month == 6,
              "2021-06",
              if_else(
                adm_date_month == 7,
                "2021-07",
                if_else(
                  adm_date_month == 8,
                  "2021-08",
                  if_else(
                    adm_date_month == 9,
                    "2021-09",
                    if_else(
                      adm_date_month == 10,
                      "2021-10",
                      if_else(
                        adm_date_month == 11,
                        "2021-11",
                        if_else(
                          adm_date_month == 12,
                          "2021-12",
                          if_else(adm_date_month == 13, "2022-01",
                                  NA_character_)
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  ))



## Create latest value summary table
c_vxrate_latest <- filter(b_vxrate, adm_latest == "Yes")

### Remove is_latest column
c_vxrate_latest <- select(c_vxrate_latest, -c("adm_latest"))



# Last week dataset
## Select relevant columns and rename
b_vxrate_lw_sum <-
  select(
    b_vxrate_lw_sum,
    c(
      "iso_code",
      "rolling_4_week_avg_td",
      "rolling_4_week_avg_td_lastmonth",
      "fully_vaccinated"
    )
  )
colnames(b_vxrate_lw_sum) <-
  c("a_iso", "dvr_4wk_td_lw", "dvr_4wk_td_lw_lm", "adm_fv_lw")

## Calculate percent change and category
b_vxrate_change_lw <- b_vxrate_lw_sum %>%
  mutate(dvr_4wk_td_change_lw_lm = dvr_4wk_td_lw - dvr_4wk_td_lw_lm) %>%
  
  mutate(dvr_4wk_td_change_lw_lm_per = dvr_4wk_td_change_lw_lm / dvr_4wk_td_lw_lm) %>%
  
  mutate(
    dvr_4wk_td_change_lw_lm_per_cat = if_else(
      dvr_4wk_td_change_lw_lm_per <= -0.25,
      "1) < (-25)%",
      if_else(
        dvr_4wk_td_change_lw_lm_per >= 0.25,
        "4) > 25%",
        if_else(
          dvr_4wk_td_change_lw_lm_per <= 0,
          "2) (-25)-0%",
          if_else(dvr_4wk_td_change_lw_lm_per > 0, "3) 0-25%",
                  NA_character_)
        )
      )
    )
  )


## Select relevant columns for dvr category count change table
b_vxrate_change_lw <-
  select(b_vxrate_change_lw,
         "a_iso",
         "dvr_4wk_td_change_lw_lm_per_cat")

## Select relevant columns for coverage category count change table
b_vxrate_cov <- select(b_vxrate_lw_sum, "a_iso", "adm_fv_lw")

c_vxrate_latest <-
  left_join(c_vxrate_latest, b_vxrate_cov, by = "a_iso")



# Last month dataset
## Select relevant columns and rename
b_vxrate_lm_sum <-
  select(b_vxrate_lm_sum,
         c("iso_code", "total_doses", "fully_vaccinated", "at_least_one_dose","persons_booster_add_dose"))
colnames(b_vxrate_lm_sum) <- c("a_iso", "adm_td_lm", "adm_fv_lm","adm_a1d_lm","adm_booster_lm")

## Merge with current summary dataset
c_vxrate_latest <-
  left_join(c_vxrate_latest, b_vxrate_lm_sum, by = "a_iso")



# Two month dataset
## Select relevant columns and rename
b_vxrate_2m_sum <-
  select(b_vxrate_2m_sum,
         c("iso_code", "total_doses", "fully_vaccinated", "at_least_one_dose"))
colnames(b_vxrate_2m_sum) <- c("a_iso", "adm_td_2m", "adm_fv_2m","adm_a1d_2m")

## Merge with current summary dataset
c_vxrate_latest <-
  left_join(c_vxrate_latest, b_vxrate_2m_sum, by = "a_iso")



# Population coverage in target groups ------------------------------------

# Load datasets




# Supply secured and/or expected ------------------------------------------

# Load summary and forecast datasets
b_sec <-
  data.frame(read_excel("input/base_supply_secured_summary.xlsx",
                        sheet = "supply_tracker"))

b_sec_forecast <-
  data.frame(read_excel("input/base_supply_secured_forecasts.xlsx",
                        sheet = "data"))



# Supply secured summary
## Select relevant columns and rename
c_sec_cour <-
  select(
    b_sec,
    c(
      "ISO3",
      "Secured.and.or.Expected.Vaccine..millions.of.courses.",
      "Bilateral.Deals..millions.of.courses.",
      "Bilateral.Donations..millions.of.courses.",
      "COVAX.Total..millions.of.courses.",
      "EU.Deal..millions.of.courses.",
      "Other.sources..millions.of.courses.",
      "Domestic.Supply..millions.of.courses."
    )
  )

colnames(c_sec_cour) <-
  c(
    "iso",
    "sec_total",
    "sec_bilat",
    "sec_donat",
    "sec_covax",
    "sec_eu",
    "sec_other",
    "sec_domestic"
  )

## Multiply by one million
c_sec_cour <- c_sec_cour %>%
  mutate(sec_total = sec_total * 1000000) %>%
  mutate(sec_bilat = sec_bilat * 1000000) %>%
  mutate(sec_donat = sec_donat * 1000000) %>%
  mutate(sec_covax = sec_covax * 1000000) %>%
  mutate(sec_other = sec_other * 1000000) %>%
  mutate(sec_eu = sec_eu * 1000000) %>%
  mutate(sec_domestic = sec_domestic * 1000000)

## Replace NAs and roundup
c_sec_cour <- c_sec_cour %>% replace(is.na(.), 0)

c_sec_cour <- c_sec_cour %>%
  mutate_if(is.numeric, round)

## Consolidate other secured sources for visualization
c_sec_cour <- c_sec_cour %>%
  mutate(sec_other_sum = sec_eu + sec_other + sec_domestic)

## Add dataset date
c_sec_cour$sec_date <- as.Date("2022-01-20")



# Supply forecasts
## Select relevant columns
c_sec_forecast <-
  select(
    b_sec_forecast,
    c(
      "iso3",
      "month",
      "total",
      "bilateral",
      "donation",
      "covax",
      "avat",
      "unknown"
    )
  )

## Convert month field type to character
c_sec_forecast$month <- as.character(c_sec_forecast$month)

## Create by-month forecast tables
c_sec_forecast_nov <- filter(c_sec_forecast, month == "2021-11-30")
colnames(c_sec_forecast_nov) <-
  c(
    "iso",
    "month",
    "sec_cum_total_nov",
    "sec_bilat_nov",
    "sec_donat_nov",
    "sec_covax_nov",
    "sec_avat_nov",
    "sec_unknown_nov"
  )

c_sec_forecast_dec <- filter(c_sec_forecast, month == "2021-12-31")
colnames(c_sec_forecast_dec) <-
  c(
    "iso",
    "month",
    "sec_cum_total_dec",
    "sec_bilat_dec",
    "sec_donat_dec",
    "sec_covax_dec",
    "sec_avat_dec",
    "sec_unknown_dec"
  )

c_sec_forecast_jan <- filter(c_sec_forecast, month == "2022-01-31")
colnames(c_sec_forecast_jan) <-
  c(
    "iso",
    "month",
    "sec_cum_total_jan",
    "sec_bilat_jan",
    "sec_donat_jan",
    "sec_covax_jan",
    "sec_avat_jan",
    "sec_unknown_jan"
  )

c_sec_forecast_feb <- filter(c_sec_forecast, month == "2022-02-28")
colnames(c_sec_forecast_feb) <-
  c(
    "iso",
    "month",
    "sec_cum_total_feb",
    "sec_bilat_feb",
    "sec_donat_feb",
    "sec_covax_feb",
    "sec_avat_feb",
    "sec_unknown_feb"
  )

c_sec_forecast_mar <- filter(c_sec_forecast, month == "2022-03-31")
colnames(c_sec_forecast_mar) <-
  c(
    "iso",
    "month",
    "sec_cum_total_mar",
    "sec_bilat_mar",
    "sec_donat_mar",
    "sec_covax_mar",
    "sec_avat_mar",
    "sec_unknown_mar"
  )

c_sec_forecast_apr <- filter(c_sec_forecast, month == "2022-04-30")
colnames(c_sec_forecast_apr) <-
  c(
    "iso",
    "month",
    "sec_cum_total_apr",
    "sec_bilat_apr",
    "sec_donat_apr",
    "sec_covax_apr",
    "sec_avat_apr",
    "sec_unknown_apr"
  )

c_sec_forecast_may <- filter(c_sec_forecast, month == "2022-05-31")
colnames(c_sec_forecast_may) <-
  c(
    "iso",
    "month",
    "sec_cum_total_may",
    "sec_bilat_may",
    "sec_donat_may",
    "sec_covax_may",
    "sec_avat_may",
    "sec_unknown_may"
  )

c_sec_forecast_jun <- filter(c_sec_forecast, month == "2022-06-30")
colnames(c_sec_forecast_jun) <-
  c(
    "iso",
    "month",
    "sec_cum_total_jun",
    "sec_bilat_jun",
    "sec_donat_jun",
    "sec_covax_jun",
    "sec_avat_jun",
    "sec_unknown_jun"
  )

## Combine all by-month tables
c_sec_forecast_all <-
  left_join(c_sec_forecast_nov, c_sec_forecast_dec, by = "iso") %>%
  left_join(., c_sec_forecast_jan, by = "iso") %>%
  left_join(., c_sec_forecast_feb, by = "iso") %>%
  left_join(., c_sec_forecast_mar, by = "iso") %>%
  left_join(., c_sec_forecast_apr, by = "iso") %>%
  left_join(., c_sec_forecast_may, by = "iso") %>%
  left_join(., c_sec_forecast_jun, by = "iso")

## Select monthly totals
c_sec_forecast_totals <-
  select(
    c_sec_forecast_all,
    c(
      "iso",
      "sec_cum_total_nov",
      "sec_cum_total_dec",
      "sec_cum_total_jan",
      "sec_cum_total_feb",
      "sec_cum_total_mar",
      "sec_cum_total_apr",
      "sec_cum_total_may",
      "sec_cum_total_jun"
    )
  )

## Calculate difference between monthly totals
c_sec_forecast_totals <- c_sec_forecast_totals %>%
  mutate(dec_add_total_dec = sec_cum_total_dec - sec_cum_total_nov) %>%
  mutate(dec_add_total_jan = sec_cum_total_jan - sec_cum_total_dec) %>%
  mutate(dec_add_total_feb = sec_cum_total_feb - sec_cum_total_jan) %>%
  mutate(dec_add_total_mar = sec_cum_total_mar - sec_cum_total_feb) %>%
  mutate(dec_add_total_apr = sec_cum_total_apr - sec_cum_total_mar) %>%
  mutate(dec_add_total_may = sec_cum_total_may - sec_cum_total_apr) %>%
  mutate(dec_add_total_jun = sec_cum_total_jun - sec_cum_total_may)



# Supply received ---------------------------------------------------------

# Load current, lm, and 2m datasets
b_mdb <-
  data.frame(read_excel("input/base_supply_received_current.xlsx",
                        sheet = "Delivery_Table"))

b_mdb_lm <-
  data.frame(read_excel("input/base_supply_received_lastmonth.xlsx",
                        sheet = "Delivery_Table"))

b_mdb_2m <-
  data.frame(read_excel("input/base_supply_received_twomonth.xlsx",
                        sheet = "Delivery_Table"))

# Add ISO codes, remove entries without ISO codes, and rename columns
## Current
b_mdb$iso <-
  countrycode(
    b_mdb$Country.territory,
    origin = 'country.name',
    destination = 'iso3c',
    warn = TRUE
  )
b_mdb <-
  b_mdb %>% mutate(iso = replace(iso, Country.territory == "Kosovo", "XKX"))
b_mdb <- b_mdb[!(is.na(b_mdb$iso)),]
b_mdb <- select(b_mdb, -c("Country.territory"))
b_mdb$del_date <- as.Date("2022-01-26")
colnames(b_mdb) <- c(
  "product",
  "bimultilat",
  "donations",
  "covax",
  "avat",
  "unknown",
  "total",
  "iso",
  "del_date"
)

## Last month
b_mdb_lm$iso <-
  countrycode(
    b_mdb_lm$Country.territory,
    origin = 'country.name',
    destination = 'iso3c',
    warn = TRUE
  )
b_mdb_lm <-
  b_mdb_lm %>% mutate(iso = replace(iso, Country.territory == "Kosovo", "XKX"))
b_mdb_lm <- b_mdb_lm[!(is.na(b_mdb_lm$iso)),]
b_mdb_lm <- select(b_mdb_lm, -c("Country.territory"))
colnames(b_mdb_lm) <-
  c(
    "product",
    "bimultilat_lm",
    "donations_lm",
    "covax_lm",
    "avat_lm",
    "unknown_lm",
    "total_lm",
    "iso"
  )

## Two months ago
b_mdb_2m$iso <-
  countrycode(
    b_mdb_2m$Country.territory,
    origin = 'country.name',
    destination = 'iso3c',
    warn = TRUE
  )
b_mdb_2m <-
  b_mdb_2m %>% mutate(iso = replace(iso, Country.territory == "Kosovo", "XKX"))
b_mdb_2m <- b_mdb_2m[!(is.na(b_mdb_2m$iso)),]
b_mdb_2m <- select(b_mdb_2m, -c("Country.territory"))
colnames(b_mdb_2m) <-
  c(
    "product",
    "bimultilat_2m",
    "donations_2m",
    "covax_2m",
    "avat_2m",
    "unknown_2m",
    "total_2m",
    "iso"
  )

# Combine current, last month, and two month delivery tables
c_delivery_all <-
  left_join(b_mdb, b_mdb_lm, by = c("iso" = "iso", "product" = "product")) %>%
  left_join(., b_mdb_2m, by = c("iso" = "iso", "product" = "product"))

# Calculate doses
## Current summary
c_delivery_doses <-
  select(
    c_delivery_all,
    c(
      "iso",
      "product",
      "bimultilat",
      "donations",
      "covax",
      "avat",
      "unknown",
      "total",
      "total_lm",
      "total_2m"
    )
  )

c_delivery_doses <- c_delivery_doses %>%
  group_by(iso) %>%
  summarize_at(
    c(
      "bimultilat",
      "donations",
      "covax",
      "avat",
      "unknown",
      "total",
      "total_lm",
      "total_2m"
    ),
    sum,
    na.rm = TRUE
  )
colnames(c_delivery_doses) <-
  c(
    "iso",
    "del_dose_bilat",
    "del_dose_donat",
    "del_dose_covax",
    "del_dose_avat",
    "del_dose_unkwn",
    "del_dose_total",
    "del_dose_total_lm",
    "del_dose_total_2m"
  )



## Calculate doses delivered since last month and since previous two months
c_delivery_doses <- c_delivery_doses %>%
  mutate(del_dose_since_lm = del_dose_total - del_dose_total_lm)

c_delivery_doses <- c_delivery_doses %>%
  mutate(del_dose_prior_2m = del_dose_total_2m)

c_delivery_doses <- c_delivery_doses %>%
  mutate(del_dose_lm_2m = del_dose_total_lm - del_dose_total_2m)



## Calculate doses received per product
c_delivery_doses_product <- b_mdb %>%
  group_by(iso, product) %>%
  summarize_at(c("bimultilat", "donations", "covax", "avat", "unknown",
                 "total"),
               sum,
               na.rm = TRUE)

### Rename products for visualization
c_delivery_doses_product <- c_delivery_doses_product %>%
  mutate(product_short = if_else(
    product == "Gamaleya - Sputnik V",
    "Non-COVAX product",
    if_else(
      product == "Bharat - Covaxin",
      "Non-COVAX product",
      if_else(
        product == "CanSino - Ad5-nCOV",
        "Non-COVAX product",
        if_else(
          product == "Gamaleya - Sputnik Light",
          "Non-COVAX product",
          if_else(
            product == "Chumakov - Covi-Vac",
            "Non-COVAX product",
            if_else(
              product == "RIBSP - QazCovid",
              "Non-COVAX product",
              if_else(
                product == "SRCVB - EpiVacCorona",
                "Non-COVAX product",
                if_else(
                  product == "Medigen - MVC-COV1901",
                  "Non-COVAX product",
                  if_else(
                    product == "Anhui ZL - Recombinant SARS-CoV-2 vaccine",
                    "Non-COVAX product",
                    if_else(
                      product == "Sinopharm (Wuhan) - Inactivated",
                      "Non-COVAX product",
                      if_else(
                        product == "AstraZeneca - Vaxzevria",
                        "AZ",
                        if_else(
                          product == "SII - Covishield",
                          "SII",
                          if_else(
                            product == "Pfizer BioNTech - Comirnaty",
                            "Pfizer",
                            if_else(
                              product == "Sinopharm (Beijing) - BBIBP-CorV",
                              "Sinopharm",
                              if_else(
                                product == "Moderna - Spikevax",
                                "Moderna",
                                if_else(
                                  product == "Sinovac - CoronaVac",
                                  "Sinovac",
                                  if_else(
                                    product == "CIGB - CIGB-66",
                                    "Non-COVAX product",
                                    if_else(
                                      product == "Janssen - Ad26.COV 2.S",
                                      "J&J",
                                      if_else(
                                        product == "BBIL - Covaxin",
                                        "Non-COVAX product",
                                        if_else(
                                          product == "CIGB - Abdala",
                                          "Non-COVAX product",
                                          if_else(
                                            product == "Soberana 2",
                                            "Non-COVAX product",
                                            if_else(
                                              product == "SII - Covavax",
                                              "Novavax",
                                              if_else(product == "Unknown", "Unknown",
                                                      NA_character_)
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  ))

colnames(c_delivery_doses_product) <-
  c(
    "a_iso",
    "product",
    "bimultilat",
    "donations",
    "covax",
    "avat",
    "unknown",
    "total",
    "product_short"
  )



# Calculate courses
## Separate 1-dose courses
c_delivery_courses_1d <-
  filter(c_delivery_all, product == "Janssen - Ad26.COV 2.S")

### Select relevant columns and rename
c_delivery_courses_1d <-
  select(
    c_delivery_courses_1d,
    c(
      "iso",
      "del_date",
      "bimultilat",
      "donations",
      "covax",
      "avat",
      "unknown",
      "total",
      "total_lm",
      "total_2m"
    )
  )

colnames(c_delivery_courses_1d) <-
  c(
    "iso",
    "del_date",
    "bimultilat_1d",
    "donations_1d",
    "covax_1d",
    "avat_1d",
    "unknown_1d",
    "total_1d",
    "total_lm_1d",
    "total_2m_1d"
  )

### Replace NAs with 0
c_delivery_courses_1d <- c_delivery_courses_1d %>%
  replace_na(
    list(
      "bimultilat_1d" = 0,
      "donations_1d"  = 0,
      "covax_1d" = 0,
      "avat_1d" = 0,
      "unknown_1d" = 0,
      "total_1d" = 0,
      "total_lm_1d" = 0,
      "total_2m_1d" = 0
    )
  )

## Calculate estimated 1-dose course wastage
c_delivery_courses_1d <- c_delivery_courses_1d %>%
  mutate(wast_1d = total_1d * 0.1)

## Separate 2-dose courses
c_delivery_courses_2d <-
  filter(c_delivery_all, product != "Janssen - Ad26.COV 2.S")

### Select relevant columns and rename
c_delivery_courses_2d <- c_delivery_courses_2d %>%
  group_by(iso) %>%
  summarize_at(
    c(
      "bimultilat",
      "donations",
      "covax",
      "avat",
      "unknown",
      "total",
      "total_lm",
      "total_2m"
    ),
    sum,
    na.rm = TRUE
  )

colnames(c_delivery_courses_2d) <-
  c(
    "iso",
    "bimultilat_2d",
    "donations_2d",
    "covax_2d",
    "avat_2d",
    "unknown_2d",
    "total_2d",
    "total_lm_2d",
    "total_2m_2d"
  )

## Calculate estimated 2-dose course wastage
c_delivery_courses_2d <- c_delivery_courses_2d %>%
  mutate(wast_2d = total_2d * 0.1)

### Divide by 2 to calculate number courses
c_delivery_courses_2d <- c_delivery_courses_2d %>%
  mutate(bimultilat_2d = bimultilat_2d / 2) %>%
  mutate(donations_2d = donations_2d / 2) %>%
  mutate(covax_2d = covax_2d / 2) %>%
  mutate(avat_2d = avat_2d / 2) %>%
  mutate(unknown_2d = unknown_2d / 2) %>%
  mutate(total_2d = total_2d / 2) %>%
  mutate(wast_2d = wast_2d / 2) %>%
  mutate(total_lm_2d = total_lm_2d / 2) %>%
  mutate(total_2m_2d = total_2m_2d / 2)

## Combine 1-dose and 2-dose courses tables
c_delivery_courses <-
  left_join(c_delivery_courses_2d, c_delivery_courses_1d, by = "iso")

## Replace NAs with 0
c_delivery_courses <- c_delivery_courses %>%
  mutate(across(-c(del_date), ~ replace_na(., 0)))

## Sum 1-dose and 2-dose courses and round
c_delivery_courses <- c_delivery_courses %>%
  mutate(del_cour_bilat = bimultilat_1d + bimultilat_2d) %>%
  mutate(del_cour_donat = donations_1d + donations_2d) %>%
  mutate(del_cour_covax = covax_1d + covax_2d) %>%
  mutate(del_cour_avat = avat_1d + avat_2d) %>%
  mutate(del_cour_unkwn = unknown_1d + unknown_2d) %>%
  mutate(del_cour_total = total_1d + total_2d) %>%
  mutate(del_cour_wast = wast_1d + wast_2d) %>%
  mutate(del_cour_total_lm = total_lm_1d + total_lm_2d) %>%
  mutate(del_cour_total_2m = total_2m_1d + total_2m_2d)

c_delivery_courses <- c_delivery_courses %>%
  mutate_if(is.numeric, round)

## Select relevant columns
c_delivery_courses <-
  select(
    c_delivery_courses,
    c(
      "iso",
      "del_date",
      "del_cour_bilat",
      "del_cour_donat",
      "del_cour_covax",
      "del_cour_avat",
      "del_cour_unkwn",
      "del_cour_total",
      "del_cour_wast",
      "del_cour_total_lm",
      "del_cour_total_2m"
    )
  )

## Calculate courses delivered since last month and two month
c_delivery_courses <- c_delivery_courses %>%
  mutate(del_cour_since_lm = del_cour_total - del_cour_total_lm)

c_delivery_courses <- c_delivery_courses %>%
  mutate(del_cour_prior_2m = del_cour_total_2m)

c_delivery_courses <- c_delivery_courses %>%
  mutate(del_cour_lm_2m = del_cour_total_lm - del_cour_total_2m)

# Combine doses and courses delivered tables
c_delivery <-
  left_join(c_delivery_doses, c_delivery_courses, by = "iso")



# Additional data ---------------------------------------------------------

# Load datasets
b_smartsheet <-
  data.frame(read_excel("input/base_smartsheet.xlsx",
                        sheet = "data"))

b_who_dashboard <-
  fread("https://covid19.who.int/who-data/vaccination-data.csv")
head(b_who_dashboard)


# IMR Smartsheet
## Select relevant columns and rename
c_smartsheet_red <-
  select(
    b_smartsheet,
    c(
      "ISO3",
      "NDVP...Coverage.target..",
      "NDVP...Coverage.deadline",
      "NDVP.Target.Population",
      "At.risk.for.expiry",
      "Driving.factors"
    )
  )

colnames(c_smartsheet_red) <-
  c(
    "iso",
    "ndvp_target",
    "ndvp_deadline",
    "ndvp_tarpop",
    "expiry_risk",
    "note_ss_drivers"
  )


## Rename expiry risk
c_smartsheet_red <- c_smartsheet_red %>%
  mutate(expiry_risk = if_else(
    expiry_risk == "Red",
    "Doses at risk",
    if_else(
      expiry_risk == "Yellow",
      "Doses under observation",
      if_else(expiry_risk == "Green", "No doses at risk",
              "Unknown")
    )
  ))

## Change country target field type to date
c_smartsheet_red$ndvp_deadline <-
  as.Date(c_smartsheet_red$ndvp_deadline)



# WHO COVID-19 Dashboard
## Select relevant columns and rename
c_whodb_red <-
  select(b_who_dashboard,
         c("ISO3", "NUMBER_VACCINES_TYPES_USED", "FIRST_VACCINE_DATE"))
colnames(c_whodb_red) <- c("iso", "prod_inuse", "intro_date")



# Consolidate master summary ----------------------------------------------

# Remove duplicative base details from latest vxrate summary
c_vxrate_latest_red <-
  select(
    c_vxrate_latest,-c(
      "a_continent",
      "a_who_region",
      "a_income_group",
      "a_covax_status",
      "a_unicef_region",
      "a_name_short",
      "a_name_long",
      "a_who_subregion",
      "a_who_status"
    )
  )

# Merge details and latest vxrate summary dataframes
a_data <-
  left_join(b_details_red, c_vxrate_latest_red, by = "a_iso")

# Calculate introduction status
a_data <- a_data %>%
  mutate(intro_status = if_else(
    is.na(adm_td) | adm_td == 0,
    "No product introduced",
    "Product introduced"
  ))


# Assign population size category
a_data <- a_data %>%
  mutate(a_pop_cat = if_else(
    a_pop < 1000000,
    "1) <1M",
    if_else(
      a_pop < 10000000,
      "2) 1-10M",
      if_else(
        a_pop < 100000000,
        "3) 10-100M",
        if_else(a_pop >= 100000000, "4) 100M+",
                NA_character_)
      )
    )
  ))


# Calculate theoretical fully vaccinated for non-reporters for current, lm, and 2m
a_data <- a_data %>%
  mutate(adm_fv_homo = if_else(
    adm_a1d == 0 & adm_fv == 0 & adm_booster == 0, (adm_td / 2),
    if_else(adm_a1d == 0 & adm_fv == 0 & adm_booster != 0, ((adm_td - adm_booster)/ 2),
            if_else(adm_a1d != 0 & adm_fv == 0 & adm_booster == 0, (adm_td - adm_a1d),
                    if_else(adm_a1d != 0 & adm_fv == 0 & adm_booster != 0, (adm_td - adm_a1d - adm_booster),
                            (adm_fv)))))) %>%
  
  mutate(adm_fv_lm_homo = if_else(
    adm_a1d_lm == 0 & adm_fv == 0 & adm_booster_lm == 0, (adm_td_lm / 2),
    if_else(adm_a1d_lm == 0 & adm_fv == 0 & adm_booster_lm != 0, ((adm_td_lm - adm_booster_lm)/ 2),
            if_else(adm_a1d_lm != 0 & adm_fv == 0 & adm_booster_lm == 0, (adm_td_lm - adm_a1d_lm),
                    if_else(adm_a1d_lm != 0 & adm_fv == 0 & adm_booster_lm != 0, (adm_td_lm - adm_a1d_lm - adm_booster_lm),
                            (adm_fv)))))) %>%
  
  mutate(adm_fv_2m_homo = if_else(
    adm_a1d_2m == 0 & adm_fv == 0, (adm_td_2m / 2),
            if_else(adm_a1d_2m != 0 & adm_fv == 0, (adm_td_2m - adm_a1d_2m),
                            (adm_fv))))

# Calculate td and fv change from lm and 2m
a_data <- a_data %>%
  mutate(adm_td_less_1m = adm_td - adm_td_lm) %>%
  
  mutate(adm_td_1m_2m = adm_td_lm - adm_td_2m) %>%
  
  mutate(adm_fv_less_1m = adm_fv_homo - adm_fv_lm_homo) %>%
  
  mutate(adm_fv_1m_2m = adm_fv_lm_homo - adm_fv_2m_homo)


# Calculate adm_a1d and adm_fv coverage for current, lm, and 2m, including change
a_data <- a_data %>%
  mutate(cov_total_a1d = adm_a1d / a_pop) %>%
  
  mutate(cov_total_fv = if_else((adm_fv_homo / a_pop) > 1, 1, (adm_fv_homo / a_pop))) %>%
  
  mutate(cov_total_fv_lw = adm_fv_lw / a_pop) %>%
  
  mutate(cov_total_fv_lm = adm_fv_lm_homo / a_pop) %>%
  
  mutate(cov_total_fv_2m = adm_fv_2m_homo / a_pop) %>%
  
  mutate(cov_total_fv_less_1m = cov_total_fv - cov_total_fv_lm) %>%
  
  mutate(cov_total_fv_1m_2m = cov_total_fv_lm - cov_total_fv_2m) %>%
  
  mutate(cov_total_fv_less_1m_prop = cov_total_fv_less_1m / cov_total_fv)


# Assign coverage category for current and lw
a_data <- a_data %>%
  mutate(cov_total_fv_cat = if_else(
    cov_total_fv < 0.01,
    "1) 0-1%",
    if_else(
      cov_total_fv < 0.1,
      "2) 1-10%",
      if_else(
        cov_total_fv < 0.2,
        "3) 10-20%",
        if_else(
          cov_total_fv < 0.4,
          "4) 20-40%",
          if_else(cov_total_fv >= 0.4, "5) 40%+",
                  NA_character_)
        )
      )
    )
  )) %>%
  
  
  mutate(cov_total_fv_lw_cat = if_else(
    cov_total_fv_lw < 0.01,
    "1) 0-1%",
    if_else(
      cov_total_fv_lw < 0.1,
      "2) 1-10%",
      if_else(
        cov_total_fv_lw < 0.2,
        "3) 10-20%",
        if_else(
          cov_total_fv_lw < 0.4,
          "4) 20-40%",
          if_else(cov_total_fv_lw >= 0.4, "5) 40%+",
                  NA_character_)
        )
      )
    )
  ))


# Calculate linear population coverage projection by 30 June 2022
a_data <- a_data %>%
  mutate(cov_total_fv_atpace_30jun = (adm_fv_homo + (dvr_4wk_fv * (
    as.numeric(as.Date("2022-06-30") - Sys.Date())
  ))) / a_pop)

# Calculate 4-week average daily rates as % of pop.
a_data <- a_data %>%
  mutate(dvr_4wk_td_per = dvr_4wk_td / a_pop) %>%
  
  mutate(dvr_4wk_fv_per = dvr_4wk_fv / a_pop)


# Assign vaccination rate category
a_data <- a_data %>%
  mutate(dvr_4wk_td_per_cat = if_else(
    dvr_4wk_td_per < 0.0015,
    "1) Low (< 0.15%*)",
    if_else(
      dvr_4wk_td_per < 0.0035,
      "2) Medium (< 0.35%)",
      if_else(
        dvr_4wk_td_per < 0.0065,
        "3) High (< 0.65%)",
        if_else(dvr_4wk_td_per >= 0.0065, "4) Very high (> 0.65%)",
                NA_character_)
      )
    )
  ))


# Calculate (percent) change in 4-week average daily vaccination rate & assign category
a_data <- a_data %>%
  mutate(dvr_4wk_td_change_lm = dvr_4wk_td - dvr_4wk_td_lm) %>%
  
  mutate(dvr_4wk_td_change_lm_per = dvr_4wk_td_change_lm / dvr_4wk_td_lm) %>%
  
  mutate(
    dvr_4wk_td_change_lm_per_cat = if_else(
      dvr_4wk_td_change_lm_per <= -0.25,
      "1) < (-25)%",
      if_else(
        dvr_4wk_td_change_lm_per >= 0.25,
        "4) > 25%",
        if_else(
          dvr_4wk_td_change_lm_per <= 0,
          "2) (-25)-0%",
          if_else(dvr_4wk_td_change_lm_per > 0, "3) 0-25%",
                  NA_character_)
        )
      )
    )
  ) %>%
  
  mutate(
    dvr_4wk_td_change_lm_trend = if_else(
      dvr_4wk_td_change_lm_per <= -0.25,
      "Downward",
      if_else(
        dvr_4wk_td_change_lm_per >= 0.25,
        "Upward",
        if_else(
          dvr_4wk_td_change_lm_per < 0.25 &
            dvr_4wk_td_change_lm_per > -0.25,
          "Stable",
          NA_character_
        )
      )
    )
  )


# Progress against coverage targets
## 10% target
a_data <- left_join(a_data, c_vxrate_sept_t10, by = "a_iso")

a_data <- a_data %>%
  mutate(t10_goalmet_sep = if_else(a_iso == "BDI", "No", t10_goalmet_sep)) %>%
  
  mutate(t10_goalmet_after = if_else(cov_total_fv >= 0.1, "Yes", "No")) %>%
  
  mutate(t10_notmet = if_else(cov_total_fv < 0.1, "Yes", "No")) %>%
  
  mutate(t10_timeto = if_else((((
    0.1 * a_pop
  ) - adm_fv_homo) / (dvr_4wk_fv)) < 0 , 0,
  if_else(is.infinite((((0.1 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
  )), NA_real_,
  (((0.1 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
  )))) %>%
  
  mutate(t10_status = if_else(
    t10_goalmet_sep == "Yes" ,
    "1) Goal met by deadline",
    if_else(
      t10_goalmet_after == "Yes",
      "2) Goal met after deadline",
      if_else(t10_notmet == "Yes", "3) Goal not yet met",
              NA_character_)
    )
  ))


## 20%/40% target
a_data <- left_join(a_data, c_vxrate_dec_t2040, by = "a_iso")

a_data <- a_data %>%
  mutate(t20_goalmet_after = if_else(cov_total_fv >= 0.2, "Yes", "No")) %>%
  
  mutate(t20_notmet = if_else(cov_total_fv < 0.2, "Yes", "No")) %>%
  
  mutate(t20_timeto = if_else((((
    0.2 * a_pop
  ) - adm_fv_homo) / (dvr_4wk_fv)) < 0 , 0,
  if_else(is.infinite((((0.2 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
  )), NA_real_,
  (((0.2 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
  )))) %>%
  
  mutate(t20_status = if_else(
    t20_goalmet_dec == "Yes" |
      (is.na(t20_goalmet_dec) & adm_td > 0),
    "1) Goal met by deadline",
    if_else(
      t20_goalmet_after == "Yes",
      "2) Goal met after deadline",
      if_else(t20_notmet == "Yes", "3) Goal not yet met",
              NA_character_)
    )
  )) %>%
  
  mutate(t40_goalmet_after = if_else(cov_total_fv >= 0.4, "Yes", "No")) %>%
  
  mutate(t40_notmet = if_else(cov_total_fv < 0.4, "Yes", "No")) %>%
  
  mutate(t40_timeto = if_else((((
    0.4 * a_pop
  ) - adm_fv_homo) / (dvr_4wk_fv)) < 0 , 0,
  if_else(is.infinite((((0.4 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
  )), NA_real_,
  (((0.4 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
  )))) %>%
  
  mutate(t40_status = if_else(
    t40_goalmet_dec == "Yes" |
      (is.na(t40_goalmet_dec) & adm_td > 0),
    "1) Goal met by deadline",
    if_else(
      t40_goalmet_after == "Yes",
      "2) Goal met after deadline",
      if_else(t40_notmet == "Yes", "3) Goal not yet met",
              NA_character_)
    )
  )) %>%
  
  
  mutate(t40_jun_peratpace = (adm_fv_homo + (dvr_4wk_fv * (
    as.numeric(as.Date("2022-06-30") - Sys.Date())
  ))) / a_pop) %>%
  
  mutate(t40_jun_willmeet = if_else(t40_jun_peratpace >= 0.4, "Yes", "No"))


## 70% target
a_data <- a_data %>%
  mutate(t70_goalmet = if_else(cov_total_fv >= 0.7, "Yes", "No")) %>%
  
  mutate(t70_willmeet = if_else(cov_total_fv_atpace_30jun >= 0.7, "Yes", "No")) %>%
  
  mutate(t70_ontrack = if_else(t70_goalmet != "Yes" &
                                 t70_willmeet == "Yes", "Yes", "No")) %>%
  
  mutate(t70_offtrack = if_else(t70_goalmet != "Yes" &
                                  t70_willmeet != "Yes", "Yes", "No")) %>%
  
  mutate(t70_timeto = if_else((((
    0.7 * a_pop
  ) - adm_fv_homo) / (dvr_4wk_fv)) < 0 , 0,
  if_else(is.infinite((((0.7 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
  )), NA_real_,
  (((0.7 * a_pop) - adm_fv_homo) / (dvr_4wk_fv)
  )))) %>%
  
  mutate(t70_status = if_else(
    t70_goalmet == "Yes",
    "Goal met",
    if_else(
      t70_ontrack == "Yes",
      "On track",
      if_else(t70_offtrack == "Yes", "Off track",
              NA_character_)
    )
  )) %>%
  
  mutate(t70_status_num = if_else(
    t70_goalmet == "Yes",
    1,
    if_else(
      t70_ontrack == "Yes",
      2,
      if_else(t70_offtrack == "Yes", 3,
              NA_real_)
    )
  )) %>%
  
  mutate(t70_rate_needed = ((a_pop * 0.7) - adm_fv_homo) / as.numeric(as.Date("2022-06-30") - Sys.Date())) %>%
  
  mutate(t70_scaleup = t70_rate_needed / dvr_4wk_fv) %>%
  
  mutate(t70_scaleup_cat = if_else(
    t70_scaleup == 0,
    "1) Goal met",
    if_else(
      t70_scaleup <= 2,
      "2) <2x",
      if_else(
        t70_scaleup <= 5,
        "3) 3-5x",
        if_else(
          t70_scaleup <= 10,
          "4) 5-10x",
          if_else(t70_scaleup > 10, "5) >10x",
                  NA_character_)
        )
      )
    )
  ))


# Booster and additional doses
a_data <- a_data %>%
  mutate(cov_total_booster = adm_booster / a_pop) %>%
  
  mutate(
    booster_status = if_else(
      adm_booster > 0,
      "Administering booster/additional doses",
      "Not reporting on booster/additional dose administration"
    )
  ) %>%
  
  mutate(cov_total_booster_cat = if_else(
    cov_total_booster > .1,
    "4) >10%",
    if_else(
      cov_total_booster > .05,
      "3) 5-9.9%",
      if_else(
        cov_total_booster > .01,
        "2) 1-4.9%",
        if_else(
          cov_total_booster > 0,
          "1) 0-0.9%",
          if_else(
            is.na(cov_total_booster) |
              cov_total_booster == 0,
            "0) Not reporting",
            NA_character_
          )
        )
      )
    )
  ))


# Merge supply secured and/or expected data
a_data <- left_join(a_data, c_sec_cour, by = c("a_iso" = "iso"))


# Calculate secured courses as percent of population
a_data <- a_data %>%
  mutate(sec_total_per = sec_total / a_pop)


# Assign secured courses category
a_data <- a_data %>%
  mutate(sec_total_per_cat = if_else(
    sec_total_per < 0.25,
    "0) <25%",
    if_else(
      sec_total_per < 0.5,
      "1) 25-49%",
      if_else(
        sec_total_per < 0.75,
        "2) 50-74%",
        if_else(
          sec_total_per < 1,
          "3) 75-100%",
          if_else(sec_total_per >= 1, "4) >100%",
                  NA_character_)
        )
      )
    )
  ))


# Calculate supply secured proportions of totals
a_data <- a_data %>%
  mutate(sec_covax_prop = sec_covax / (sec_bilat + sec_covax + sec_donat + sec_other_sum)) %>%
  
  mutate(sec_noncovaxdonat_prop = (sec_bilat + sec_other_sum) / sec_total) %>%
  
  mutate(sec_bilat_prop = sec_bilat / sec_total)


# Merge supply received data
a_data <- left_join(a_data, c_delivery, by = c("a_iso" = "iso"))


# Calculate delivered courses as percent of population
a_data <- a_data %>%
  mutate(del_cour_total_per = del_cour_total / a_pop) %>%
  
  mutate(del_cour_since_lm_per = del_cour_since_lm / a_pop) %>%
  
  mutate(del_wast_per = del_cour_wast / a_pop)


# Assign received courses category
a_data <- a_data %>%
  mutate(del_cour_total_per_cat = if_else(
    del_cour_total_per < 0.25,
    "0) <25%",
    if_else(
      del_cour_total_per < 0.5,
      "1) 25-49%",
      if_else(
        del_cour_total_per < 0.75,
        "2) 50-74%",
        if_else(
          del_cour_total_per < 1,
          "3) 75-100%",
          if_else(del_cour_total_per >= 1, "4) >100%",
                  NA_character_)
        )
      )
    )
  ))


# Calculate supply received proportions of total
a_data <- a_data %>%
  mutate(del_cour_covax_prop = del_cour_covax / del_cour_total) %>%
  
  mutate(del_cour_since_lm_prop = del_cour_since_lm / del_cour_total)


# Calculate supply influx status
a_data <- a_data %>%
  mutate(del_influx_status = if_else(del_cour_since_lm_per >= 0.1,
                                     "Yes", "No"))


# Product utilization
## Calculate remaining doses, absolute and % pop.
a_data <- a_data %>%
  mutate(pu_del_rem = if_else(
    is.na(del_dose_total) | del_dose_total == 0,
    NA_real_,
    ((del_dose_total * 0.9) - adm_td)
  )) %>%
  
  mutate(pu_del_rem_per = pu_del_rem / a_pop)

## Calculate percent of doses received utilized
a_data <- a_data %>%
  mutate(pu_del_rem_prop = if_else(
    pu_del_rem > 0,
    (pu_del_rem / del_dose_total),
    if_else(
      is.na(pu_del_rem) | pu_del_rem == 0,
      NA_real_,
      if_else(pu_del_rem <= 0, 0,
              NA_real_)
    )
  )) %>%
  
  mutate(pu_used_per = 1 - pu_del_rem_prop)

## Assign percent utilization categories
a_data <- a_data %>%
  mutate(pu_used_per_cat = if_else(
    pu_used_per < 0.25,
    "0) <25%",
    if_else(
      pu_used_per < 0.5,
      "1) 25-49%",
      if_else(
        pu_used_per < 0.75,
        "2) 50-74%",
        if_else(pu_used_per <= 1, "3) 75-100%",
                NA_character_)
      )
    )
  ))


# Calculate supply secured not yet delivered, supply received not yet administered
a_data <- a_data %>%
  mutate(sec_tobedel = if_else((sec_total - del_cour_total) < 0, 0, (sec_total - del_cour_total))) %>%
  
  mutate(sec_tobedel_per = sec_tobedel / a_pop) %>%
  
  mutate(rem_cour_del = del_cour_total - del_cour_wast - adm_fv_homo) %>%
  
  mutate(rem_cour_del_per = rem_cour_del / a_pop) %>%
  
  mutate(rem_cour_del_prop = rem_cour_del / del_cour_total)


# Calculate proportions of courses of total
a_data <- a_data %>%
  mutate(sec_del_prop = del_cour_total / sec_total)


# Calculate if courses secured, received, and administered sufficient to reach targets
a_data <- a_data %>%
  mutate(t20_suff_sec = if_else(sec_total_per >= 0.2, "Yes", "No")) %>%
  
  mutate(t20_suff_del = if_else(del_cour_total_per >= 0.2, "Yes", "No")) %>%
  
  mutate(t40_suff_sec = if_else(sec_total_per >= 0.4, "Yes", "No")) %>%
  
  mutate(t40_suff_del = if_else(del_cour_total_per >= 0.4, "Yes", "No")) %>%
  
  mutate(t70_suff_sec = if_else(sec_total_per >= 0.7, "Yes", "No")) %>%
  
  mutate(t70_suff_del = if_else(del_cour_total_per >= 0.7, "Yes", "No"))


# Calculate absolute courses needed for reach targets
a_data <- a_data %>%
  mutate(t20_cour_req = (a_pop * 0.2) * 1.1) %>%
  
  mutate(t40_cour_req = (a_pop * 0.4) * 1.1) %>%
  
  mutate(t70_cour_req = (a_pop * 0.7) * 1.1)

# Calculate remaining secured, received, and admnistered courses required for targets
a_data <- a_data %>%
  mutate(t20_cour_need_sec = if_else((t20_cour_req - sec_total) < 0, 0,
                                     (t20_cour_req - sec_total))) %>%
  
  mutate(t20_cour_need_del = if_else((t20_cour_req - del_cour_total) < 0,
                                     0,
                                     (t20_cour_req - del_cour_total)
  )) %>%
  
  mutate(t20_cour_need_admin = if_else((t20_cour_req - adm_fv_homo) < 0, 0,
                                       (t20_cour_req - adm_fv_homo))) %>%
  
  mutate(t40_cour_need_sec = if_else((t40_cour_req - sec_total) < 0, 0,
                                     (t40_cour_req - sec_total))) %>%
  
  mutate(t40_cour_need_del = if_else((t40_cour_req - del_cour_total) < 0,
                                     0,
                                     (t40_cour_req - del_cour_total)
  )) %>%
  
  mutate(t40_cour_need_admin = if_else((t40_cour_req - adm_fv_homo) < 0, 0,
                                       (t40_cour_req - adm_fv_homo))) %>%
  
  mutate(t70_cour_need_sec = if_else((t70_cour_req - sec_total) < 0, 0,
                                     (t70_cour_req - sec_total))) %>%
  
  mutate(t70_cour_need_del = if_else((t70_cour_req - del_cour_total) < 0,
                                     0,
                                     (t70_cour_req - del_cour_total)
  )) %>%
  
  mutate(t70_cour_need_admin = if_else((t70_cour_req - adm_fv_homo) < 0, 0,
                                       (t70_cour_req - adm_fv_homo)))


# Merge IMR Smartsheet data
a_data <-
  left_join(a_data, c_smartsheet_red, by = c("a_iso" = "iso"))


# Calculate progress against country coverage targets
a_data <- a_data %>%
  mutate(ndvp_goalmet = if_else(cov_total_fv >= ndvp_target, "Yes", "No")) %>%
  
  mutate(ndvp_peratpace = (adm_fv_homo + (
    dvr_4wk_fv * as.numeric(as.Date(ndvp_deadline) - Sys.Date())
  ) / a_pop)) %>%
  
  mutate(ndvp_willmeet = if_else(ndvp_peratpace >= ndvp_target, "Yes", "No")) %>%
  
  mutate(ndvp_ontrack = if_else(ndvp_goalmet != "Yes" &
                                  ndvp_willmeet == "Yes", "Yes", "No")) %>%
  
  mutate(ndvp_offtrack = if_else(ndvp_goalmet != "Yes" &
                                   ndvp_willmeet != "Yes", "Yes", "No")) %>%
  
  mutate(ndvp_status = if_else(
    is.na(ndvp_target) | ndvp_target == 0,
    "Not captured",
    if_else(
      is.na(ndvp_deadline),
      "No timeline",
      if_else(
        ndvp_goalmet == "Yes",
        "Goal met",
        if_else(
          ndvp_deadline < Sys.Date(),
          "Deadline past",
          if_else(
            ndvp_ontrack == "Yes",
            "On track",
            if_else(ndvp_offtrack == "Yes", "Off track",
                    NA_character_)
          )
        )
      )
    )
  )) %>%
  
  mutate(ndvp_status_num = if_else(
    is.na(ndvp_target) | ndvp_target == 0,
    1,
    if_else(
      is.na(ndvp_deadline),
      2,
      if_else(
        ndvp_goalmet == "Yes",
        3,
        if_else(
          ndvp_deadline < Sys.Date(),
          4,
          if_else(
            ndvp_ontrack == "Yes",
            5,
            if_else(ndvp_offtrack == "Yes", 6,
                    NA_real_)
          )
        )
      )
    )
  )) %>%
  
  mutate(ndvp_rate_needed = ((a_pop * ndvp_target) - adm_fv_homo) / as.numeric(ndvp_deadline - Sys.Date())) %>%
  
  mutate(ndvp_scaleup = ndvp_rate_needed / dvr_4wk_fv) %>%
  
  mutate(ndvp_scaleup_cat = if_else(
    ndvp_scaleup == 0,
    "1) Goal met",
    if_else(
      ndvp_scaleup <= 2,
      "2) <2x",
      if_else(
        ndvp_scaleup <= 5,
        "3) 3-5x",
        if_else(
          ndvp_scaleup <= 10,
          "4) 5-10x",
          if_else(ndvp_scaleup > 10, "5) >10x",
                  NA_character_)
        )
      )
    )
  ))


# Merge WHO Dashboard data
a_data <- left_join(a_data, c_whodb_red, by = c("a_iso" = "iso"))


# Add notes
a_data <- a_data %>%
  mutate(note_highcov = if_else(cov_total_fv > 0.5, "High fully vaccinated coverage", "No")) %>%
  
  mutate(note_recent_rollout = if_else(intro_date > (Sys.Date() - 60), "Recent rollout", "No")) %>%
  
  mutate(note_reporting_date = if_else(
    adm_date < (as.Date("2022-01-18") - 10),
    "Likely reporting issue",
    NA_character_
  )) %>%
  
  mutate(
    note_drivers_auto = if_else(
      note_reporting_date == "Likely reporting issue",
      "Likely reporting issue",
      if_else(
        note_nochange == 1,
        "Likely reporting issue",
        if_else(
          dvr_4wk_td_per < 0,
          "Likely reporting issue",
          if_else(
            note_highcov == "High fully vaccinated coverage",
            "High fully vaccinated coverage",
            if_else(
              note_recent_rollout == "Recent rollout",
              "Recent rollout",
              NA_character_
            )
          )
        )
      )
    )
  ) %>%
  
  mutate(note_supplyconstraint = if_else(rem_cour_del_per < 0.05 &
                                           pu_used_per > 0.5, 1, 0))

a_data <- a_data %>%
  mutate(note_drivers = if_else(
    is.na(if_else(
      is.na(note_drivers_auto),
      note_ss_drivers,
      if_else(
        is.na(note_ss_drivers),
        note_drivers_auto,
        paste(note_drivers_auto, note_ss_drivers, sep = ", ")
      )
    )),
    "None",
    if_else(
      is.na(note_drivers_auto),
      note_ss_drivers,
      if_else(
        is.na(note_ss_drivers),
        note_drivers_auto,
        paste(note_drivers_auto, note_ss_drivers, sep = ", ")
      )
    )
  ))

a_data <- a_data %>%
  mutate(
    cats_a = if_else(
      cov_total_fv < 0.1 &
        t70_willmeet == "No",
      "Country for concerted support",
      "Other country"
    )
  )


# Sort columns
a_data <- a_data %>%
  select("a_iso", sort(colnames(.)))


# Create AMC summary table

a_data_amc <- filter(a_data, a_covax_status == "AMC")
a_data_hic <- filter(a_data, a_income_group == "HIC")



# Ranking and binning -----------------------------------------------------

# Coverage fully vaccinated (% pop.)
a_data <- a_data %>%
  group_by(a_covax_status) %>%
  
  mutate(cov_total_fv_rank = row_number(cov_total_fv)) %>%
  
  mutate(cov_total_fv_bins = ntile(cov_total_fv_rank, 2))


##4-week average daily vaccination rate (% pop. / day)
a_data <- a_data %>%
  group_by(a_covax_status) %>%
  
  mutate(dvr_4wk_td_per_rank = row_number(dvr_4wk_td_per)) %>%
  
  mutate(dvr_4wk_td_per_bins = ntile(dvr_4wk_td_per_rank, 2))


##Supply secured and/or expected (% pop.)
a_data <- a_data %>%
  group_by(a_covax_status) %>%
  
  mutate(sec_total_per_rank = row_number(sec_total_per)) %>%
  
  mutate(sec_total_per_bins = ntile(sec_total_per_rank, 2))


##Supply received (% pop.)
a_data <- a_data %>%
  group_by(a_covax_status) %>%
  
  mutate(del_cour_total_per_rank = row_number(del_cour_total_per)) %>%
  
  mutate(del_cour_total_per_bins = ntile(del_cour_total_per_rank, 2))


##ISO code
a_data <- a_data %>%
  group_by(a_covax_status) %>%
  
  mutate(iso_rank = row_number(a_iso)) %>%
  
  mutate(iso_bins = ntile(iso_rank, 2))


##Product utilization
a_data <- a_data %>%
  group_by(a_covax_status) %>%
  
  mutate(pu_used_per_rank = row_number(pu_used_per)) %>%
  
  mutate(pu_used_per_bins = ntile(pu_used_per_rank, 2))


##Short name
a_data <- a_data %>%
  group_by(a_covax_status, intro_status) %>%
  
  mutate(iso_vx_rank = row_number(a_name_short)) %>%
  
  mutate(iso_vx_bins = ntile(iso_vx_rank, 4))


##Proportion of supply received from COVAX
a_data <- a_data %>%
  group_by(a_covax_status, intro_status) %>%
  
  mutate(del_cour_covax_prop_rank = row_number(del_cour_covax_prop)) %>%
  
  mutate(del_cour_covax_prop_bins = ntile(del_cour_covax_prop_rank, 2))


##Proportion of supply delivered that remains
a_data <- a_data %>%
  group_by(a_covax_status, intro_status) %>%
  
  mutate(rem_cour_del_prop_rank = row_number(rem_cour_del_prop)) %>%
  
  mutate(rem_cour_del_prop_bins = ntile(rem_cour_del_prop_rank, 2))


##Supply received since last month (% pop.)
a_data <- a_data %>%
  group_by(a_covax_status, intro_status) %>%
  
  mutate(del_cour_since_lm_per_rank = row_number(del_cour_since_lm_per)) %>%
  
  mutate(del_cour_since_lm_per_bins = ntile(del_cour_since_lm_per_rank, 2))


##Proportion of coverage achieved in past month
a_data <- a_data %>%
  group_by(a_covax_status, intro_status) %>%
  
  mutate(cov_total_fv_less_1m_rank = row_number(cov_total_fv_less_1m_prop)) %>%
  
  mutate(cov_total_fv_less_1m_bins = ntile(cov_total_fv_less_1m_rank, 2))


##Proportion of secured courses that have been received
a_data <- a_data %>%
  group_by(a_covax_status, intro_status) %>%
  
  mutate(sec_del_prop_rank = row_number(sec_del_prop)) %>%
  
  mutate(sec_del_prop_bins = ntile(sec_del_prop_rank, 2))


##Proportion of supply secured from COVAX
a_data <- a_data %>%
  group_by(a_covax_status, intro_status) %>%
  
  mutate(sec_covax_prop_rank = row_number(sec_covax_prop)) %>%
  
  mutate(sec_covax_prop_bins = ntile(sec_covax_prop_rank, 2)) %>%
  
  data.frame()



# Condense supplementary dataframes ---------------------------------------

# Create base condense file
c_condense <-
  select(
    a_data,
    c(
      "a_iso",
      "a_name_short",
      "a_continent",
      "a_who_region",
      "a_who_status",
      "a_covax_status",
      "dvr_4wk_td_per_cat",
      "dvr_4wk_td_change_lm_trend",
      "cov_total_fv_cat",
      "t10_status",
      "t20_status",
      "t40_status",
      "t70_status",
      "t70_scaleup_cat",
      "booster_status",
      "pu_used_per_cat",
      "del_cour_total_per_cat",
      "sec_total_per_cat"
    )
  )

# Filter by grouping
c_condense_amc <- filter(c_condense, a_covax_status == "AMC")
c_condense_amc_exc <-
  filter(c_condense, a_covax_status == "AMC" & a_iso != "NIC")
c_condense_africa <- filter(c_condense, a_continent == "Africa")

c_condense_afr <- filter(c_condense, a_who_region == "AFR")
c_condense_amr <- filter(c_condense, a_who_region == "AMR")
c_condense_emr <- filter(c_condense, a_who_region == "EMR")
c_condense_eur <- filter(c_condense, a_who_region == "EUR")
c_condense_sear <- filter(c_condense, a_who_region == "SEAR")
c_condense_wpr <- filter(c_condense, a_who_region == "WPR")

# Daily vaccination rate category
e_vrcat_amc <-
  aggregate(
    c_condense_amc_exc$a_name_short,
    list(c_condense_amc_exc$dvr_4wk_td_per_cat),
    paste,
    collapse = "; "
  )
colnames(e_vrcat_amc) <- c("dvr_cat", "cat_amc")
e_vrcat_africa <-
  aggregate(
    c_condense_africa$a_name_short,
    list(c_condense_africa$dvr_4wk_td_per_cat),
    paste,
    collapse = "; "
  )
colnames(e_vrcat_africa) <- c("dvr_cat", "cat_africa")
e_vrcat_amr <-
  aggregate(
    c_condense_amr$a_name_short,
    list(c_condense_amr$dvr_4wk_td_per_cat),
    paste,
    collapse = "; "
  )
colnames(e_vrcat_amr) <- c("dvr_cat", "cat_amr")
e_vrcat_emr <-
  aggregate(
    c_condense_emr$a_name_short,
    list(c_condense_emr$dvr_4wk_td_per_cat),
    paste,
    collapse = "; "
  )
colnames(e_vrcat_emr) <- c("dvr_cat", "cat_emr")

e_vrcat_all <-
  left_join(e_vrcat_amc, e_vrcat_africa, by = "dvr_cat") %>%
  left_join(., e_vrcat_emr, by = "dvr_cat") %>%
  left_join(., e_vrcat_amr, by = "dvr_cat")

e_vrcat_all <- e_vrcat_all %>% replace(is.na(.), "None")

# Monthly vaccination rate trend
e_trend_amc <-
  aggregate(
    c_condense_amc_exc$a_name_short,
    list(c_condense_amc_exc$dvr_4wk_td_change_lm_trend),
    paste,
    collapse = "; "
  )
colnames(e_trend_amc) <- c("dvr_trend", "trend_amc")
e_trend_africa <-
  aggregate(
    c_condense_africa$a_name_short,
    list(c_condense_africa$dvr_4wk_td_change_lm_trend),
    paste,
    collapse = "; "
  )
colnames(e_trend_africa) <- c("dvr_trend", "trend_africa")
e_trend_month_amr <-
  aggregate(
    c_condense_amr$a_name_short,
    list(c_condense_amr$dvr_4wk_td_change_lm_trend),
    paste,
    collapse = "; "
  )
colnames(e_trend_month_amr) <- c("dvr_trend", "trend_amr")
e_trend_month_emr <-
  aggregate(
    c_condense_emr$a_name_short,
    list(c_condense_emr$dvr_4wk_td_change_lm_trend),
    paste,
    collapse = "; "
  )
colnames(e_trend_month_emr) <- c("dvr_trend", "trend_emr")

e_trend_all <-
  left_join(e_trend_amc, e_trend_africa, by = "dvr_trend") %>%
  left_join(., e_trend_month_emr, by = "dvr_trend") %>%
  left_join(., e_trend_month_amr, by = "dvr_trend")

## Past coverage targets
### 10% target
e_cond_t10_amc <-
  aggregate(c_condense_amc$a_name_short,
            list(c_condense_amc$t10_status),
            paste,
            collapse = "; ")
colnames(e_cond_t10_amc) <- c("tar_stat", "t10_amc")
e_cond_t10_africa <-
  aggregate(
    c_condense_africa$a_name_short,
    list(c_condense_africa$t10_status),
    paste,
    collapse = "; "
  )
colnames(e_cond_t10_africa) <- c("tar_stat", "t10_africa")

### 20% target
e_cond_t20_amc <-
  aggregate(c_condense_amc$a_name_short,
            list(c_condense_amc$t20_status),
            paste,
            collapse = "; ")
colnames(e_cond_t20_amc) <- c("tar_stat", "t20_amc")
e_cond_t20_africa <-
  aggregate(
    c_condense_africa$a_name_short,
    list(c_condense_africa$t20_status),
    paste,
    collapse = "; "
  )
colnames(e_cond_t20_africa) <- c("tar_stat", "t20_africa")

### 40% target
e_cond_t40_amc <-
  aggregate(c_condense_amc$a_name_short,
            list(c_condense_amc$t40_status),
            paste,
            collapse = "; ")
colnames(e_cond_t40_amc) <- c("tar_stat", "t40_amc")
e_cond_t40_africa <-
  aggregate(
    c_condense_africa$a_name_short,
    list(c_condense_africa$t40_status),
    paste,
    collapse = "; "
  )
colnames(e_cond_t40_africa) <- c("tar_stat", "t40_africa")

e_cond_pasttar <-
  left_join(e_cond_t10_amc, e_cond_t20_amc, by = "tar_stat") %>%
  left_join(., e_cond_t40_amc, by = "tar_stat") %>%
  left_join(., e_cond_t10_africa, by = "tar_stat") %>%
  left_join(., e_cond_t20_africa, by = "tar_stat") %>%
  left_join(., e_cond_t40_africa, by = "tar_stat")

e_tar_past_all <- e_cond_pasttar %>% replace(is.na(.), "None")


## Current coverage targets
### 70% target by 30 Jun 2022
#### Status
e_cond_t70_amc <-
  aggregate(c_condense_amc$a_name_short,
            list(c_condense_amc$t70_status),
            paste,
            collapse = "; ")
colnames(e_cond_t70_amc) <- c("tar_stat", "t70_amc")
e_cond_t70_africa <-
  aggregate(
    c_condense_africa$a_name_short,
    list(c_condense_africa$t70_status),
    paste,
    collapse = "; "
  )
colnames(e_cond_t70_africa) <- c("tar_stat", "t70_africa")

e_tar_cur_all <-
  left_join(e_cond_t70_amc, e_cond_t70_africa, by = "tar_stat")
#left_join(., t40_stat_emr, by = "t40_status") %>%
#left_join(.,t40_stat_paho, by = "t40_status")

#### Scaleup
e_cond_t70_scale_amc <-
  aggregate(
    c_condense_amc$a_name_short,
    list(c_condense_amc$t70_scaleup_cat),
    paste,
    collapse = "; "
  )
colnames(e_cond_t70_scale_amc) <- c("scaleup_cat", "t70_amc")
e_cond_t70_scale_africa <-
  aggregate(
    c_condense_africa$a_name_short,
    list(c_condense_africa$t70_scaleup_cat),
    paste,
    collapse = "; "
  )
colnames(e_cond_t70_scale_africa) <- c("scaleup_cat", "t70_africa")

e_tar_cur_scale_all <-
  left_join(e_cond_t70_scale_amc, e_cond_t70_scale_africa, by = "scaleup_cat")
#left_join(., t40_emr, by = "scaleup_cat") %>%
#left_join(.,t40_paho, by = "scaleup_cat")

###NDVP coverage target
#e_ndvp_amc <- aggregate(c_condense_amc$name_short, list(c_condense_amc$ndv), paste, collapse="; ")
#colnames(e_ndvp_amc) <- c("ndvp_cat", "ndvp_amc")
#e_ndvp_africa <- aggregate(c_condense_africa$name_short, list(c_condense_africa$ndvp_status), paste, collapse="; ")
#colnames(e_ndvp_africa) <- c("ndvp_cat", "ndvp_africa")
#e_ndvp_amr <- aggregate(c_condense_amr$name_short, list(c_condense_amr$ndvp_status), paste, collapse="; ")
#colnames(e_ndvp_amr) <- c("ndvp_cat", "ndvp_amr")
#e_ndvp_emr <- aggregate(c_condense_emr$name_short, list(c_condense_emr$ndvp_status), paste, collapse="; ")
#colnames(e_ndvp_emr) <- c("ndvp_cat", "ndvp_emr")

#e_ndvp_all <- left_join(e_ndvp_amc, e_ndvp_africa, by = "ndvp_cat") %>%
#left_join(., e_ndvp_amr, by = "ndvp_cat") %>%
#left_join(., e_ndvp_emr, by = "ndvp_cat")

# Booster use
e_booster_amc <-
  aggregate(
    c_condense_amc$a_name_short,
    list(c_condense_amc$booster_status),
    paste,
    collapse = "; "
  )
colnames(e_booster_amc) <- c("booster_stat", "booster_amc")
e_booster_africa <-
  aggregate(
    c_condense_africa$a_name_short,
    list(c_condense_africa$booster_status),
    paste,
    collapse = "; "
  )
colnames(e_booster_africa) <- c("booster_stat", "booster_africa")
e_booster_amr <-
  aggregate(
    c_condense_amr$a_name_short,
    list(c_condense_amr$booster_status),
    paste,
    collapse = "; "
  )
colnames(e_booster_amr) <- c("booster_stat", "booster_amr")
e_booster_emr <-
  aggregate(
    c_condense_emr$a_name_short,
    list(c_condense_emr$booster_status),
    paste,
    collapse = "; "
  )
colnames(e_booster_emr) <- c("booster_stat", "booster_emr")

e_booster_all <-
  left_join(e_booster_amc, e_booster_africa, by = "booster_stat") %>%
  left_join(., e_booster_emr, by = "booster_stat") %>%
  left_join(., e_booster_amr, by = "booster_stat")


# Secured category
e_sec_amc <-
  aggregate(
    c_condense_amc$a_name_short,
    list(c_condense_amc$sec_total_per_cat),
    paste,
    collapse = "; "
  )
colnames(e_sec_amc) <- c("cat", "seccat_amc")
e_sec_africa <-
  aggregate(
    c_condense_africa$a_name_short,
    list(c_condense_africa$sec_total_per_cat),
    paste,
    collapse = "; "
  )
colnames(e_sec_africa) <- c("cat", "seccat_africa")
e_sec_amr <-
  aggregate(
    c_condense_amr$a_name_short,
    list(c_condense_amr$sec_total_per_cat),
    paste,
    collapse = "; "
  )
colnames(e_sec_amr) <- c("cat", "seccat_amr")
e_sec_emr <-
  aggregate(
    c_condense_emr$a_name_short,
    list(c_condense_emr$sec_total_per_cat),
    paste,
    collapse = "; "
  )
colnames(e_sec_emr) <- c("cat", "seccat_emr")

# Courses delivered category
e_del_amc <-
  aggregate(
    c_condense_amc$a_name_short,
    list(c_condense_amc$del_cour_total_per_cat),
    paste,
    collapse = "; "
  )
colnames(e_del_amc) <- c("cat", "delcat_amc")
e_del_africa <-
  aggregate(
    c_condense_africa$a_name_short,
    list(c_condense_africa$del_cour_total_per_cat),
    paste,
    collapse = "; "
  )
colnames(e_del_africa) <- c("cat", "delcat_africa")
e_del_amr <-
  aggregate(
    c_condense_amr$a_name_short,
    list(c_condense_amr$del_cour_total_per_cat),
    paste,
    collapse = "; "
  )
colnames(e_del_amr) <- c("cat", "delcat_amr")
e_del_emr <-
  aggregate(
    c_condense_emr$a_name_short,
    list(c_condense_emr$del_cour_total_per_cat),
    paste,
    collapse = "; "
  )
colnames(e_del_emr) <- c("cat", "delcat_emr")

# Product utilization category
e_pu_amc <-
  aggregate(
    c_condense_amc$a_name_short,
    list(c_condense_amc$pu_used_per_cat),
    paste,
    collapse = "; "
  )
colnames(e_pu_amc) <- c("cat", "pucat_amc")
e_pu_africa <-
  aggregate(
    c_condense_africa$a_name_short,
    list(c_condense_africa$pu_used_per_cat),
    paste,
    collapse = "; "
  )
colnames(e_pu_africa) <- c("cat", "pucat_africa")
e_pu_amr <-
  aggregate(
    c_condense_amr$a_name_short,
    list(c_condense_amr$pu_used_per_cat),
    paste,
    collapse = "; "
  )
colnames(e_pu_amr) <- c("cat", "pucat_paho")
e_pu_emr <-
  aggregate(
    c_condense_emr$a_name_short,
    list(c_condense_emr$pu_used_per_cat),
    paste,
    collapse = "; "
  )
colnames(e_pu_emr) <- c("cat", "pucat_emr")


e_secdelpu_all <- left_join(e_sec_amc, e_sec_africa, by = "cat") %>%
  left_join(., e_sec_emr, by = "cat") %>%
  left_join(., e_sec_amr, by = "cat") %>%
  left_join(., e_del_amc, by = "cat") %>%
  left_join(., e_del_africa, by = "cat") %>%
  left_join(., e_del_amr, by = "cat") %>%
  left_join(., e_del_emr, by = "cat") %>%
  left_join(., e_pu_amc, by = "cat") %>%
  left_join(., e_pu_africa, by = "cat") %>%
  left_join(., e_pu_amr, by = "cat") %>%
  left_join(., e_pu_emr, by = "cat")

e_secdelpu_all <- e_secdelpu_all %>% replace(is.na(.), "None")

# Coverage category
e_cov_amc <-
  aggregate(
    c_condense_amc$a_name_short,
    list(c_condense_amc$cov_total_fv_cat),
    paste,
    collapse = "; "
  )
colnames(e_cov_amc) <- c("cov_cat", "covcat_amc")
e_cov_africa <-
  aggregate(
    c_condense_africa$a_name_short,
    list(c_condense_africa$cov_total_fv_cat),
    paste,
    collapse = "; "
  )
colnames(e_cov_africa) <- c("cov_cat", "covcat_africa")
e_cov_amr <-
  aggregate(
    c_condense_amr$a_name_short,
    list(c_condense_amr$cov_total_fv_cat),
    paste,
    collapse = "; "
  )
colnames(e_cov_amr) <- c("cov_cat", "covcat_amr")
e_cov_emr <-
  aggregate(
    c_condense_emr$a_name_short,
    list(c_condense_emr$cov_total_fv_cat),
    paste,
    collapse = "; "
  )
colnames(e_cov_emr) <- c("cov_cat", "covcat_emr")

e_cov_all <- left_join(e_cov_amc, e_cov_africa, by = "cov_cat") %>%
  left_join(., e_cov_emr, by = "cov_cat") %>%
  left_join(., e_cov_amr, by = "cov_cat")


# Create values table
z_values <- data.frame(c("Text"))

z_values$ig_amc_lic <- 25
z_values$ig_amc_lmic <- 53
z_values$ig_amc_umic <- 12

z_values$wr_amc_afr <- 39
z_values$wr_amc_amr <- 10
z_values$wr_amc_emr <- 11
z_values$wr_amc_eur <- 6
z_values$wr_amc_sear <- 9
z_values$wr_amc_wpr <- 15

z_values$pop_amc_1m <- 20
z_values$pop_amc_10m <- 22
z_values$pop_amc_100m <- 40
z_values$pop_amc_100mp <- 8

z_values$pop_amc <- 3889355899
z_values$pop_amc_10 <- z_values$pop_amc * 0.1
z_values$pop_amc_20 <- z_values$pop_amc * 0.2
z_values$pop_amc_40 <- z_values$pop_amc * 0.4
z_values$pop_amc_70 <- z_values$pop_amc * 0.7


# Change count tables
## Daily vaccination rate percent change category
c_data_dvr_lm_cat <-
  select(
    a_data,
    c(
      "a_iso",
      "dvr_4wk_td_change_lm_per_cat",
      "a_covax_status",
      "intro_status"
    )
  )

f_dvr_change_count <-
  left_join(c_data_dvr_lm_cat, b_vxrate_change_lw, by = "a_iso")
f_dvr_change_count <-
  filter(f_dvr_change_count,
         a_covax_status == "AMC" &
           intro_status == "Product introduced")
f_dvr_change_count <-
  select(
    f_dvr_change_count,
    c(
      "a_iso",
      "dvr_4wk_td_change_lm_per_cat",
      "dvr_4wk_td_change_lw_lm_per_cat"
    )
  )

colnames(f_dvr_change_count) <-
  c("a_iso", "dvr_change_cat_lm_cur", "dvr_change_cat_lm_lw")

f_dvr_change_count_cur <- f_dvr_change_count %>%
  group_by(dvr_change_cat_lm_cur) %>%
  summarise(count_cur = n())
colnames(f_dvr_change_count_cur) <- c("cat", "count_cur")

f_dvr_change_count_lw <- f_dvr_change_count %>%
  group_by(dvr_change_cat_lm_lw) %>%
  summarise(count_lw = n())
colnames(f_dvr_change_count_lw) <- c("cat", "count_lw")

f_dvr_change_count <-
  left_join(f_dvr_change_count_cur, f_dvr_change_count_lw, by = "cat")

f_dvr_change_count <- f_dvr_change_count %>%
  mutate(count_change = count_cur - count_lw)

## Coverage category change
f_cov_change_count <-
  filter(a_data,
         a_covax_status == "AMC" &
           intro_status == "Product introduced")
f_cov_change_count <-
  select(f_cov_change_count,
         c("a_iso", "cov_total_fv_cat", "cov_total_fv_lw_cat"))

f_cov_change_count_cur <- f_cov_change_count %>%
  group_by(cov_total_fv_cat) %>%
  summarise(count_cur = n())
colnames(f_cov_change_count_cur) <- c("cat", "count_cur")

f_cov_change_count_lw <- f_cov_change_count %>%
  group_by(cov_total_fv_lw_cat) %>%
  summarise(count_lw = n())
colnames(f_cov_change_count_lw) <- c("cat", "count_lw")

f_cov_change_count <-
  left_join(f_cov_change_count_cur, f_cov_change_count_lw, by = "cat")

f_cov_change_count <- f_cov_change_count %>%
  mutate(count_change = count_cur - count_lw)



# Export ------------------------------------------------------------------

# Write to Excel
write_xlsx(
  list(
    "0_base_data" = a_data,
    "1_absorption_month" = d_absorption,
    "1_adm_dvr_long" = b_vxrate_amc,
    "1_adm_all_long" = b_vxrate_pub,
    "1_delivery_doses" = c_delivery_doses_product,
    "2_base_data_amc" = a_data_amc,
    "2_base_data_hic" = a_data_hic,
    "2_dvr_perchange_count" = f_dvr_change_count,
    "2_cov_change_count" = f_cov_change_count,
    "8_dvr_cat" = e_vrcat_all,
    "8_dvr_lm_trend" = e_trend_all,
    "8_tarpast_cat" = e_tar_past_all,
    "8_curtar_cat" = e_tar_cur_all,
    "8_curtar_scale_cat" = e_tar_cur_scale_all,
    "8_booster_status" = e_booster_all,
    "8_secdelpu_cat" = e_secdelpu_all,
    "8_cov_cat" = e_cov_all,
    "9_values" = z_values
  ),
  "output/output_master.xlsx"
)
