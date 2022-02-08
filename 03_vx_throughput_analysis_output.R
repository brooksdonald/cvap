
##############################################################
# Environment Setup
##############################################################
getwd()
csv_save_path = "/dbfs/mnt/covax-supply-chain-analytics/r_studio/COVID-Vax-Throughput/vx_throughput_analysis_output.csv"

# what's missing?
# the input into this script is the dataset that's already been cleaned (dates and smoothed series by removing erroneous values)
# need to sort out a supply source (can revert back to the Utilization data from Marta)
# need to get a list of country-specific vaccine rollout dates
# the median looks to be calculated SLIGHTLY different, this is common, the difference is negligible

##############################################################
# Load Libraries
##############################################################
# install.packages("roll")

library(SparkR)
library(dplyr)
library(zoo)
library(tidyr)
library(lubridate)
library(roll)

##############################################################
# Get Data
##############################################################
df <- SparkR::sql("SELECT * FROM covax_supply_chain_analytics.analysis_vx_throughput_data_cleaned") %>% as.data.frame()
country <- SparkR::sql("SELECT * FROM covax_supply_chain_analytics.covax_sca_country_dimension") %>% as.data.frame()
received_supply <- SparkR::sql("SELECT * FROM covax_supply_chain_analytics.linksbridge_delivered_supply") %>% as.data.frame()
uti_supply <- SparkR::sql("SELECT * FROM covax_supply_chain_analytics.analysis_vx_throughput_supply") %>% as.data.frame()
owid <- SparkR::sql("SELECT * FROM covax_supply_chain_analytics.owid_covid_data") %>% as.data.frame()
cc <- SparkR::sql("SELECT * FROM covax_supply_chain_analytics.country_characteristics") %>% as.data.frame()

##############################################################
# Preprocessing
##############################################################

# get country specific vaccine rollout date
country1 <- country %>%
  select('iso_code', 'min_vx_rollout_date')

# get total doses from OWID to use as a comparison
owid1 <- owid %>% 
  select('iso_code', 'date', 'total_vaccinations_int')
colnames(owid1) <- c('iso_code', 'date', 'total_doses_owid')

# get supply from Linksbridge
received_supply1 <- received_supply %>%
  filter(!is.na(iso_code)) %>%
  mutate(received_date = as.Date(date)) %>%
  group_by(iso_code, received_date) %>%
  summarize(doses_received=sum(doses_delivered)) %>%
  mutate(cumulative_doses_received = cumsum(doses_received))
colnames(received_supply1) <- c('iso_code', 'date', 'doses_received', 'cumulative_doses_received')

# get Utilization supply
uti_supply1 <- uti_supply %>%
  mutate(cumulative_doses_received_uti = as.numeric(cumulative_doses_received_uti),
         date = as.Date(date)) %>%
  replace(is.na(.), 0) %>%
  select(iso_code, date, monthly_doses_recieved_uti, cumulative_doses_received_uti)
colnames(uti_supply1) <- c('iso_code', 'date', 'doses_received', 'cumulative_doses_received')

# get flag for most recent week reported
df_flags <- df %>% select('iso_code','is_latest_week_reported') %>% distinct()


##############################################################
# Engineer Throughput Dataset
##############################################################

# filter out records previously flagged
# merge with country dimension
# if min_vx_rollout_date is after first reported date, then set min_vx_rollout_date to the day before the first reported date
df1 <- df %>%
  filter(to_remove==0 & to_remove_1st==0 & to_remove_2nd==0) %>%
  merge(country1, by=c('iso_code'), all.x=TRUE) %>%
  group_by(iso_code) %>%
  mutate(min_date = min(date)) %>%
  mutate(min_vx_rollout_date = as.Date(min_vx_rollout_date),
         min_vx_rollout_date = case_when(min_vx_rollout_date >= min_date ~ min_date-1,
                                         is.na(min_vx_rollout_date) ~ min_date-1,
                                         TRUE ~ min_vx_rollout_date),
         days_since_vx_intro = as.numeric(difftime(date, min_vx_rollout_date, units='days'))) %>%
  arrange(iso_code, date)

# generate a new dataframe that runs from min_vx_rollout_date to the max reported date at daily frequency, this is country specific
df_daterange = df1 %>%
  mutate(date_max = max(date)) %>%
  select(iso_code, min_vx_rollout_date, date_max) %>%
  distinct() %>%
  rowwise() %>% 
  do(data.frame(.[1], date = seq(.$min_vx_rollout_date, .$date_max, by = "1 day")))

# join together so dataframe is set for interpolation to daily frequency
df2 <- df1 %>%
  select('iso_code', 'date', 'min_vx_rollout_date', 'total_doses', 'at_least_one_dose', 'fully_vaccinated', 'persons_booster_add_dose')

# do some cleaning
# use na.approx() to interpolate
df_inter <- df_daterange %>%
  merge(df2, by=c('iso_code', 'date'), all.x=TRUE) %>%
  group_by(iso_code) %>%
  mutate(date_start = min(date),
         total_doses = case_when(date==date_start & is.na(total_doses) ~ 0,
                                 TRUE ~ total_doses),
         at_least_one_dose = case_when(date==date_start & is.na(at_least_one_dose) ~ 0,
                                 TRUE ~ at_least_one_dose),
         fully_vaccinated = case_when(date==date_start & is.na(fully_vaccinated) ~ 0,
                                       TRUE ~ fully_vaccinated),
         persons_booster_add_dose = case_when(date==date_start & is.na(persons_booster_add_dose) ~ 0,
                                      TRUE ~ persons_booster_add_dose)) %>%
  mutate(total_doses = na.approx(total_doses),
         at_least_one_dose = na.approx(at_least_one_dose),
         fully_vaccinated = na.approx(fully_vaccinated),
         persons_booster_add_dose = na.approx(persons_booster_add_dose))

# create flag that allows us to quickly reduce dataset to the original frequency
# merge with supply and create effective (available) supply and flag supply constrained situations
df3 <- df_inter %>%
  group_by(iso_code) %>%
  mutate(is_original_reported = case_when(!is.na(min_vx_rollout_date) ~ 1,
                                          TRUE ~ 0),
         min_vx_rollout_date = date_start) %>%
  merge(received_supply1, by=c('iso_code', 'date'), all=TRUE) %>%
  fill(cumulative_doses_received) %>%
  mutate(cumulative_doses_received = cumulative_doses_received*.9,
         cumulative_doses_received = case_when(cumulative_doses_received < total_doses ~ total_doses,
                                               TRUE ~ cumulative_doses_received),
         total_doses_prev = lag(total_doses),
         effective_supply = cumulative_doses_received - total_doses_prev,
         supply_constrained = case_when(effective_supply < 0 ~ 1,
                                        effective_supply >= 0 ~ 0)) %>%
  select(-total_doses_prev)


##############################################################
# Create Calculated Throughput Columns
##############################################################        
            
# this is the key part, generate rolling averages and aggregations of these calculations (max, med)
# decide to use the roll_mean() function from the "roll" package

# define n days
# days_in_weeks4 = 28
# days_in_weeks8 = 56

# create measure set for total_doses
# var1 = 'total_doses'
# var2 = 'daily_rate_td'
# var3 = 'rolling_4_week_avg_td'
# var4 = 'rolling_8_week_avg_td'
# var5 = 'rolling_4_week_avg_td_lastweek'
# var6 = 'rolling_4_week_avg_td_lastmonth'
# var7 = 'max_rolling_4_week_avg_td'
# var8 = 'med_rolling_4_week_avg_td'

df4 <- df3 %>%
  mutate(prev_total = lag(total_doses),
         daily_rate_td = total_doses - prev_total,
         rolling_4_week_avg_td = roll_mean(daily_rate_td, width = 28, min_obs = 1),
         rolling_8_week_avg_td = roll_mean(daily_rate_td, width = 56, min_obs = 1),
         rolling_4_week_avg_td_lastweek = lag(rolling_4_week_avg_td, 7),
         rolling_4_week_avg_td_lastmonth = lag(rolling_4_week_avg_td, 28)) %>%
         group_by(iso_code) %>%
  mutate(max_rolling_4_week_avg_td = max(rolling_4_week_avg_td, na.rm=TRUE),
         med_rolling_4_week_avg_td = median(rolling_4_week_avg_td, na.rm=TRUE)) %>%
  select(-prev_total)


# create measure set for at_least_one_dose
# var1 = 'at_least_one_dose'
# var2 = 'daily_rate_1d'
# var3 = 'rolling_4_week_avg_1d'
# var4 = 'rolling_8_week_avg_1d'
# var5 = 'rolling_4_week_avg_1d_lastweek'
# var6 = 'rolling_4_week_avg_1d_lastmonth'
# var7 = 'max_rolling_4_week_avg_1d'
# var8 = 'med_rolling_4_week_avg_1d'

df5 <- df4 %>%
  mutate(prev_total = lag(at_least_one_dose),
         daily_rate_1d = at_least_one_dose - prev_total,
         rolling_4_week_avg_1d = roll_mean(daily_rate_1d, width = 28, min_obs = 1),
         rolling_8_week_avg_1d = roll_mean(daily_rate_1d, width = 56, min_obs = 1),
         rolling_4_week_avg_1d_lastweek = lag(rolling_4_week_avg_1d, 7),
         rolling_4_week_avg_1d_lastmonth = lag(rolling_4_week_avg_1d, 28)) %>%
  group_by(iso_code) %>%
  mutate(max_rolling_4_week_avg_1d = max(rolling_4_week_avg_1d, na.rm=TRUE),
         med_rolling_4_week_avg_1d = median(rolling_4_week_avg_1d, na.rm=TRUE)) %>%
  select(-prev_total)  


# create measure set for fully_vaccinated
# var1 = 'fully_vaccinated'
# var2 = 'daily_rate_fv'
# var3 = 'rolling_4_week_avg_fv'
# var4 = 'rolling_8_week_avg_fv'
# var5 = 'rolling_4_week_avg_fv_lastweek'
# var6 = 'rolling_4_week_avg_fv_lastmonth'
# var7 = 'max_rolling_4_week_avg_fv'
# var8 = 'med_rolling_4_week_avg_fv'

df6 <- df5 %>%
  mutate(prev_total = lag(fully_vaccinated),
         daily_rate_fv = fully_vaccinated - prev_total,
         rolling_4_week_avg_fv = roll_mean(daily_rate_fv, width = 28, min_obs = 1),
         rolling_8_week_avg_fv = roll_mean(daily_rate_fv, width = 56, min_obs = 1),
         rolling_4_week_avg_fv_lastweek = lag(rolling_4_week_avg_fv, 7),
         rolling_4_week_avg_fv_lastmonth = lag(rolling_4_week_avg_fv, 28)) %>%
  group_by(iso_code) %>%
  mutate(max_rolling_4_week_avg_fv = max(rolling_4_week_avg_fv, na.rm=TRUE),
         med_rolling_4_week_avg_fv = median(rolling_4_week_avg_fv, na.rm=TRUE)) %>%
  select(-prev_total)


##############################################################
# Join Datasets Together and Final Cleaning
##############################################################  

# generate per100 normalized calculations
df7 <- df6 %>%
  merge(cc, by=c('iso_code')) %>%
  merge(owid1, by=c('iso_code', 'date'), all.x=TRUE) %>%
  mutate(population = as.numeric(population),
         rolling_4_week_avg_td_per100 = rolling_4_week_avg_td/population*100,
         rolling_8_week_avg_td_per100 = rolling_8_week_avg_td/population*100,
         max_rolling_4_week_avg_td_per100 = max_rolling_4_week_avg_td/population*100)

# create a date_week column which will allow filtering down to this frequency, little manipulation required
df_dateweek <- df7 %>%
  filter(is_original_reported==1) %>%
  select(iso_code, date, total_doses) %>%
  mutate(date_num = wday(date-1),
         date_week = date + 5 - date_num) %>%
  group_by(iso_code, date_week) %>%
  mutate(week_max = max(total_doses)) %>%
  filter(week_max == total_doses) %>%
  mutate(date_max = max(date)) %>%
  filter(date == date_max) %>%
  group_by(iso_code) %>%
  mutate(max_date_week = max(date_week),
         is_latest = case_when(max_date_week==date_week ~ 1,
                               TRUE ~ 0)) %>%
  select(iso_code, date, date_week, is_latest)

# create no_change_from_previous flag
df8 <- df7 %>%
  merge(df_dateweek, by=c('iso_code', 'date'), all.x=TRUE) %>%
  merge(df_flags, by=c('iso_code'), all.x=TRUE) %>%
  group_by(iso_code) %>%
  mutate(is_latest = case_when(is_latest != 1 ~ 0,
                               TRUE ~ is_latest),
         prev_week_val = lag(total_doses),
         no_change_from_previous = case_when(total_doses == prev_week_val ~ 1,
                                             TRUE ~ 0)) %>%
  select(-prev_week_val)

# final subsetting and reordering
df9 <- df8 %>%
  select('iso_code', 'entity_name', 'population', 'date', 'is_original_reported', 
         'cumulative_doses_received', 'effective_supply',
         'total_doses_owid', 'total_doses', 'at_least_one_dose', 'fully_vaccinated', 'persons_booster_add_dose',
         'daily_rate_td', 'rolling_4_week_avg_td', 'max_rolling_4_week_avg_td', 'med_rolling_4_week_avg_td', 
         'rolling_4_week_avg_td_lastweek', 'rolling_4_week_avg_td_lastmonth', 'rolling_8_week_avg_td', 
         'rolling_4_week_avg_td_per100', 'rolling_8_week_avg_td_per100', 'max_rolling_4_week_avg_td_per100',
         'daily_rate_1d', 'rolling_4_week_avg_1d', 'daily_rate_fv', 'rolling_4_week_avg_fv', 
         'is_latest', 'is_latest_week_reported', 'no_change_from_previous')

# add in the date_accessed field (when we pulled data originally from source)
df9 <- df9 %>%
  merge(df %>% select(iso_code, date_accessed) %>% distinct(), by=c('iso_code'), all.x=TRUE) %>%
  arrange(iso_code, date)


##############################################################
# Save output to CSV
##############################################################

write.csv(df9, csv_save_path, row.names = FALSE, na="")


