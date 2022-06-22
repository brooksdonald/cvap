from datetime import date
import pandas as pd
import requests
import io

  
# Get Data
print(" > Getting ISO mapping...")
iso_mapping = pd.read_csv("data/_input/supply_data/iso_mapping.csv")
print(" > Done.")

## get uti_supply
print(" > Getting uti_supply data...")
uti_supply = pd.read_csv("data/_input/supply_data/analysis_vx_throughput_supply.csv")
print(" > Done.")

# get dose administration data for comparison
print(" > Getting dose administration data for comparison...")
owid = pd.read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv')
print(" > Done.")

# get primary data
print(" > Getting throughput cleaned data...")
who = pd.read_csv('data/_input/supply_data/analysis_vx_throughput_data_cleaned.csv')
print(" > Done.")

# get country characteristics
print(" > Getting country characteristics...")
# cc = pd.read_excel("data/_input/supply_data/country_characteristics.xlsx")
cc = pd.read_csv("data/_input/supply_data/country_characteristics.csv")
print(" > Done.")

print(" > Getting country dimensions...")
country_dimension = pd.read_csv("data/_input/supply_data/country_dimension.csv")
country = country_dimension[['iso_code', 'country_name_friendly', 'sub_region_name', 'region_name', 'wb_income_group', 'is_amc92', 'affiliation', 'min_vx_rollout_date', 'first_covax_arrival_date', 'first_vx_shipment_received_date']]
country = country.loc[country['is_amc92'] == 1, :]

# Transformation
print(" > Owid transformation...")
owid1 = owid[['iso_code', 'date', 'total_vaccinations']]
owid1.columns = ['iso_code', 'date', 'total_vaccinations_owid']
owid1 = pd.DataFrame(owid1)
print(" > Done.")

# # supply side

# alternate supply, sourced by Marta
print(" uti alternate supply (supply1)...")
uti_supply1 = uti_supply[['iso_code', 'date', 'cumulative_doses_received_uti']]
# print(" > Fill forward...")
# uti_supply1[['iso_code', 'date', 'cumulative_doses_received_uti']].fillna( method ='ffill', inplace = True)
print(" > changing cumulative_doses_received_uti data type...")
uti_supply1['cumulative_doses_received_uti'] = uti_supply1['cumulative_doses_received_uti'].astype(float)
print(" > changing date t date time...")
uti_supply1['date'] = pd.to_datetime(uti_supply1['date'])
print(" > filling all na's with 0...")
uti_supply1.fillna(0, inplace = True)
print(" > lag intro...")
uti_supply1['doses_received'] = uti_supply1.sort_values(by=['date'], ascending=True).groupby(['iso_code'])['cumulative_doses_received_uti'].shift(1)
print(" > Calculating doses received column...")
uti_supply1['doses_received'] = uti_supply1['cumulative_doses_received_uti'] - uti_supply1['doses_received']
print(" > filling na's with cumulative_doses_received_uti...")
uti_supply1['doses_received'].fillna(uti_supply1['cumulative_doses_received_uti'], inplace = True)
print(" > creating uti-supply1 df...")
uti_supply1.columns = ['iso_code', 'date', 'doses_received', 'cumulative_doses_received']
print(" > Done.")

# define supply threshold
print(" > Defining supply threshold...")
supply_threshold = 0.0

print(" > Selecting columns from who dataframe...")
df_flags = who[['iso_code', 'date', 'is_latest_week_reported', 'manual_adjustment', 'is_data_error', 'to_remove']]

# filter out records we want to drop
# generate prev total_doses value
# generate days since prev reported date, if it's the first value > 0, then calc use days since vx intro to smooth things out
# generate daily_rate_per_week, our key measurement
print(" > Grouping and sortingwho df...")
who['date'] = pd.to_datetime(who['date'], format = '%Y-%m-%d')
df1 = who.loc[((who['to_remove'] == 0) & (who['to_remove_1st'] == 0) & (who['to_remove_1st'] == 0)), :]

df1 = df1.merge(country, on = 'iso_code', how = 'left')
df1 = df1.loc[~(df1['country_name_friendly'].isna()), :]
df1['min_vx_rollout_date'] = pd.to_datetime(df1['min_vx_rollout_date'], format = '%Y-%m-%d')
min_date = df1.groupby('iso_code')['date'].min().reset_index()
min_date.rename(columns = {'date': 'min_date'}, inplace = True)
df1 = df1.merge(min_date, on = 'iso_code', how = 'left')
df1.loc[(df1['min_vx_rollout_date'] >= df1['min_date']), 'min_vx_rollout_date'] = df1['min_date'] - pd.to_timedelta(1, unit='D')

df1['days_since_vx_intro'] = df1['date'] - df1['min_vx_rollout_date']
df1['date_prev'] = df1.sort_values(by = 'date', ascending = True) \
    .groupby(['iso_code'])['date'].shift(1)
df1['days_since_prev'] = df1['date'] - df1['date_prev']
df1['days_since_prev'].fillna(df1['days_since_vx_intro'], inplace = True)

date_max = df1.groupby('iso_code')['date'].max().reset_index()
date_max.rename(columns = {'date': 'date_max'}, inplace = True)
df_daterange = df1.merge(date_max, on = 'iso_code', how = 'left')
df_daterange = df_daterange[['iso_code', 'min_vx_rollout_date', 'date_max']]
df_daterange.drop_duplicates(inplace = True)

df_daterange['date'] = df_daterange.apply(lambda row: pd.date_range(row['min_vx_rollout_date'],
    row['date_max'], freq = 'D'), axis = 1)
df_daterange = df_daterange.explode('date').reset_index()
df_daterange = df_daterange[['iso_code', 'date']]

df2 = df1[['iso_code', 'date', 'min_vx_rollout_date', 'total_doses', 'at_least_one_dose',
    'fully_vaccinated', 'persons_booster_add_dose']].copy()

df_inter = df_daterange.merge(df2, on = ['iso_code', 'date'], how = 'left')
date_start = df_inter.groupby('iso_code')['date'].min().reset_index()
date_start.rename(columns = {'date': 'date_start'}, inplace = True)
df_inter = df_inter.merge(date_start, on = 'iso_code', how = 'left')
df_inter.loc[((df_inter['date'] == df_inter['date_start']) & (df_inter['at_least_one_dose'].isna())), 'at_least_one_dose'] = 0
df_inter.loc[((df_inter['date'] == df_inter['date_start']) & (df_inter['total_doses'].isna())), 'total_doses'] = 0
df_inter.loc[((df_inter['date'] == df_inter['date_start']) & (df_inter['fully_vaccinated'].isna())), 'fully_vaccinated'] = 0
df_inter.loc[((df_inter['date'] == df_inter['date_start']) & (df_inter['persons_booster_add_dose'].isna())), 'persons_booster_add_dose'] = 0

df_inter['total_doses_int'] = df_inter['total_doses']
df_inter['at_least_one_dose_int'] = df_inter['at_least_one_dose']
df_inter['fully_vaccinated_int'] = df_inter['fully_vaccinated']
df_inter['persons_booster_add_dose_int'] = df_inter['persons_booster_add_dose']

def interpolate_measures(df):
    df.sort_values(by=['iso_code', 'date'], ascending=True, inplace=True)
    df['total_doses_int'] = df['total_doses_int'].interpolate(method='linear', limit_direction='forward')
    df['at_least_one_dose_int'] = df['at_least_one_dose_int'].interpolate(method='linear', limit_direction='forward')
    df['fully_vaccinated_int'] = df['fully_vaccinated_int'].interpolate(method='linear', limit_direction='forward')
    df['persons_booster_add_dose_int'] = df['persons_booster_add_dose_int'].interpolate(method='linear', limit_direction='forward')
    return df

df_inter = df_inter.groupby('iso_code').apply(interpolate_measures)
df_inter.index.names = ['index']
df_inter.reset_index().drop(['index'], axis = 1, inplace = True)

df3 = df_inter.merge(country[['iso_code', 'country_name_friendly']], on = 'iso_code', how = 'left')
df3['is_original_reported'] = 0
df3.loc[~(df3['min_vx_rollout_date'].isna()), 'is_original_reported'] = 1
df3 = df3[['iso_code', 'date', 'country_name_friendly', 'min_vx_rollout_date', 'total_doses_int', 'at_least_one_dose_int', 'fully_vaccinated_int', 'persons_booster_add_dose_int', 'is_original_reported']]
df3.columns = ['iso_code', 'date', 'country_name_friendly', 'min_vx_rollout_date', 'total_doses', 'at_least_one_dose', 'fully_vaccinated', 'persons_booster_add_dose', 'is_original_reported']
min_vx_rollout_date = df3.groupby('iso_code')['min_vx_rollout_date'].min().reset_index()
min_vx_rollout_date.rename(columns = {'min_vx_rollout_date': 'min_vx_rollout_date'}, inplace = True)
df3 = df3.merge(min_vx_rollout_date, on = 'iso_code', how = 'left')


df4 = df3.merge(uti_supply1, on = ['iso_code', 'date'], how = 'outer')
df4.loc[:, ['iso_code', 'date', 'cumulative_doses_received']].ffill(axis = 0, inplace = True)
df4 = df4.loc[~(df4['country_name_friendly'].isna()), :]

df4['cumulative_doses_received'] = df4[['cumulative_doses_received', 'total_doses']].max(axis = 1)
df4['total_doses_prev_week'] = df4.sort_values(by=['date'], ascending=True).groupby(['iso_code'])['total_doses'].shift(1)
df4['effective_supply'] = df4['cumulative_doses_received'] - df4['total_doses_prev_week']
df4['cumulative_supply_20'] = df4['cumulative_doses_received'] * supply_threshold
df4['supply_constrained'] = None
df4.loc[(df4['effective_supply'] < df4['cumulative_supply_20']), 'supply_constrained'] = 1
df4.loc[(df4['effective_supply'] >= df4['cumulative_supply_20']), 'supply_constrained'] = 0
df4.drop('total_doses_prev_week', axis = 1, inplace = True)

