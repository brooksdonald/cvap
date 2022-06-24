from datetime import date
import datetime
import pandas as pd
import io

def import_data():
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
    owid = pd.read_csv('data/_input/supply_data/owid-covid-data.csv')
    #owid = pd.read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv')
    print(" > Done.")

    # get primary data
    print(" > Getting throughput cleaned data...")
    who = pd.read_csv('data/_input/supply_data/analysis_vx_throughput_data_cleaned.csv')
    print(" > Done.")

    # get country characteristics
    print(" > Getting country characteristics...")
    cc = pd.read_csv("data/_input/supply_data/country_characteristics.csv")
    print(" > Done.")

    print(" > Getting country dimensions...")
    country_dimension = pd.read_csv("data/_input/supply_data/country_dimension.csv")
    country = country_dimension[['iso_code', 'country_name_friendly', 'sub_region_name', 'region_name', 'wb_income_group', 'is_amc92', 'affiliation', 'min_vx_rollout_date', 'first_covax_arrival_date', 'first_vx_shipment_received_date']]

    # Transformation
    print(" > Owid transformation...")
    owid1 = owid[['iso_code', 'date', 'total_vaccinations']]
    owid1.columns = ['iso_code', 'date', 'total_doses_owid']
    owid1 = pd.DataFrame(owid1)
    print(" > Done.")

    # # supply side

    # alternate supply, sourced by Marta
    print(" > uti alternate supply (supply1)...")
    uti_supply1 = uti_supply[['iso_code', 'date', 'cumulative_doses_received_uti']]
    # print(" > Fill forward...")
    # uti_supply1[['iso_code', 'date', 'cumulative_doses_received_uti']].fillna( method ='ffill', inplace = True)
    print(" > changing cumulative_doses_received_uti data type...")
    pd.set_option('mode.chained_assignment', None)
    uti_supply1.loc[:, 'cumulative_doses_received_uti'] = uti_supply1['cumulative_doses_received_uti'].astype(float)
    print(" > changing date t date time...")
    uti_supply1.loc[:, 'date'] = pd.to_datetime(uti_supply1['date'])
    print(" > filling all na's with 0...")
    uti_supply1 = uti_supply1.fillna(0).copy()
    print(" > lag intro...")
    uti_supply1.loc[:, 'doses_received'] = uti_supply1.sort_values(by=['date'], ascending=True) \
        .groupby(['iso_code'])['cumulative_doses_received_uti'].shift(1)
    print(" > Calculating doses received column...")
    uti_supply1.loc[:, 'doses_received'] = uti_supply1.loc[:, 'cumulative_doses_received_uti'] - uti_supply1.loc[:, 'doses_received']
    print(" > filling na's with cumulative_doses_received_uti...")
    uti_supply1.loc[:, 'doses_received'] = uti_supply1.loc[:, 'doses_received'].fillna(uti_supply1.loc[:, 'cumulative_doses_received_uti'])
    print(" > creating uti-supply1 df...")
    uti_supply1.columns = ['iso_code', 'date', 'doses_received', 'cumulative_doses_received']
    print(" > Done.")
    return who, iso_mapping, cc, country, owid1, uti_supply1


def flags(who):
    print(" > Selecting columns from who dataframe...")
    df_flags = who[['iso_code', 'date', 'is_latest_week_reported', 'manual_adjustment', 'is_data_error', 'to_remove']]
    return df_flags


def merge_who_country(who, country):
    print(" > Grouping and sorting who df...")
    who['date'] = pd.to_datetime(who['date'], format = '%Y-%m-%d')
    df1 = who.loc[((who['to_remove'] == 0) & (who['to_remove_1st'] == 0) & (who['to_remove_1st'] == 0)), :]
    df1 = df1.merge(country, on = 'iso_code', how = 'left')
    return df1


def filter_data(df1):
    print(' > Filter data...')
    df1 = df1.loc[~(df1['country_name_friendly'].isna()), :]
    df1['min_vx_rollout_date'] = pd.to_datetime(df1['min_vx_rollout_date'], format = '%Y-%m-%d')
    min_date = df1.groupby('iso_code')['date'].min().reset_index()
    min_date.rename(columns = {'date': 'min_date'}, inplace = True)
    df1 = df1.merge(min_date, on = 'iso_code', how = 'left')
    df1.loc[(df1['min_vx_rollout_date'] >= df1['min_date']), 'min_vx_rollout_date'] = df1['min_date'] - pd.to_timedelta(1, unit='D')
    df1 = df1.loc[~(df1['iso_code'] == 'MTQ'), :]  # this has been manually added. Consider removing
    return df1


def exploding_dates(df1):
    print(' > Exploding dates for continuous dataset...')
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
    return df_inter


def interpolate_data(df_inter):
    print(" > Interpolating missing values...")
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

    def interpolate_measures(df): # consider moving outside of the function
        df.sort_values(by=['iso_code', 'date'], ascending=True, inplace=True)
        df['total_doses_int'] = df['total_doses_int'].interpolate(method='linear', limit_direction='forward')
        df['at_least_one_dose_int'] = df['at_least_one_dose_int'].interpolate(method='linear', limit_direction='forward')
        df['fully_vaccinated_int'] = df['fully_vaccinated_int'].interpolate(method='linear', limit_direction='forward')
        df['persons_booster_add_dose_int'] = df['persons_booster_add_dose_int'].interpolate(method='linear', limit_direction='forward')
        return df

    df_inter = df_inter.groupby('iso_code').apply(interpolate_measures)
    df_inter.index.names = ['index']
    df_inter.reset_index().drop(['index'], axis = 1, inplace = True)
    return df_inter


def minimum_rollout_date(df_inter, country):
    print(' > Calculating minimum rollout date...')
    df3 = df_inter.merge(country[['iso_code', 'country_name_friendly']], on = 'iso_code', how = 'left')
    df3['is_original_reported'] = 0
    df3.loc[~(df3['min_vx_rollout_date'].isna()), 'is_original_reported'] = 1
    df3 = df3[['iso_code', 'date', 'country_name_friendly', 'min_vx_rollout_date', 'total_doses_int', 'at_least_one_dose_int', 'fully_vaccinated_int', 'persons_booster_add_dose_int', 'is_original_reported']]
    df3.columns = ['iso_code', 'date', 'country_name_friendly', 'min_vx_rollout_date', 'total_doses', 'at_least_one_dose', 'fully_vaccinated', 'persons_booster_add_dose', 'is_original_reported']
    min_vx_rollout_date = df3.groupby('iso_code')['min_vx_rollout_date'].min().reset_index()
    min_vx_rollout_date.rename(columns = {'min_vx_rollout_date': 'min_vx_rollout_date'}, inplace = True)
    df3 = df3.merge(min_vx_rollout_date, on = 'iso_code', how = 'left')
    return df3


def merge_with_supply(df3, uti_supply1):
    print(' > Merging data with supply...')
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
    return df4


def moving_averages_td(df4, days_in_weeks4, days_in_weeks8):
    df5 = df4.copy()
    print(" > Calculating daily rate...")
    df5['prev_total'] = df4.sort_values(by=['date'], ascending=True).groupby(['iso_code'])['total_doses'].shift(1)
    df5['daily_rate_td'] = df5['total_doses'] - df5['prev_total']
    df5['daily_rate_td'].fillna(df5['total_doses'], inplace = True)
    df5.drop('prev_total', axis = 1, inplace = True)


    print(' > Calculating moving averages...')
    df5['hours'] = '00.00.00'
    df5['DateTime'] = df5['date'].astype(str).str.cat(df5['hours'], sep = " ") 
    df5.drop("hours", axis = 1, inplace = True)
    df5.DateTime = df5.DateTime.apply(lambda x: datetime.datetime.strptime(x, '%Y-%m-%d %H.%M.%S'))
    df5.index = df5.DateTime
    df5.sort_index(inplace = True)

    print(" > This week's moving avaerages...")
    # df5['rolling_4_week_avg_td'] = df5.sort_values('date').groupby(['iso_code'])['daily_rate_td'].transform(lambda x: x.rolling(days_in_weeks4, 1).mean())#.reset_index()
    rolling_4_week_avg_td = df5.groupby(['iso_code'])['daily_rate_td'].rolling(str(days_in_weeks4 + 1) + 'D').mean().reset_index()
    rolling_4_week_avg_td.rename(columns = {'daily_rate_td': 'rolling_4_week_avg_td'}, inplace = True)

    rolling_8_week_avg_td = df5.groupby(['iso_code'])['daily_rate_td'].rolling(str(days_in_weeks8 + 1) + 'D').mean().reset_index()
    rolling_8_week_avg_td.rename(columns = {'daily_rate_td': 'rolling_8_week_avg_td'}, inplace = True)

    df5.index.names = ['index']
    df5.reset_index(inplace = True)
    df5 = df5.merge(rolling_4_week_avg_td, on = ['iso_code', 'DateTime'], how = 'left')
    df5 = df5.merge(rolling_8_week_avg_td, on = ['iso_code', 'DateTime'], how = 'left')

    if 'rolling_4_week_avg_td_x' in df5.columns:
        df5.rename(columns = {'rolling_4_week_avg_td_x' : 'rolling_4_week_avg_td'}, inplace = True)
    if 'rolling_4_week_avg_td_y' in df5.columns:
        df5.rename(columns = {'rolling_4_week_avg_td_y' : 'rolling_4_week_avg_td'}, inplace = True)

    print(' > last week\'s moving averages...')
    df5['rolling_4_week_avg_td_lastweek'] = df5.sort_values(by=['date'], ascending=True).groupby(['iso_code'])['rolling_4_week_avg_td'].shift(7) ## using the assumption that observations are daily and continuous

    print(' > last month\'s moving averages...')
    df5['rolling_4_week_avg_td_lastmonth'] = df5.sort_values(by=['date'], ascending=True).groupby(['iso_code'])['rolling_4_week_avg_td'].shift(7 * 4)

    df5.drop('DateTime', axis = 1, inplace = True)
    print(' > Maximum and median moving average...')
    df5_max = df5.groupby(['iso_code'])['rolling_4_week_avg_td'].max().reset_index() 
    df5_max.rename(columns = {'rolling_4_week_avg_td' : 'max_rolling_4_week_avg_td'}, inplace = True)
    df5_median = df5.groupby(['iso_code'])['rolling_4_week_avg_td'].median().reset_index() 
    df5_median.rename(columns = {'rolling_4_week_avg_td' : 'med_rolling_4_week_avg_td'}, inplace = True)
    df5 = df5.merge(df5_max, on = 'iso_code', how = 'left')
    df5 = df5.merge(df5_median, on = 'iso_code', how = 'left')
    return df5


def moving_averages_1d(df5, days_in_weeks4, days_in_weeks8):
    df6 = df5.copy()
    print(" > Calculating daily rate...")
    df6['prev_total'] = df5.sort_values(by=['date'], ascending=True).groupby(['iso_code'])['at_least_one_dose'].shift(1)
    df6['daily_rate_1d'] = df6['at_least_one_dose'] - df6['prev_total']
    df6['daily_rate_1d'].fillna(df6['at_least_one_dose'], inplace = True)
    df6.drop('prev_total', axis = 1, inplace = True)

    print(' > Calculating moving averages...')
    df6['hours'] = '00.00.00'
    df6['DateTime'] = df6['date'].astype(str).str.cat(df6['hours'], sep = " ") 
    df6.drop("hours", axis = 1, inplace = True)
    df6.DateTime = df6.DateTime.apply(lambda x: datetime.datetime.strptime(x, '%Y-%m-%d %H.%M.%S'))
    df6.index = df6.DateTime
    df6.sort_index(inplace = True)

    print(' > this week\'s moving averages...')
    rolling_4_week_avg_1d = df6.groupby(['iso_code'])['daily_rate_1d'].rolling(str(days_in_weeks4 + 1) + 'D').mean().reset_index()
    rolling_4_week_avg_1d.rename(columns = {'daily_rate_1d': 'rolling_4_week_avg_1d'}, inplace = True)

    rolling_8_week_avg_1d = df6.groupby(['iso_code'])['daily_rate_1d'].rolling(str(days_in_weeks8 + 1) + 'D').mean().reset_index()
    rolling_8_week_avg_1d.rename(columns = {'daily_rate_1d': 'rolling_8_week_avg_1d'}, inplace = True)

    df6.index.names = ['index1']
    df6.reset_index(inplace = True)
    df6 = df6.merge(rolling_4_week_avg_1d, on = ['iso_code', 'DateTime'], how = 'left')
    df6 = df6.merge(rolling_8_week_avg_1d, on = ['iso_code', 'DateTime'], how = 'left')

    if 'rolling_4_week_avg_1d_x' in df6.columns:
        df6.rename(columns = {'rolling_4_week_avg_1d_x' : 'rolling_4_week_avg_1d'}, inplace = True)
    if 'rolling_4_week_avg_1d_y' in df6.columns:
        df6.rename(columns = {'rolling_4_week_avg_1d_y' : 'rolling_4_week_avg_1d'}, inplace = True)

    print(' > last week\'s moving averages...')
    df6['rolling_4_week_avg_1d_lastweek'] = df6.sort_values(by=['date'], ascending=True).groupby(['iso_code'])['rolling_4_week_avg_1d'].shift(7) ## using the assumption that observations are daily and continuous

    print(' > last month\'s moving averages...')
    df6['rolling_4_week_avg_1d_lastmonth'] = df6.sort_values(by=['date'], ascending=True).groupby(['iso_code'])['rolling_4_week_avg_1d'].shift(7 * 4)

    df6.drop('DateTime', axis = 1, inplace = True)

    print(' > Maximum and median moving average...')
    df6_max = df6.groupby(['iso_code'])['rolling_4_week_avg_1d'].max().reset_index() 
    df6_max.rename(columns = {'rolling_4_week_avg_1d' : 'max_rolling_4_week_avg_1d'}, inplace = True)
    df6 = df6.merge(df6_max, on = 'iso_code', how = 'left')
    df6_median = df6.groupby(['iso_code'])['rolling_4_week_avg_1d'].median().reset_index() 
    df6_median.rename(columns = {'rolling_4_week_avg_1d' : 'med_rolling_4_week_avg_1d'}, inplace = True)
    df6 = df6.merge(df6_median, on = 'iso_code', how = 'left')
    return df6


def moving_averages_fv(df6, days_in_weeks4, days_in_weeks8):
    df7 = df6.copy()
    print(" > Calculating daily rate...")
    df7['prev_total'] = df6.sort_values(by=['date'], ascending=True).groupby(['iso_code'])['fully_vaccinated'].shift(1)
    df7['daily_rate_fv'] = df7['fully_vaccinated'] - df7['prev_total']
    df7['daily_rate_fv'].fillna(df7['fully_vaccinated'], inplace = True)
    df7.drop('prev_total', axis = 1, inplace = True)

    print(' > Calculating moving averages...')
    df7['hours'] = '00.00.00'
    df7['DateTime'] = df7['date'].astype(str).str.cat(df7['hours'], sep = " ") 
    df7.drop("hours", axis = 1, inplace = True)
    df7.DateTime = df7.DateTime.apply(lambda x: datetime.datetime.strptime(x, '%Y-%m-%d %H.%M.%S'))
    df7.index = df7.DateTime
    df7.sort_index(inplace = True)

    print(' > this week\'s moving averages...')
    rolling_4_week_avg_fv = df7.groupby(['iso_code'])['daily_rate_fv'].rolling(str(days_in_weeks4 + 1) + 'D').mean().reset_index()
    rolling_4_week_avg_fv.rename(columns = {'daily_rate_fv': 'rolling_4_week_avg_fv'}, inplace = True)

    rolling_8_week_avg_fv = df7.groupby(['iso_code'])['daily_rate_fv'].rolling(str(days_in_weeks8 + 1) + 'D').mean().reset_index()
    rolling_8_week_avg_fv.rename(columns = {'daily_rate_fv': 'rolling_8_week_avg_fv'}, inplace = True)

    df7.index.names = ['index2']
    df7.reset_index(inplace = True)
    df7 = df7.merge(rolling_4_week_avg_fv, on = ['iso_code', 'DateTime'], how = 'left')
    df7 = df7.merge(rolling_8_week_avg_fv, on = ['iso_code', 'DateTime'], how = 'left')

    if 'rolling_4_week_avg_fv_x' in df7.columns:
        df7.rename(columns = {'rolling_4_week_avg_fv_x' : 'rolling_4_week_avg_fv'}, inplace = True)
    if 'rolling_4_week_avg_fv_y' in df7.columns:
        df7.rename(columns = {'rolling_4_week_avg_fv_y' : 'rolling_4_week_avg_fv'}, inplace = True)


    print(' > last week\'s moving averages...')
    df7['rolling_4_week_avg_fv_lastweek'] = df7.sort_values(by=['date'], ascending=True).groupby(['iso_code'])['rolling_4_week_avg_fv'].shift(7) ## using the assumption that observations are daily and continuous

    print(' > last month\'s moving averages...')
    df7['rolling_4_week_avg_fv_lastmonth'] = df7.sort_values(by=['date'], ascending=True).groupby(['iso_code'])['rolling_4_week_avg_fv'].shift(7 * 4)

    df7.drop('DateTime', axis = 1, inplace = True)

    print(' > Maximum and median moving average...')
    df7_max = df7.groupby(['iso_code'])['rolling_4_week_avg_fv'].max().reset_index() 
    df7_max.rename(columns = {'rolling_4_week_avg_fv' : 'max_rolling_4_week_avg_fv'}, inplace = True)
    df7 = df7.merge(df7_max, on = 'iso_code', how = 'left')
    df7_median = df7.groupby(['iso_code'])['rolling_4_week_avg_fv'].median().reset_index() 
    df7_median.rename(columns = {'rolling_4_week_avg_fv' : 'med_rolling_4_week_avg_fv'}, inplace = True)
    df7 = df7.merge(df7_median, on = 'iso_code', how = 'left')

    df8 = df7.copy()
    return df8


def join_with_cc_and_owid(df8, cc, owid1):
    print(' > merging data with cc and owid1...')
    cc.rename(columns = {'ISO': 'iso_code', 'Population': 'population', 'Entity': 'entity_name'}, inplace = True)
    cc['population'] = cc['population'].str.replace(',', '').astype(float)
    df9 = df8.merge(cc, on = 'iso_code', how = 'inner')
    df9['date'] = df9['date'].astype(str)

    print(" > Merge with owid data and interpolate")
    def interpolate_owid(df):
        df.sort_values(by=['iso_code', 'date'], ascending=True, inplace=True)
        df['total_doses_owid'] = df['total_doses_owid'].interpolate(method='linear', limit_direction='forward')
        return df

    df9 = df9.merge(owid1, on = ['iso_code', 'date'], how = 'left')
    df9 = df9.groupby('iso_code').apply(interpolate_owid)
    df9['rolling_4_week_avg_td_per100'] = 100 * df9['rolling_4_week_avg_td'] / df9['population'] #data from cc is used. Ambigious reference!
    df9['rolling_8_week_avg_td_per100'] = 100 * df9['rolling_8_week_avg_td'] / df9['population'] 
    df9['max_rolling_4_week_avg_td_per100'] = 100 * df9['max_rolling_4_week_avg_td'] / df9['population'] 
    return df9


def identifying_missing_countries(df9):
    print(' > Identifying countries that have not reported last week...')
    df_date_week = df9.loc[(df9['is_original_reported'] == 1), ['iso_code', 'date', 'total_doses']]
    df_date_week['date'] = pd.to_datetime(df_date_week['date']) #, format = '%Y-%m-%d')
    df_date_week['date_week'] = df_date_week['date'] + pd.to_timedelta(-1, unit = 'D') + \
        pd.to_timedelta( (4 - df_date_week['date'].dt.dayofweek) % 7 , unit = 'D')
    df_date_week.drop_duplicates(inplace = True)

    week_max = df_date_week.groupby(['iso_code', 'date_week'])['total_doses'].max().reset_index()
    week_max.rename(columns = {'total_doses': 'week_max'}, inplace = True)
    df_date_week = df_date_week.merge(week_max, on = ['iso_code', 'date_week'], how = 'left')
    df_date_week = df_date_week.loc[(df_date_week['week_max'] == df_date_week['total_doses']), :]

    date_max = df_date_week.groupby(['iso_code', 'date_week'])['date'].max().reset_index()
    date_max.rename(columns = {'date': 'date_max'}, inplace = True)
    df_date_week = df_date_week.merge(date_max, on = ['iso_code', 'date_week'], how = 'left')
    df_date_week = df_date_week.loc[(df_date_week['date_max'] == df_date_week['date']), :]

    max_date_week = df_date_week.groupby(['iso_code'])['date_week'].max().reset_index()
    max_date_week.rename(columns = {'date_week': 'max_date_week'}, inplace = True)
    df_date_week = df_date_week.merge(max_date_week, on = ['iso_code'], how = 'left')

    df_date_week['is_latest'] = 0
    df_date_week.loc[(df_date_week['max_date_week'] == df_date_week['date_week']), 'is_latest'] = 1
    df_date_week['week_num'] = df_date_week.sort_values('date_week').groupby(['iso_code']).cumcount() + 1
    df_date_week = df_date_week[['iso_code', 'date', 'date_week', 'max_date_week', 'is_latest', 'week_num']]

    print(' > adding flag if latest week is reported...')
    df_date_week['date'] = df_date_week['date'].astype(str)
    df10 = df9.merge(df_date_week, on = ['iso_code', 'date'], how = 'left')
    df10 = df10.merge(df_flags[['iso_code', 'is_latest_week_reported']].drop_duplicates(), on = 'iso_code', how = 'left')
    return df10


def adding_flags_for_changes(df10):
    print(' > adding change from previous flag...')
    df11 = df10.copy()
    df11['prev_week_val'] = df11.sort_values(by=['date'], ascending=True).groupby(['iso_code'])['total_doses'].shift(1)
    df11['no_change_from_previous'] = 0
    df11.loc[(df11['total_doses'] == df11['prev_week_val']), 'no_change_from_previous'] = 1
    df11.drop('prev_week_val', axis = 1, inplace = True)
    return df11


def final_variable_selection(df11):
    print(' > Creating final dataframe')
    df12 = df11[['iso_code', 'entity_name', 'population', 'date', 'is_original_reported', 
                'cumulative_doses_received', 'effective_supply',
                'total_doses_owid', 'total_doses', 'at_least_one_dose', 'fully_vaccinated', 'persons_booster_add_dose',
                'daily_rate_td', 'rolling_4_week_avg_td', 'max_rolling_4_week_avg_td', 'med_rolling_4_week_avg_td', 
                'rolling_4_week_avg_td_lastweek', 'rolling_4_week_avg_td_lastmonth', 'rolling_8_week_avg_td', 
                'rolling_4_week_avg_td_per100', 'rolling_8_week_avg_td_per100', 'max_rolling_4_week_avg_td_per100',
                'daily_rate_1d', 'rolling_4_week_avg_1d', 'daily_rate_fv', 'rolling_4_week_avg_fv', 
                'is_latest', 'is_latest_week_reported', 'no_change_from_previous']]

    df12 = df12.merge(who[['iso_code', 'date_accessed']].drop_duplicates(), on = 'iso_code', how = 'left')
    df12.sort_values(by = ['iso_code', 'date'], ascending=True, inplace = True)
    df12.fillna('null', inplace = True) # consider deleting this line for compatibility with R
    return df12


def export_data(df12):
    print(' > Saving /analysis_vx_throughput_output_daily_to_compare_3 to csv file......')
    df12.to_csv('data/_input/supply_data/analysis_vx_throughput_output_daily_to_compare_3.csv', index = False)
    print(' > Done.')


supply_threshold = 0.0
days_in_weeks4 = 27
days_in_weeks8 = 55

who, iso_mapping, cc, country, owid1, uti_supply1 = import_data()
df_flags = flags(who)
df1 = merge_who_country(who, country)
df1 = filter_data(df1)
df_inter = exploding_dates(df1)
df_inter = interpolate_data(df_inter)
df3 = minimum_rollout_date(df_inter, country)
df4 = merge_with_supply(df3, uti_supply1)
df5 = moving_averages_td(df4, days_in_weeks4, days_in_weeks8)
df6 = moving_averages_1d(df5, days_in_weeks4, days_in_weeks8)
df8 = moving_averages_fv(df6, days_in_weeks4, days_in_weeks8)
df9 = join_with_cc_and_owid(df8, cc, owid1)
df10 = identifying_missing_countries(df9)
df11 = adding_flags_for_changes(df10)
df12 = final_variable_selection(df11)
export_data(df12)
