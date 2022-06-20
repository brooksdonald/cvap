import pandas as pd
import datetime

days_in_weeks4 = 27
days_in_weeks8 = 55

### this is only an example input. Will be deleted after merging with vx_rate_output_daily.py
df4 = pd.read_csv('data/_input/supply_data/analysis_vx_throughput_supply.csv')  
df4.rename(columns = {'ISO_3_CODE': 'iso_code', 'DATE': 'date', 'cumulative_doses_received_uti': 'total_doses'}, inplace = True)

df5 = df4.copy()

print(" > Calculating daily rate...")
df5['prev_total'] = df4.sort_values(by=['date'], ascending=True).groupby(['iso_code'])['total_doses'].shift(1)
df5['daily_rate_td'] = df5['total_doses'] - df5['prev_total']
df5['daily_rate_td'].fillna(df5['total_doses'], inplace = True)
df5.drop('prev_total', axis = 1, inplace = True)


print(' > Calculating moving averages...')
df5['DateTime'] = (df5['date']) + ' ' + '00.00.00'
df5.DateTime = df5.DateTime.apply(lambda x: datetime.datetime.strptime(x, '%Y-%m-%d %H.%M.%S'))
df5.index = df5.DateTime
df5.sort_index(inplace = True)

print(' > this week\'s moving averages...')
rolling_4_week_avg_td = df5.groupby(['iso_code'])['total_doses'].rolling(str(days_in_weeks4) + 'D').mean().reset_index()
rolling_4_week_avg_td.rename(columns = {'total_doses': 'rolling_4_week_avg_td'}, inplace = True)

rolling_8_week_avg_td = df5.groupby(['iso_code'])['total_doses'].rolling(str(days_in_weeks8) + 'D').mean().reset_index()
rolling_8_week_avg_td.rename(columns = {'total_doses': 'rolling_8_week_avg_td'}, inplace = True)

print(' > last week\'s moving averages...')
df5a = df5.copy()
df5a.Time = pd.to_timedelta(7, unit='D')
df5a.index = df5a.index + df5a.Time
df5c = df5.copy()
df5c['total_doses'] = None
df5b = pd.concat([df5a, df5c], ignore_index = False)
df5b.sort_index(inplace = True)

rolling_4_week_avg_1d_lastweek = df5b.groupby(['iso_code'])['total_doses'].rolling(str(days_in_weeks4) + 'D').mean().reset_index()
rolling_4_week_avg_1d_lastweek.rename(columns = {'total_doses': 'rolling_4_week_avg_1d_lastweek'}, inplace = True)

print(' > last month\'s moving averages...')
df5d = df5.copy()
df5d.Time = pd.to_timedelta(days_in_weeks4 + 1, unit='D')
df5d.index = df5d.index + df5d.Time
df5e = df5.copy()
df5e['total_doses'] = None
df5f = pd.concat([df5d, df5e], ignore_index = False)
df5f.sort_index(inplace = True)

rolling_4_week_avg_td_lastmonth = df5f.groupby(['iso_code'])['total_doses'].rolling(str(days_in_weeks4) + 'D').mean().reset_index()
rolling_4_week_avg_td_lastmonth.rename(columns = {'total_doses': 'rolling_4_week_avg_td_lastmonth'}, inplace = True)

df5.index.names = ['index']
df5.reset_index()
df5 = df5.merge(rolling_4_week_avg_td, on = ['iso_code', 'DateTime'], how = 'left')
df5 = df5.merge(rolling_8_week_avg_td, on = ['iso_code', 'DateTime'], how = 'left')
df5 = df5.merge(rolling_4_week_avg_1d_lastweek, on = ['iso_code', 'DateTime'], how = 'left')
df5 = df5.merge(rolling_4_week_avg_td_lastmonth, on = ['iso_code', 'DateTime'], how = 'left')
df5.drop('DateTime', axis = 1, inplace = True)
if 'rolling_4_week_avg_td_x' in df5.columns:
    df5.rename(columns = {'rolling_4_week_avg_td_x' : 'rolling_4_week_avg_td'}, inplace = True)
if 'rolling_4_week_avg_td_y' in df5.columns:
    df5.rename(columns = {'rolling_4_week_avg_td_y' : 'rolling_4_week_avg_td'}, inplace = True)

print(' > Maximum and median moving average...')
df5_max = df5.groupby(['iso_code'])['rolling_4_week_avg_td'].max().reset_index() 
df5_max.rename(columns = {'rolling_4_week_avg_td' : 'max_rolling_4_week_avg_1d'}, inplace = True)
df5 = df5.merge(df5_max, on = 'iso_code', how = 'left')
df5_median = df5.groupby(['iso_code'])['rolling_4_week_avg_td'].median().reset_index() 
df5_max.rename(columns = {'rolling_4_week_avg_td' : 'med_rolling_4_week_avg_1d'}, inplace = True)
df5 = df5.merge(df5_median, on = 'iso_code', how = 'left')


## TODO: make repitition dry from here

df6 = df5.copy()

print(" > Calculating daily rate...")
df6['prev_total'] = df4.sort_values(by=['date'], ascending=True).groupby(['iso_code'])['total_doses'].shift(1)
df6['daily_rate_1d'] = df6['total_doses'] - df6['prev_total']
df6['daily_rate_1d'].fillna(df6['total_doses'], inplace = True)
df6.drop('prev_total', axis = 1, inplace = True)


print(' > Calculating moving averages...')
df6['DateTime'] = (df6['date']) + ' ' + '00.00.00'
df6.DateTime = df6.DateTime.apply(lambda x: datetime.datetime.strptime(x, '%Y-%m-%d %H.%M.%S'))
df6.index = df6.DateTime
df6.sort_index(inplace = True)

print(' > this week\'s moving averages...')
rolling_4_week_avg_1d = df6.groupby(['iso_code'])['total_doses'].rolling(str(days_in_weeks4) + 'D').mean().reset_index()
rolling_4_week_avg_1d.rename(columns = {'total_doses': 'rolling_4_week_avg_1d'}, inplace = True)

rolling_8_week_avg_1d = df6.groupby(['iso_code'])['total_doses'].rolling(str(days_in_weeks8) + 'D').mean().reset_index()
rolling_8_week_avg_1d.rename(columns = {'total_doses': 'rolling_8_week_avg_1d'}, inplace = True)

print(' > last week\'s moving averages...')
df6a = df6.copy()
df6a.Time = pd.to_timedelta(7, unit='D')
df6a.index = df6a.index + df6a.Time
df6c = df6.copy()
df6c['total_doses'] = None
df6b = pd.concat([df6a, df6c], ignore_index = False)
df6b.sort_index(inplace = True)

rolling_4_week_avg_1d_lastweek = df6b.groupby(['iso_code'])['total_doses'].rolling(str(days_in_weeks4) + 'D').mean().reset_index()
rolling_4_week_avg_1d_lastweek.rename(columns = {'total_doses': 'rolling_4_week_avg_1d_lastweek'}, inplace = True)

print(' > last month\'s moving averages...')
df6d = df6.copy()
df6d.Time = pd.to_timedelta(days_in_weeks4 + 1, unit='D')
df6d.index = df6d.index + df6d.Time
df6e = df6.copy()
df6e['total_doses'] = None
df6f = pd.concat([df6d, df6e], ignore_index = False)
df6f.sort_index(inplace = True)

rolling_4_week_avg_1d_lastmonth = df6f.groupby(['iso_code'])['total_doses'].rolling(str(days_in_weeks4) + 'D').mean().reset_index()
rolling_4_week_avg_1d_lastmonth.rename(columns = {'total_doses': 'rolling_4_week_avg_1d_lastmonth'}, inplace = True)

df6.index.names = ['index']
df6.reset_index()
df6 = df6.merge(rolling_4_week_avg_1d, on = ['iso_code', 'DateTime'], how = 'left')
df6 = df6.merge(rolling_8_week_avg_1d, on = ['iso_code', 'DateTime'], how = 'left')
df6 = df6.merge(rolling_4_week_avg_1d_lastweek, on = ['iso_code', 'DateTime'], how = 'left')
df6 = df6.merge(rolling_4_week_avg_1d_lastmonth, on = ['iso_code', 'DateTime'], how = 'left')
df6.drop('DateTime', axis = 1, inplace = True)
if 'rolling_4_week_avg_1d_x' in df6.columns:
    df6.rename(columns = {'rolling_4_week_avg_1d_x' : 'rolling_4_week_avg_1d'}, inplace = True)
if 'rolling_4_week_avg_1d_y' in df6.columns:
    df6.rename(columns = {'rolling_4_week_avg_1d_y' : 'rolling_4_week_avg_1d'}, inplace = True)

print(' > Maximum and median moving average...')
df6_max = df6.groupby(['iso_code'])['rolling_4_week_avg_1d'].max().reset_index() 
df6_max.rename(columns = {'rolling_4_week_avg_1d' : 'max_rolling_4_week_avg_1d'}, inplace = True)
df6 = df6.merge(df6_max, on = 'iso_code', how = 'left')
df6_median = df6.groupby(['iso_code'])['rolling_4_week_avg_1d'].median().reset_index() 
df6_max.rename(columns = {'rolling_4_week_avg_1d' : 'med_rolling_4_week_avg_1d'}, inplace = True)
df6 = df6.merge(df6_median, on = 'iso_code', how = 'left')
