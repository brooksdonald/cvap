import pandas as pd

days_in_weeks4 = 27
days_in_weeks8 = 55

df4 = pd.read_csv('data/_input/supply_data/data_export_WIISE_V_COV_UTI_LONG.csv')

w_rolling_avg_4 = Window.partitionBy('iso_code').orderBy('date').rowsBetween(-days_in_weeks4, 0)
w_rolling_avg_8 = Window.partitionBy('iso_code').orderBy('date').rowsBetween(-days_in_weeks8, 0)
w_rolling_avg_4_lastweek = Window.partitionBy('iso_code').orderBy('date').rowsBetween(-34, -7)
w_rolling_avg_4_lastmonth = Window.partitionBy('iso_code').orderBy('date').rowsBetween(-55, -28)


# create measure set for total_doses
var1 = 'total_doses'
var2 = 'daily_rate_td'
var3 = 'rolling_4_week_avg_td'
var4 = 'rolling_8_week_avg_td'
var5 = 'rolling_4_week_avg_td_lastweek'
var6 = 'rolling_4_week_avg_td_lastmonth'
var7 = 'max_rolling_4_week_avg_td'
var8 = 'med_rolling_4_week_avg_td'


df5 = df4.copy()
df5['prev_total'] = df4.sort_values(by=['date'], ascending=True).groupby(['iso_code'])['total_doses'].shift(1)
df5['daily_rate_td'] = df5['total_doses'] - df5['prev_total']
df5['daily_rate_td'].fillna(df5['total_doses'], inplace = True)
pt1 = pd.pivot_table(df5, columns = 'date', values = 'daily_rate_td')
df5['rolling_4_week_avg_td'] = pt1.resample('W').mean().rolling(4).mean()

toll = pd.pivot_table(toll, columns='timeblock',index='date', values='speed')
toll = toll.resample('W').mean().rolling(4).mean()

df_uti['monthly_doses_recieved_uti'] = df_uti['doses_received'] - df_uti['doses_received_lag']
df_uti['monthly_doses_recieved_uti'].fillna(df_uti['doses_received'], inplace = True)