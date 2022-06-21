#!/usr/bin/env python
# coding: utf-8

# ### Country Vx Throughput Analysis
#  
# **Note:**
# 
# * Inputs:
#   - covax_supply_chain_analytics.analysis_vx_throughput_data
#   - covax_supply_chain_analytics.covax_sca_country_dimension
#   - covax_supply_chain_analytics.country_dimension.iso_mapping
#   - covax_supply_chain_analytics.vx_received_supply
#   - covax_supply_chain_analytics.analysis_vx_throughput_supply
#   - covax_supply_chain_analytics.owid_covid_data
#   - covax_supply_chain_analytics.population_coverage_measures
#   - covax_supply_chain_analytics.country_characteristics
# * Output:
#   - covax_supply_chain_analytics.analysis_vx_throughput_output_daily
# 

# Impot required libraries
from datetime import date
import pandas as pd
import requests
import io

#  Define any functions
# function takes the following parameters and will peform a pandas ffill() on a Spark DataFrame

# def fill_forward(df, id_column, key_column, fill_column, new_column):
#     # Fill null's with last *non null* value in the window
#     ff = df.withColumn(
#         new_column,
#         last(fill_column, True) # True: fill with last non-null
#         .over(
#             Window.partitionBy(id_column)
#             .orderBy(key_column)
#             .rowsBetween(-sys.maxsize, 0))
#         )
    
#     return(ff)


  
# Get Data
print(" > Getting ISO mapping...")
iso_mapping = pd.read_csv("data/_input/supply_data/iso_mapping.csv")
print(" > Done.")

## get uti_supply
print(" > Getting uti_supply data...")
uti_supply = pd.read_csv("data/_input/supply_data/analysis_vx_throughput_supply.csv")
print(" > Done.")

## get dose administration data for comparison
# print(" > Getting dose administration data for comparison...")
# owid = pd.read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv')
# print(" > Done.")

# # get primary data
# print(" > Getting throughput cleaned data...")
# who = pd.read_csv('data/_input/supply_data/analysis_vx_throughput_data_cleaned.csv')
# print(" > Done.")

# # get country characteristics
# print(" > Getting country characteristics...")
# cc = pd.read_excel("data/_input/supply_data/country_characteristics.xlsx")
# print(" > Done.")

# # Transformation
# print(" > Owid transformation...")
# owid1 = owid[['iso_code', 'date', 'total_vaccinations']]
# owid1.columns = ['iso_code', 'date', 'total_vaccinations_owid']
# owid1 = pd.DataFrame(owid1)
# print(" > Done.")

# # supply side
# # aggregate to received date
# # generate cumulative supply
# received_supply1 = received_supply \
#     .filter(col('iso_code').isNotNull())   
#     .withColumn('received_date', to_date(col('date')))   
#     .groupBy('iso_code', 'received_date').agg(sum('doses_delivered').alias('doses_received'))   
#     .withColumn('cumulative_doses_received', sum('doses_received').over(Window.partitionBy('iso_code').orderBy('received_date').rowsBetween(-sys.maxsize, 0)))   
#     .withColumn('max_cumulative_doses_received', max('cumulative_doses_received').over(Window.partitionBy('iso_code', 'received_date')))   .filter((col('max_cumulative_doses_received')==col('cumulative_doses_received')) | col('max_cumulative_doses_received').isNull())   
#     .drop('max_cumulative_doses_received')   
#     .filter(col('doses_received')!=0)   
#     .toDF('iso_code', 'date', 'doses_received', 'cumulative_doses_received')

# display(received_supply1.orderBy('iso_code', 'received_date'))
# display(received_supply1.filter(col('iso_code')==iso_code))

# alternate supply, sourced by Marta
print(" uti alternate supply (supply1)...")
uti_supply1 = uti_supply[['iso_code', 'date', 'cumulative_doses_received_uti', 'cumulative_doses_received_uti']]
# uti_supply1[['iso_code', 'date', 'cumulative_doses_received_uti', 'cumulative_doses_received_uti']].fillna( method ='ffill', inplace = True)
print(" > changing cumulative_doses_received_uti data type...")
uti_supply1['cumulative_doses_received_uti'] = uti_supply1['cumulative_doses_received_uti'].astype(float)
print(" > changing date t date time...")
uti_supply1['date'] = pd.to_datetime(uti_supply1['date'])
print(" > filling all na's with 0...")
uti_supply1.fillna(0)
print(" > lag intro...") #TODO Check on this lag method. Errors generated
uti_supply1['doses_received'] = uti_supply1.sort_values(by=['date'], ascending=True).groupby(['iso_code'])['cumulative_doses_received_uti'].shift(1)
print(" > Calculating doses received column...")
uti_supply1['doses_received'] = uti_supply1['cumulative_doses_received_uti'] - uti_supply1['doses_received']
print(" > filling na's with cumulative_doses_received_uti...")
uti_supply1['doses_received'].fillna(uti_supply1['cumulative_doses_received_uti'], uti_supply1['doses_received'])
print(" > creating uti-supply1 df...")
uti_supply1 = pd.DataFrame(uti_supply1)
print(uti_supply1)
print(" > Done.")

# uti_supply1 = uti_supply1 \
#     .withColumn('cumulative_doses_received_uti', col('cumulative_doses_received_uti').cast(DoubleType()))   
#     .withColumn('date', to_date(col('date')))   
#     .fillna(0)   
#     .withColumn('doses_received', lag(col('cumulative_doses_received_uti')).over(Window.partitionBy('iso_code').orderBy('date')))   
#     .withColumn('doses_received', col('cumulative_doses_received_uti') - col('doses_received'))   
#     .withColumn('doses_received', when(col('doses_received').isNull(), col('cumulative_doses_received_uti')).otherwise(col('doses_received')))   
#     .select('iso_code', 'date', 'doses_received', 'cumulative_doses_received_uti')   .toDF('iso_code', 'date', 'doses_received', 'cumulative_doses_received') 
# display(uti_supply1)

# # define supply threshold
# supply_threshold = 0.0

# df_flags = who.select('iso_code', 'date', 'is_latest_week_reported', 'manual_adjustment', 'is_data_error', 'to_remove')

# display(df_flags)


# # In[ ]:


# # filter out records we want to drop
# # generate prev total_doses value
# # generate days since prev reported date, if it's the first value > 0, then calc use days since vx intro to smooth things out
# # generate daily_rate_per_week, our key measurement
# w = Window.partitionBy('iso_code').orderBy('date')
# df1 = who   .filter(col('to_remove')==0)   .filter(col('to_remove_1st')==0)   .filter(col('to_remove_2nd')==0) 
# df1 = df1   .join(country, 'iso_code', how='left')   .filter(col('country_name_friendly').isNotNull())   .withColumn('min_date', min('date').over(Window.partitionBy('iso_code')))   .withColumn('min_vx_rollout_date', when(col('min_vx_rollout_date')>=col('min_date'), col('min_date')-1).otherwise(col('min_vx_rollout_date')))   .withColumn('days_since_vx_intro', datediff(col('date'), col('min_vx_rollout_date')))   .withColumn('date_prev', lag(col('date')).over(w))   .withColumn('days_since_vx_intro', datediff(col('date'), col('min_vx_rollout_date')))   .withColumn('days_since_prev', when(col('date_prev').isNull(), col('days_since_vx_intro')).otherwise(datediff(col('date'), col('date_prev'))))

# # create date range from vx rollout date to max report date
# df_daterange = df1   .withColumn('date_max', max('date').over(Window.partitionBy('iso_code')))   .withColumn('min_vx_rollout_date', to_date(col('min_vx_rollout_date')))   .select('iso_code', 'min_vx_rollout_date', 'date_max')   .drop_duplicates()   .withColumn('date', explode(expr('sequence(min_vx_rollout_date, date_max, interval 1 day)')))   .select('iso_code', 'date') 
# # subset to input measures
# df2 = df1   .select('iso_code', 'date', 'min_vx_rollout_date', 'total_doses', 'at_least_one_dose', 'fully_vaccinated', 'persons_booster_add_dose') 
# # join and set vx rollout date to 0 as starting point (if currently null)
# df_inter = df_daterange   .join(df2, ['iso_code', 'date'], how='left')   .withColumn('date_start', min('date').over(Window.partitionBy('iso_code')))   .withColumn('total_doses', when((col('date')==col('date_start')) & (col('total_doses').isNull()), 0).otherwise(col('total_doses')))   .withColumn('at_least_one_dose', when((col('date')==col('date_start')) & (col('at_least_one_dose').isNull()), 0).otherwise(col('at_least_one_dose')))   .withColumn('fully_vaccinated', when((col('date')==col('date_start')) & (col('fully_vaccinated').isNull()), 0).otherwise(col('fully_vaccinated')))   .withColumn('persons_booster_add_dose', when((col('date')==col('date_start')) & (col('persons_booster_add_dose').isNull()), 0).otherwise(col('persons_booster_add_dose'))) 
# # create placeholder columns of correct data type
# df_inter = df_inter   .withColumn('total_doses_int', col('total_doses').cast(IntegerType()))   .withColumn('at_least_one_dose_int', col('at_least_one_dose').cast(IntegerType()))   .withColumn('fully_vaccinated_int', col('fully_vaccinated').cast(IntegerType()))   .withColumn('persons_booster_add_dose_int', col('persons_booster_add_dose').cast(IntegerType())) 
# # pandas_udf requires a schema to be defined
# schema = df_inter.schema

# # interpolation function
# @pandas_udf(schema, functionType=PandasUDFType.GROUPED_MAP)
# def interpolate_measures(df):
#     df.sort_values(by=['iso_code', 'date'], ascending=True, inplace=True)
#     df['total_doses_int'] = df['total_doses_int'].interpolate(method='linear', limit_direction='forward')
#     df['at_least_one_dose_int'] = df['at_least_one_dose_int'].interpolate(method='linear', limit_direction='forward')
#     df['fully_vaccinated_int'] = df['fully_vaccinated_int'].interpolate(method='linear', limit_direction='forward')
#     df['persons_booster_add_dose_int'] = df['persons_booster_add_dose_int'].interpolate(method='linear', limit_direction='forward')
#     return df

# df_inter = df_inter.groupBy('iso_code').apply(interpolate_measures)

# df3 = df_inter   .join(country.select('iso_code', 'country_name_friendly'), 'iso_code', how='left')   .withColumn('is_original_reported', when(col('min_vx_rollout_date').isNotNull(), 1).otherwise(0))   .drop('total_doses', 'at_least_one_dose', 'fully_vaccinated', 'persons_booster_add_dose', 'date_start')   .select('iso_code', 'date', 'country_name_friendly', 'min_vx_rollout_date', 'total_doses_int', 'at_least_one_dose_int', 'fully_vaccinated_int', 'persons_booster_add_dose_int', 'is_original_reported')   .toDF('iso_code', 'date', 'country_name_friendly', 'min_vx_rollout_date', 'total_doses', 'at_least_one_dose', 'fully_vaccinated', 'persons_booster_add_dose', 'is_original_reported')   .withColumn('min_vx_rollout_date', min('min_vx_rollout_date').over(Window.partitionBy('iso_code')))

# # join demand and supply, full join, fill forward, and then filter to "left join"
# # take out 10% for wastage assumption

# df4 = df3   .join(received_supply1, ['iso_code', 'date'], how='full')
# df4 = fill_forward(df4, 'iso_code', 'date', 'cumulative_doses_received', 'cumulative_doses_received')
# df4 = df4.withColumn('cumulative_doses_received', col('cumulative_doses_received')*.9)
# df4 = df4.filter(col('country_name_friendly').isNotNull()) 
# # join supply with demand
# df4 = df4   .withColumn('cumulative_doses_received', when(col('cumulative_doses_received') < col('total_doses'), col('total_doses')).otherwise(col('cumulative_doses_received')))   .withColumn('total_doses_prev_week', lag(col('total_doses')).over(w))   .withColumn('effective_supply', col('cumulative_doses_received') - col('total_doses_prev_week'))   .withColumn('cumulative_supply_20', col('cumulative_doses_received')*supply_threshold)   .withColumn('supply_constrained', when(col('effective_supply') < col('cumulative_supply_20'), 1)               .when(col('effective_supply') >= col('cumulative_supply_20'), 0))   .drop('total_doses_prev_week')

# display(df4)


# # key
# # _td = total_doses
# # _1d = at_least_one_dose
# # _fv = fully_vaccinated

# # define n days
# days_in_weeks4 = 27
# days_in_weeks8 = 55

# # window partitions
# w_rolling_avg_4 = Window.partitionBy('iso_code').orderBy('date').rowsBetween(-days_in_weeks4, 0)
# w_rolling_avg_8 = Window.partitionBy('iso_code').orderBy('date').rowsBetween(-days_in_weeks8, 0)
# w_rolling_avg_4_lastweek = Window.partitionBy('iso_code').orderBy('date').rowsBetween(-34, -7)
# w_rolling_avg_4_lastmonth = Window.partitionBy('iso_code').orderBy('date').rowsBetween(-55, -28)


# # create measure set for total_doses
# var1 = 'total_doses'
# var2 = 'daily_rate_td'
# var3 = 'rolling_4_week_avg_td'
# var4 = 'rolling_8_week_avg_td'
# var5 = 'rolling_4_week_avg_td_lastweek'
# var6 = 'rolling_4_week_avg_td_lastmonth'
# var7 = 'max_rolling_4_week_avg_td'
# var8 = 'med_rolling_4_week_avg_td'

# # calculate
# df5 = df4   .withColumn('prev_total', lag(col(var1)).over(Window.partitionBy('iso_code').orderBy('date')))   .withColumn(var2, when((col(var1) - col('prev_total')).isNull(), col(var1)).otherwise((col(var1) - col('prev_total'))))   .withColumn(var3, avg(var2).over(w_rolling_avg_4))   .withColumn(var4, avg(var2).over(w_rolling_avg_8))   .withColumn(var5, avg(var2).over(w_rolling_avg_4_lastweek))   .withColumn(var6, avg(var2).over(w_rolling_avg_4_lastmonth))   .withColumn(var7, max(var3).over(Window.partitionBy('iso_code')))   .withColumn(var8, expr('percentile_approx({0}, .5)'.format(var3)).over(Window.partitionBy('iso_code')))   .drop('prev_total')


# # create measure set for at_least_one_dose
# var1 = 'at_least_one_dose'
# var2 = 'daily_rate_1d'
# var3 = 'rolling_4_week_avg_1d'
# var4 = 'rolling_8_week_avg_1d'
# var5 = 'rolling_4_week_avg_1d_lastweek'
# var6 = 'rolling_4_week_avg_1d_lastmonth'
# var7 = 'max_rolling_4_week_avg_1d'
# var8 = 'med_rolling_4_week_avg_1d'

# # calculate
# df6 = df5   .withColumn('prev_total', lag(col(var1)).over(Window.partitionBy('iso_code').orderBy('date')))   .withColumn(var2, when((col(var1) - col('prev_total')).isNull(), col(var1)).otherwise((col(var1) - col('prev_total'))))   .withColumn(var3, avg(var2).over(w_rolling_avg_4))   .withColumn(var4, avg(var2).over(w_rolling_avg_8))   .withColumn(var5, avg(var2).over(w_rolling_avg_4_lastweek))   .withColumn(var6, avg(var2).over(w_rolling_avg_4_lastmonth))   .withColumn(var7, max(var3).over(Window.partitionBy('iso_code')))   .withColumn(var8, expr('percentile_approx({0}, .5)'.format(var3)).over(Window.partitionBy('iso_code')))   .drop('prev_total')


# # create measure set for fully_vaccinated
# var1 = 'fully_vaccinated'
# var2 = 'daily_rate_fv'
# var3 = 'rolling_4_week_avg_fv'
# var4 = 'rolling_8_week_avg_fv'
# var5 = 'rolling_4_week_avg_fv_lastweek'
# var6 = 'rolling_4_week_avg_fv_lastmonth'
# var7 = 'max_rolling_4_week_avg_fv'
# var8 = 'med_rolling_4_week_avg_fv'

# # calculate
# df7 = df6   .withColumn('prev_total', lag(col(var1)).over(Window.partitionBy('iso_code').orderBy('date')))   .withColumn(var2, when((col(var1) - col('prev_total')).isNull(), col(var1)).otherwise((col(var1) - col('prev_total'))))   .withColumn(var3, avg(var2).over(w_rolling_avg_4))   .withColumn(var4, avg(var2).over(w_rolling_avg_8))   .withColumn(var5, avg(var2).over(w_rolling_avg_4_lastweek))   .withColumn(var6, avg(var2).over(w_rolling_avg_4_lastmonth))   .withColumn(var7, max(var3).over(Window.partitionBy('iso_code')))   .withColumn(var8, expr('percentile_approx({0}, .5)'.format(var3)).over(Window.partitionBy('iso_code')))   .drop('prev_total')


# df8 = df7

# display(df8.orderBy('iso_code', 'date'))

# # plot the interpolated series
# print(iso_code)
# display(df8.filter(col('iso_code')==iso_code).orderBy('date'))


# df9 = df8   .join(cc, 'iso_code', how='inner')   .join(owid1, ['iso_code', 'date'],  how='left')   .withColumn('rolling_4_week_avg_td_per100', (col('rolling_4_week_avg_td')/col('population'))*100)   .withColumn('rolling_8_week_avg_td_per100', (col('rolling_8_week_avg_td')/col('population'))*100)   .withColumn('max_rolling_4_week_avg_td_per100', (col('max_rolling_4_week_avg_td')/col('population'))*100) 
# df_date_week = df9   .filter(col('is_original_reported')==1)   .select('iso_code', 'date', 'total_doses')   .withColumn('date_week', to_date(next_day(date_add(col('date'), -1), 'Fri')))   .withColumn('week_max', max(col('total_doses')).over(Window.partitionBy('iso_code', 'date_week')))   .filter(col('week_max')==col('total_doses'))   .withColumn('date_max', max(col('date')).over(Window.partitionBy('iso_code', 'date_week')))   .filter(col('date_max')==col('date'))   .withColumn('max_date_week', max(col('date_week')).over(Window.partitionBy('iso_code')))   .withColumn('is_latest', when(col('max_date_week')==col('date_week'), 1).otherwise(0))   .withColumn('week_num', row_number().over(Window.partitionBy('iso_code').orderBy('date_week')))   .select('iso_code', 'date', 'date_week', 'max_date_week', 'is_latest', 'week_num')

# display(df_date_week.groupBy('iso_code', 'date_week').agg(count('*')).filter(col('count(1)')>1))

# df10 = df9   .join(df_date_week, ['iso_code', 'date'], how='left')   .join(df_flags.select('iso_code', 'is_latest_week_reported').drop_duplicates(), 'iso_code', how='left') 
# df11 = df10   .withColumn('prev_week_val', lag(col('total_doses')).over(Window.partitionBy('iso_code').orderBy('date')))   .withColumn('no_change_from_previous', when(col('total_doses')==col('prev_week_val'), 1).otherwise(0))   .drop('prev_week_val')

# display(df11.orderBy('iso_code', 'date'))


# df12 = df11   .select('iso_code', 'entity_name', 'population', 'date', 'is_original_reported', 
#           'cumulative_doses_received', 'effective_supply',
#           'total_doses_owid', 'total_doses', 'at_least_one_dose', 'fully_vaccinated', 'persons_booster_add_dose',
#           'daily_rate_td', 'rolling_4_week_avg_td', 'max_rolling_4_week_avg_td', 'med_rolling_4_week_avg_td', 
#           'rolling_4_week_avg_td_lastweek', 'rolling_4_week_avg_td_lastmonth', 'rolling_8_week_avg_td', 
#           'rolling_4_week_avg_td_per100', 'rolling_8_week_avg_td_per100', 'max_rolling_4_week_avg_td_per100',
#           'daily_rate_1d', 'rolling_4_week_avg_1d', 'daily_rate_fv', 'rolling_4_week_avg_fv', 
#           'is_latest', 'is_latest_week_reported', 'no_change_from_previous')

# df12 = df12.join(who.select('iso_code', 'date_accessed').drop_duplicates(), 'iso_code', how='left')

# display(df12.orderBy('iso_code', 'date').filter(col('iso_code')==iso_code))
# display(df12.filter(col('is_latest')==1))
# display(df12.filter(col('is_latest_weeK_reported')==1))


# # Save to Azure Storage / Register in Databricks metastore


# delta_path = transformed_storage_path + '.delta'

# # dbutils.fs.rm(delta_path, True)

# df12.write.format("delta").mode("overwrite").save(delta_path)


# # In[ ]:


# # path for delta
# print(transformed_storage_path + '.delta')


# # In[ ]:


# get_ipython().run_line_magic('sql', '')

# DROP TABLE IF EXISTS covax_supply_chain_analytics.analysis_vx_throughput_output_daily;

# CREATE TABLE covax_supply_chain_analytics.analysis_vx_throughput_output_daily
# USING DELTA
# LOCATION '/mnt/covax-supply-chain-analytics/transformed/who/analysis_vx_throughput_output_daily.delta'


# # In[ ]:


# display(spark.sql("SELECT * FROM covax_supply_chain_analytics.analysis_vx_throughput_output_daily"))


# # In[ ]:


# display(spark.sql("SELECT * FROM covax_supply_chain_analytics.analysis_vx_throughput_output_daily").orderBy(['iso_code', 'date']).filter(col('iso_code')==iso_code))


# # In[ ]:


# display(
#   spark.sql("SELECT * FROM covax_supply_chain_analytics.analysis_vx_throughput_output_daily")
#     .select('iso_code', 'date', 'max_rolling_4_week_avg_td', 'rolling_4_week_avg_td') \
#     .orderBy(['iso_code', 'date'])
#     .filter(col('iso_code')==iso_code)
# )


# # ##### Query Delta Log

# # In[ ]:


# display(
#   spark.sql("DESCRIBE HISTORY delta. `/mnt/covax-supply-chain-analytics/transformed/who/analysis_vx_throughput_output_daily.delta`")
# )


# # ### Appendix

# # In[ ]:


# display(
#   spark.sql("SELECT * FROM covax_supply_chain_analytics.analysis_vx_throughput_output_daily")
#     .filter(col('is_latest')==1)
# )

