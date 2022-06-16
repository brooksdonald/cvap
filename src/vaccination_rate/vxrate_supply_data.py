#!/usr/bin/env python
# coding: utf-8

# ### Country Vx Throughput Analysis - Supply Data
#  
# **Note:** Gets data for the WHO/BMGF/Gavi Vx Throughput task force.
# 
# * Source:
#   - supply data input files are located here: https://teams.microsoft.com/_#/files/Dashboard%20of%20Dashboards?threadId=19%3A73aa2d526027440684f61b9116291b10%40thread.tacv2&ctx=channel&context=Data%2520Inputs&rootfolder=%252Fsites%252FCOVID-19GDPTeam%252FShared%2520Documents%252FDashboard%2520of%2520Dashboards%252F04_Analysis%252FCovax%2520Throughput%2520Analysis%252FData%2520Inputs
#   - Files:
#     - AFR_Revue COVID-19 Immunization DataEntry Tool_11 March 2021 AFR.xlsx
#     - data_export_WIISE_V_COV_PROC_LONG.xlsx
#       - Grain: Country + Data_Source + Month + Manufacturer_Description
#     - data_export_WIISE_V_COV_UTI_LONG.xlsx
#       - Grain: County + Source + Month + Manufacturer_Description
#     
# 
# * Libraries: 
#   - Python
#     - Office365-REST-Python-Client
#     - openpyxl
# 
# * Built by: Jeremy Cooper
# * Current owner: Jeremy Cooper
# * Initial Build Date: 06/16/2021
# * Latest Build Date: 06/16/2021

import pandas as pd
import datetime 

# Get Data
print(" > Getting supply data from WIISEMART...")
wiise_supply_data = pd.read_excel('data/_input/supply_data/data_export_WIISE_V_COV_UTI_LONG.xlsx')
df = pd.DataFrame(wiise_supply_data)
print(" > Done.")

# read in data, convert to df, validate columns, filter out blank rows
print(" > Read in data, convert to df, validate columns, filter out blank rows...")
df = df[1:].astype(str)
df = df[['ISO_3_CODE', 'COUNTRYNAME', 'YEAR', 'MONTH', 'DATA_SOURCE', 'MANUFACTURER_DESCRIPTION', 'TOTAL_DOSES_REC']]
df = df[df['COUNTRYNAME']!='None']
df.columns = ['iso_code', 'country_name', 'year', 'month', 'data_source', 'manufacturer', 'doses_received']
print(" > Done.")

# create date column
print(" > Creating a date column...")
df['day'] = '1'
cols = ['year', 'month', 'day']
df['date'] = df[cols].apply(lambda x: '-'.join(x.values.astype(str)), axis='columns')
df['date'] = pd.to_datetime(df['date']).dt.date
df = df.drop(columns=['year', 'month', 'day'])
print(" > Done.")

print(" > Creating df_uti df...")
df['date'] = df['date'].astype(str)
df_uti = pd.DataFrame(df)
print(" > Done.")

# Aggregating utilization data
# logic for aggregating the Utilization data appropriately:
## group by (iso_code + data_source + manufacturer + date) and take max doses received
## group by (iso_code + data_source + date) and take the sum
## group by (iso_code + date) and take the max
# print(" > Aggregating utilization data...")
df_uti = df_uti \
.groupBy('iso_code', 'data_source', 'manufacturer', 'date').agg(max('doses_received')).withColumnRenamed('max(doses_received)', 'doses_received') \
.groupBy('iso_code', 'data_source', 'date').agg(sum('doses_received')).withColumnRenamed('sum(doses_received)', 'doses_received') \
.groupBy('iso_code', 'date').agg(max('doses_received')).withColumnRenamed('max(doses_received)', 'doses_received') \
.withColumn('monthly_doses_recieved_uti', when((col('doses_received') - lag(col('doses_received')).over(Window.partitionBy('iso_code').orderBy('date'))).isNull(), col('doses_received')) \            
    .otherwise(col('doses_received') - lag(col('doses_received')).over(Window.partitionBy('iso_code').orderBy('date')))) \
.withColumnRenamed('doses_received', 'cumulative_doses_received_uti')

# datestamp dataframe
df_uti = df_uti.withColumn("date_accessed", current_date())

display(df[df['iso_code']==iso_code])
display(df_uti.orderBy('iso_code', 'date').filter(col('iso_code')==iso_code))
display(df_uti.filter(col('monthly_doses_recieved_uti')<0))


# ### Save to Azure Storage / Register in Databricks metastore

# In[ ]:


delta_path = transformed_storage_path + '.delta'

# dbutils.fs.rm(delta_path, True)

df_uti.write.format("delta").mode("overwrite").save(delta_path)


# In[ ]:


# path for delta
print(transformed_storage_path + '.delta')


# In[ ]:


get_ipython().run_line_magic('sql', '')

DROP TABLE IF EXISTS covax_supply_chain_analytics.analysis_vx_throughput_supply;

CREATE TABLE covax_supply_chain_analytics.analysis_vx_throughput_supply
USING DELTA
LOCATION '/mnt/covax-supply-chain-analytics/transformed/who/analysis_vx_throughput_supply.delta'


# In[ ]:


display(spark.sql("SELECT * FROM covax_supply_chain_analytics.analysis_vx_throughput_supply").orderBy('iso_code', 'date'))
display(spark.sql("SELECT * FROM covax_supply_chain_analytics.analysis_vx_throughput_supply").groupby('iso_code').agg(count('*')))


# ##### Query Delta Log

# In[ ]:


display(
  spark.sql("DESCRIBE HISTORY delta. `/mnt/covax-supply-chain-analytics/transformed/who/analysis_vx_throughput_supply.delta`")
)


# ## Appendix
