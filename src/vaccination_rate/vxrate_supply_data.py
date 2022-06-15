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

from office365.runtime.auth.authentication_context import AuthenticationContext
from office365.sharepoint.client_context import ClientContext
from office365.sharepoint.files.file import File

from openpyxl import load_workbook
import pandas as pd
import datetime 

# Get Data
print(" > Getting data...")
# uti_file_name = 'data/_input/supply_data/data_export_WIISE_V_COV_UTI_LONG.xlsx'
# sheet_name = 'data_export_WIISE_V_COV_UTI_LON'

# parameters for reading data directly from Excel stored on Teams/SharePoint
Tenant_ID = '296b3838-4bd5-496c-bd4b-f456ea743b74'
Client_ID = 'bdd5dcc6-2d9b-40d8-b419-61633eb0e380'
Client_secret = dbutils.secrets.get("dse-datasci-keyvault", "GDP-Covax-SPSecret")

sp_url = 'https://bmgf.sharepoint.com/sites/COVID-19GDPTeam'
app_settings = {
     'url': sp_url,
     'client_id': Client_ID,
     'client_secret': Client_secret,
}

ctx_auth = AuthenticationContext(url=app_settings['url'])
ctx_auth.acquire_token_for_app(client_id=app_settings['client_id'], client_secret=app_settings['client_secret'])
ctx = ClientContext(app_settings['url'], ctx_auth)

iso_code = dbutils.widgets.get("iso_code")


country_dimension = spark.sql("SELECT * FROM covax_supply_chain_analytics.covax_sca_country_dimension")   .select('iso_code', 'country_name_friendly')

awho = spark.sql("SELECT * FROM covax_supply_chain_analytics.africa_who_vx")

vx_supply = spark.sql("SELECT * FROM covax_supply_chain_analytics.vx_supply")


file_path = '/sites/COVID-19GDPTeam/Shared Documents/Dashboard of Dashboards/04_Analysis/Covax Throughput Analysis/Data Inputs/' + uti_file_name + '.xlsx'
local_save_path = '/dbfs/tmp/' + uti_file_name + '.xlsx'
# sheet_name = 'v_COV_UTI_LONG'

response = File.open_binary(ctx, file_path)
with open(local_save_path, "wb") as local_file:
    local_file.write(response.content)

# read in data, convert to df, validate columns, filter out blank rows, convert to spark df
wb = load_workbook(local_save_path, data_only=True)
ws = wb[sheet_name]
df = pd.DataFrame(ws.values)
df.columns = df.iloc[0]
df = df[1:].astype(str)
df = df[['ISO_3_CODE', 'COUNTRYNAME', 'YEAR', 'MONTH', 'DATA_SOURCE', 'MANUFACTURER_DESCRIPTION', 'TOTAL_DOSES_REC']]
df = df[df['COUNTRYNAME']!='None']

df.columns = ['iso_code', 'country_name', 'year', 'month', 'data_source', 'manufacturer', 'doses_received']

# create date column
df['day'] = '1'
cols = ['year', 'month', 'day']
df['date'] = df[cols].apply(lambda x: '-'.join(x.values.astype(str)), axis='columns')
df['date'] = pd.to_datetime(df['date']).dt.date
df = df.drop(columns=['year', 'month', 'day'])

df['date'] = df['date'].astype(str)
df_uti = spark.createDataFrame(df, schema=schema1)

# logic for aggregating the Utilization data appropriately:
# group by (iso_code + data_source + manufacturer + date) and take max doses received
# group by (iso_code + data_source + date) and take the sum
# group by (iso_code + date) and take the max
df_uti = df_uti  .groupBy('iso_code', 'data_source', 'manufacturer', 'date').agg(max('doses_received')).withColumnRenamed('max(doses_received)', 'doses_received')  .groupBy('iso_code', 'data_source', 'date').agg(sum('doses_received')).withColumnRenamed('sum(doses_received)', 'doses_received')  .groupBy('iso_code', 'date').agg(max('doses_received')).withColumnRenamed('max(doses_received)', 'doses_received')  .withColumn('monthly_doses_recieved_uti', when((col('doses_received') - lag(col('doses_received')).over(Window.partitionBy('iso_code').orderBy('date'))).isNull(), col('doses_received'))              .otherwise(col('doses_received') - lag(col('doses_received')).over(Window.partitionBy('iso_code').orderBy('date'))))  .withColumnRenamed('doses_received', 'cumulative_doses_received_uti')

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
