import datetime
from datetime import date
from urllib.request import Request, urlopen
from urllib.error import HTTPError
import urllib
import json
import pandas as pd
import numpy as np
import os


def create_new_path(folder):
  print(" > Creating new folder to store data: data/" + folder)
  newpath = r'data/' + folder
  if not os.path.exists(newpath):
    os.makedirs(newpath)
    print(" > New folder created.")

# Get Data
def get_data():

  print(" > Defining URLs for API calls...")
  url1 = 'https://frontdoor-l4uikgap6gz3m.azurefd.net/NCOV/VAC_REP_COUNTS'
  url2 = 'https://frontdoor-l4uikgap6gz3m.azurefd.net/NCOV/VAC_REP_COUNTS_EUR'
  print(" > Done.")

  # 2 data sources for Throughput, one for Europe and one for ROW
  # read in separately, clean, and then bind together
  print(" > Read in data sources separately, clean, and then bind together...")
  print(" > Downloading data from azurefd.net API...")
  response = urlopen(url1)
  data_json = json.loads(response.read())
  df1 = pd.DataFrame(data_json["value"])
  df1 = df1[['COUNTRY_FK', 'AS_OF_DATE', 'DOSES_ADMINISTERED', 'PERSONS_VACCINATED_ONE_PLUS_DOSE', 'PERSONS_VACCINATED_FULL', 'PERSONS_BOOSTER_ADD_DOSE', 'SOURCE']]
  df1 = df1[df1['COUNTRY_FK']!='None']
  df1.columns = ['entity', 'date', 'total_doses', 'at_least_one_dose', 'fully_vaccinated', 'persons_booster_add_dose', 'source']
  df1['country_name'] = df1['entity'].str.title()
  df1 = pd.DataFrame(df1).drop_duplicates()
  print(" > Done.")

  try:
    response = urlopen(url2)
    data_json = json.loads(response.read())
  except HTTPError as e:
    data_json = json.loads(e.read())

  df2 = pd.DataFrame(data_json["value"])
  df2 = df2.astype(str)
  df2 = df2[['COUNTRY_FK', 'AS_OF_DATE', 'DOSES_ADMINISTERED', 'PERSONS_VACCINATED_ONE_PLUS_DOSE', 'PERSONS_VACCINATED_FULL', 'PERSONS_BOOSTER_ADD_DOSE', 'SOURCE']]
  df2 = df2[df2['COUNTRY_FK']!='None']
  df2.columns = ['entity', 'date', 'total_doses', 'at_least_one_dose', 'fully_vaccinated', 'persons_booster_add_dose', 'source']
  df2['country_name'] = df2['entity'].str.title()
  df2 = pd.DataFrame(df2).drop_duplicates()

  print(" > Joining 2 df to one...")
  df3 = pd.concat([df1,df2], ignore_index = True)
  print(" > Done.")
  return df3

def fix_dates(df3):
  # Save df3 to variable df_data
  df_data = pd.DataFrame(df3).drop_duplicates()
  # manaully fix some data date issues
  print(" > Manually fixing data date issus...")
  df_data.loc[np.logical_and(df_data.loc[:,'entity'] == 'ANGUILLA' , df_data.loc[:,"date"] == "2001-02-26"), "date"] = "2021-02-26"
  df_data.loc[np.logical_and(df_data.loc[:,'entity'] == 'TRINIDAD AND TOBAGO', df_data.loc[:,"date"] == "2001-02-26"), "date"] = "2021-02-26"
  df_data.loc[np.logical_and(df_data.loc[:,'entity'] == 'PAKISTAN', df_data.loc[:,"date"] == "2010-03-05"), "date"] = "2021-03-05"
  df_data.loc[np.logical_and(df_data.loc[:,'entity'] == 'CANADA', df_data.loc[:,"date"] == "2020-01-29"), "date"] = "2021-01-29"
  df_data.loc[np.logical_and(df_data.loc[:,'entity'] == 'LIBYA', df_data.loc[:,"date"] == "1900-05-18"), "date"] = "2021-05-18"
  df_data.loc[np.logical_and(df_data.loc[:,'entity'] == 'LIBYA', df_data.loc[:,"date"] == "1900-05-20"), "date"] = "2021-05-20"
  df_data.loc[np.logical_and(df_data.loc[:,'entity'] == 'LIBYA', df_data.loc[:,"date"] == "1900-05-25"), "date"] = "2021-05-25"
  df_data.loc[np.logical_and(df_data.loc[:,'entity'] == 'LIBYA', df_data.loc[:,"date"] == "1900-05-26"), "date"] = "2021-05-26"
  df_data.loc[np.logical_and(df_data.loc[:,'entity'] == 'BRUNEI DARUSSALAM', df_data.loc[:,"date"] == "2021-03-20"), "date"] = "2021-04-20"
  df_data.loc[np.logical_and(df_data.loc[:,'entity'] == 'LIBYA', df_data.loc[:,"date"] == "2021-01-08"), "date"] = "2022-01-08"
  df_data.loc[np.logical_and(df_data.loc[:,'entity'] == 'MOROCCO', df_data.loc[:,"date"] == "2021-01-04"), "date"] = "2021-01-04"
  df_data.loc[np.logical_and(df_data.loc[:,'entity'] == 'MOROCCO', df_data.loc[:,"date"] == "2021-01-08"), "date"] = "2021-01-08"
  print(" > Done.")

  # date stamp dataset
  print(" > Getting the stamp dataset...")
  df_data["date_accessed"] = datetime.date.today()
  print(" > Done.")
  return df_data

def export(df_data, folder, name):
  print(" > Saving analysis_vx_throughput_data to csv file...")
  path = "data/" + folder + "/" + name
  df_data.to_csv(path, index = False)
  print(" > Done.")

def main(folder, name):
  create_new_path(folder)
  df3 = get_data()
  df_data = fix_dates(df3)
  export(df_data, folder, name)
  return df_data