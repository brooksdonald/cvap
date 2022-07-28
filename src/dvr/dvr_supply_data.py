import pandas as pd
import datetime 
from datetime import date
import openpyxl


def import_data(folder, wiise_supply_data):
    # print(" > Getting supply data from WIISEMART for supply_data.py ...")
    # path = "data/" + folder + "/data_export_WIISE_V_COV_UTI_LONG.xlsx"
    # wiise_supply_data = pd.read_excel(path)
    df = pd.DataFrame(wiise_supply_data)
    print(" > Done.")
    return df


def validate_and_filter(df):
    print(" > Read in data, convert to df, validate columns, filter out blank rows...")
    df = df[['ISO_3_CODE', 'COUNTRYNAME', 'YEAR', 'MONTH', 'DATA_SOURCE', 'MANUFACTURER_DESCRIPTION', 'TOTAL_DOSES_REC']]
    df = df[df['COUNTRYNAME']!='None']
    df.columns = ['iso_code', 'country_name', 'year', 'month', 'data_source', 'manufacturer', 'doses_received']
    print(" > Done.")
    return df


def create_date_column(df):
    print(" > Creating a date column...")
    df['day'] = '1'
    cols = ['year', 'month', 'day']
    df['date'] = df[cols].apply(lambda x: '-'.join(x.values.astype(str)), axis='columns')
    df['date'] = pd.to_datetime(df['date']).dt.date
    df = df.drop(columns=['year', 'month', 'day'])
    print(" > Done.")

    print(" > Creating df_uti df...")
    df_uti = pd.DataFrame(df)
    print(" > Done.")
    return df_uti


def aggregate_util_data(df_uti):
    # Aggregating utilization data
    # logic for aggregating the Utilization data appropriately:
    ## group by (iso_code + data_source + manufacturer + date) and take max doses received
    ## group by (iso_code + data_source + date) and take the sum
    ## group by (iso_code + date) and take the max
    print(" > Aggregating utilization data...")
    print(" > Adding monthly and cumulative columns...")
    df_uti = df_uti.groupby(['iso_code', 'data_source', 'manufacturer', 'date'])['doses_received'].agg('max').reset_index()
    df_uti = df_uti.groupby(['iso_code', 'data_source', 'date'])['doses_received'].agg('sum').reset_index()
    df_uti = df_uti.groupby(['iso_code', 'date'])['doses_received'].agg('max').reset_index()
    df_uti['doses_received_lag'] = df_uti.sort_values(by=['date'], ascending=True).groupby(['iso_code'])['doses_received'].shift(1)
    df_uti['monthly_doses_recieved_uti'] = df_uti['doses_received'] - df_uti['doses_received_lag']
    df_uti['monthly_doses_recieved_uti'].fillna(df_uti['doses_received'], inplace = True)
    df_uti.drop(['doses_received_lag'], axis = 1, inplace = True)
    df_uti.rename(columns = {'doses_received' : 'cumulative_doses_received_uti'}, inplace = True)
    print(" > Done.")
    return df_uti


def add_timestamp(df_uti):
    print(" > Timestamp data")
    df_uti['date_accessed'] = str(date.today())
    print(" > Done.")
    return df_uti


def export_data(df_uti, folder, name):
    print(" > Saving " + name + " to csv file...")
    path = "data/" + folder + "/" + name
    df_uti.to_csv(path, index = False)
    print(" > Done.")
    return df_uti


def main(folder, name, wiise_supply_data):
    df = import_data(folder, wiise_supply_data)
    df = validate_and_filter(df)
    df_uti = create_date_column(df)
    df_uti = aggregate_util_data(df_uti)
    df_uti = add_timestamp(df_uti)
    export_data(df_uti, folder, name)
    return df_uti
