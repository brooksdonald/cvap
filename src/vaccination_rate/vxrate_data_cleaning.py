import pandas as pd
from datetime import datetime, timedelta


def import_data():
    print(" > Loading dataset...")
    who = pd.read_csv("data/_input/supply_data/analysis_vx_throughput_data.csv")
    iso_mapping = pd.read_csv("data/_input/supply_data/iso_mapping.csv")
    return who, iso_mapping


def convert_data_types(who):
    print(" > Converting data types") ## these rows are technically obsolete because data is read in as float automatically
    who["total_doses"] = who["total_doses"].astype(float)
    who["at_least_one_dose"] = who["at_least_one_dose"].astype(float)
    who["fully_vaccinated"] = who["fully_vaccinated"].astype(float)
    who["persons_booster_add_dose"] = who["persons_booster_add_dose"].astype(float)
    return who


def cleaning(who):
    print(" > Remove NA...")
    who = who[who['total_doses'].notna()]
    who = who[who['total_doses'] > 0]

    print(" > Selecting relevant columns...")
    df1 = who[['country_name', 'date', 'total_doses', 'at_least_one_dose', 'fully_vaccinated', 'persons_booster_add_dose', 'date_accessed']]
    return df1


def date_to_date_week(df1):
    print(" > Converting date to date_week...")
    df1['date'] = pd.to_datetime(df1['date'], format = '%Y-%m-%d')
    df1['date_week'] = df1['date'] + pd.to_timedelta( (4 - df1['date'].dt.dayofweek) % 7 , unit = 'D')

    print(" > Dropping duplicates...")
    df1.drop_duplicates(inplace = True)
    return df1


def map_iso_codes(df1, iso_mapping):
    print(" > Mapping ISO codes...")
    iso_mapping['country_name'] = iso_mapping['country_name'].str.title()
    df1 = df1.merge(iso_mapping, on = 'country_name', how = 'left')
    df1.loc[df1['country_name'] == 'Bonaire, Sint Eustatius And Saba/Saba', 'iso_code'] = 'BES1'
    df1.loc[df1['country_name'] == 'Bonaire, Sint Eustatius And Saba/Sint Eustatius', 'iso_code'] = 'BES1'
    df1.loc[df1['country_name'] == 'Bonaire, Sint Eustatius And Saba', 'iso_code'] = 'BES2'
    df1.loc[df1['country_name'] == 'Bonaire, Sint Eustatius And Saba/Bonaire', 'iso_code'] = 'XAA'
    df1.loc[df1['country_name'] == 'Occupied Palestinian Territory, Including East Jerusalem', 'iso_code'] = 'PSE'
    df1.loc[df1['country_name'] == 'Sint Maarten', 'iso_code'] = 'SXM'
    df1.loc[df1['country_name'] == 'Wallis And Futuna', 'iso_code'] = 'WLF'
    df1.loc[df1['country_name'] == 'Pitcairn Islands', 'iso_code'] = 'PCN'
    df1.loc[df1['country_name'] == 'Northern Mariana Islands (Commonwealth Of The)', 'iso_code'] = 'MNP'
    df1.loc[df1['country_name'] == 'The United Kingdom', 'iso_code'] = 'GBR'
    df1.loc[df1['country_name'] == 'Turkey', 'iso_code'] = 'TUR'
    df1.loc[df1['country_name'] == 'Kosovo', 'iso_code'] = 'KOS'

    print(" > Identifying countries that have not reported for the latest week...")
    max_date_week = df1['date_week'].max()
    df1['max_date_week'] = df1.groupby('iso_code')['date_week'].transform('max')
    df1['is_latest_week_reported'] = 0
    df1.loc[df1['max_date_week'] == max_date_week, 'is_latest_week_reported'] = 1
    df1.drop('max_date_week', axis = 1, inplace = True)
    
    print(" > Remove missing values...")
    df1 = df1.fillna(0)
    df2 = df1[df1['iso_code'] != 'BES1']
    return df1, df2


def fix_issues_total_doses(df2):
    print(" > Fix data issues with total_doses...")
    manual_fix_list =  ['AFG', 'AGO', 'AIA', 'ARE', 'ARM', 'ASM', 'ATG', 'AUS', 'AZE', 
                        'BDI', 'BEN', 'BFA', 'BGR', 'BHR', 'BLR', 'BRA', 'BTN', 'BWA', 
                        'CAN', 'CAF', 'CHN', 'COD', 'COK', 'COL', 'COM', 'CYM', 
                        'DMK', 'DMA', 'DJI',
                        'ESP', 'ETH',
                        'FRA', 'FSM',
                        'GAB', 'GBR', 'GEO', 'GLP', 'GMB', 'GNB', 'GNQ', 'GUM',
                        'HND', 'HUN', 
                        'IRL', 'ISL', 'ISR', 
                        'JOR', 'JPN', 
                        'KAZ', 'KGZ', 'KHM', 'KIR', 'KNA', 
                        'LSO', 'LUX', 'LVA', 
                        'MAR', 'MCO', 'MDV', 'MHL', 'MKD', 'MLT', 'MOZ', 'MYS', 
                        'NAM', 'NER', 'NIC', 'NIU', 'NPL', 'NZL',
                        'OMN',
                        'PAK', 'PHL', 'POL', 'PLW', 'PYF', 'PSE',
                        'QAT'
                        'ROU', 'RUS', 'RWA', 
                        'SAU', 'SDN', 'SEN', 'SLE', 'SSD', 'SVN', 'SYC', 
                        'TCD', 'TGO', 'TKM', 'TLS', 'TUR',
                        'UGA', 'UKR', 'USA', 'UZB', 
                        'VCT', 'VUT',
                        'WSM',
                        'XKX',
                        'YEM']
    df2 = df2.loc[~((df2['iso_code'] == 'AFG') & (df2['date'] == '2021-08-20')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'AFG') & (df2['date'] == '2021-11-04')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'AFG') & (df2['date'] == '2022-04-03')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'AGO') & (df2['date'].between('2022-04-01', '2022-04-26'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'AIA') & (df2['date'] == '2021-04-23')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'AIA') & (df2['date'] == '2022-05-13')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'ALB') & (df2['date'] == '2021-11-28')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'ALB') & (df2['date'].between('2022-04-17', '2022-04-24'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'ARE') & (df2['date'] == '2021-08-11')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'ARM') & (df2['date'].between('2021-07-04', '2021-07-11'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'ARM') & (df2['date'] == '2022-04-10')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'ASM') & (df2['date'].between('2021-04-30', '2021-05-05'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'ATG') & (df2['date'] == '2021-06-25')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'AUS') & (df2['date'] == '2021-03-30')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'AUS') & (df2['date'] == '2021-04-28')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'AUS') & (df2['date'] == '2021-08-14')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'AZE') & (df2['date'] == '2021-03-09')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'AZE') & (df2['date'] == '2021-04-04')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'BDI') & (df2['date'] == '2022-04-03')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'BEN') & (df2['date'] == '2021-05-31')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'BEN') & (df2['date'] == '2021-07-19')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'BEN') & (df2['date'].between('2021-06-10', '2021-06-24'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'BEN') & (df2['date'] == '2022-03-22')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'BEN') & (df2['date'].between('2022-04-17', '2022-05-01'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'BES1') & (df2['date'].between('2021-06-11', '2021-09-17'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'BES1') & (df2['date'].between('2021-10-08', '2021-11-12'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'BFA') & (df2['date'] == '2021-10-17')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'BFA') & (df2['date'] == '2021-07-19')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'BFA') & (df2['date'] == '2021-08-03')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'BGR') & (df2['date'] == '2021-07-25')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'BHR') & (df2['date'] == '2021-08-12')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'BLR') & (df2['date'] == '2022-03-20')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'BRA') & (df2['date'] == '2021-02-05')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'BRA') & (df2['date'] == '2022-02-18')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'BTN') & (df2['date'] == '2021-10-07')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'BTN') & (df2['date'] == '2021-10-14')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'BWA') & (df2['date'].between('2022-03-21', '2022-04-26'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'CAN') & (df2['date'] == '2021-10-01')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'CAN') & (df2['date'] == '2021-11-26')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'CAN') & (df2['date'] == '2022-05-27')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'CAF') & (df2['date'] == '2021-09-01')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'CAF') & (df2['date'] == '2022-05-29')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'CHE') & (df2['date'].between('2022-04-10', '2022-05-01'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'CHN') & (df2['date'] == '2021-04-26')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'CHN') & (df2['date'] == '2021-05-25')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'CHN') & (df2['date'] == '2021-06-06')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'CHN') & (df2['date'] == '2021-08-14')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'COD') & (df2['date'] == '2022-03-24')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'COD') & (df2['date'].between('2022-04-24', '2022-05-08'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'COG') & (df2['date'] == '2021-11-06')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'COG') & (df2['date'] == '2022-05-01')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'COK') & (df2['date'] == '2022-02-06')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'COK') & (df2['date'] == '2022-03-29')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'COK') & (df2['date'] == '2022-04-16')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'COK') & (df2['date'].between('2022-04-03', '2022-04-10'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'COL') & (df2['date'] == '2021-04-23')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'COL') & (df2['date'] == '2021-05-28')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'COM') & (df2['date'] == '2021-07-19')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'CYM') & (df2['date'].between('2022-02-25', '2022-03-11'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'CYM') & (df2['date'] == '2022-04-01')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'DNK') & (df2['date'].between('2021-09-26', '2021-10-10'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'DNK') & (df2['date'].between('2022-01-02', '2022-01-09'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'DNK') & (df2['date'] == '2022-04-17')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'DMA') & (df2['date'] == '2021-04-16')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'DJI') & (df2['date'].between('2021-08-31', '2021-09-18'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'DJI') & (df2['date'] == '2022-04-17')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'DJI') & (df2['date'].between('2022-05-22', '2022-06-05'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'EGY') & (df2['date'].between('2022-04-17', '2022-05-08'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'ETH') & (df2['date'] == '2022-04-10')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'ESP') & (df2['date'] == '2021-11-14')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'ESP') & (df2['date'] == '2022-01-30')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'FJI') & (df2['date'] == '2022-03-11')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'FRA') & (df2['date'].between('2021-09-12', '2021-10-10'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'FRA') & (df2['date'].between('2022-02-20', '2022-04-24'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'FSM') & (df2['date'].between('2021-04-30', '2021-05-05'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'GAB') & (df2['date'] == '2021-09-16')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'GAB') & (df2['date'] == '2022-01-22')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'GBR') & (df2['date'] == '2021-04-25')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'GBR') & (df2['date'] == '2021-05-08')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'GBR') & (df2['date'] == '2021-05-16')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'GEO') & (df2['date'] == '2021-03-28')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'GLP') & (df2['date'].between('2021-04-23', '2021-05-07'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'GLP') & (df2['date'].between('2022-02-11', '2022-02-18'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'GMB') & (df2['date'] == '2021-10-24')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'GMB') & (df2['date'] == '2021-11-05')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'GNB') & (df2['date'] == '2021-06-24')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'GNB') & (df2['date'] == '2021-08-20')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'GNB') & (df2['date'] == '2022-04-07')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'GNQ') & (df2['date'].between('2021-05-10', '2021-05-21'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'GNQ') & (df2['date'] == '2021-08-23')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'GNQ') & (df2['date'].between('2022-05-08', '2022-05-22'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'GUM') & (df2['date'].between('2021-05-07', '2021-05-18'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'GUY') & (df2['date'] == '2021-04-16')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'GUY') & (df2['date'] == '2021-11-29')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'HND') & (df2['date'] == '2022-02-25')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'HND') & (df2['date'] == '2022-03-18')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'HND') & (df2['date'] == '2022-03-25')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'HND') & (df2['date'] == '2022-04-01')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'HND') & (df2['date'] == '2022-05-27')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'HUN') & (df2['date'] == '2021-06-06')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'HUN') & (df2['date'].between('2021-07-04', '2021-07-18'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'HUN') & (df2['date'] == '2021-08-08')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'IND') & (df2['date'] == '2022-06-07')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'IRL') & (df2['date'] == '2022-02-06')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'IRQ') & (df2['date'] == '2022-02-01')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'ISL') & (df2['date'].between('2021-06-27', '2021-07-11'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'ISL') & (df2['date'].between('2021-09-12', '2021-09-30'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'ISR') & (df2['date'] == '2021-11-07')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'ISR') & (df2['date'].between('2021-07-04', '2021-08-08'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'ISR') & (df2['date'].between('2021-12-12', '2021-12-26'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'ISR') & (df2['date'] == '2022-02-13')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'ISR') & (df2['date'] == '2022-03-06')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'JOR') & (df2['date'] == '2021-08-11')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'JOR') & (df2['date'] == '2021-10-27')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'JOR') & (df2['date'] == '2022-01-09')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'JOR') & (df2['date'] == '2022-04-11')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'JPN') & (df2['date'] == '2021-05-16')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'JPN') & (df2['date'] == '2022-01-03')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'KAZ') & (df2['date'].between('2021-05-02', '2021-05-09'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'KAZ') & (df2['date'].between('2021-08-22', '2021-08-29'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'KAZ') & (df2['date'] == '2021-09-12')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'KAZ') & (df2['date'] == '2021-12-12')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'KAZ') & (df2['date'].between('2022-02-06', '2022-02-20'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'KGZ') & (df2['date'] == '2021-05-02')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'KGZ') & (df2['date'] == '2021-05-30')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'KGZ') & (df2['date'] == '2021-08-22')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'KHM') & (df2['date'] == '2021-08-13')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'KHM') & (df2['date'].between('2021-10-22', '2021-10-24'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'KHM') & (df2['date'] == '2022-05-02')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'KHM') & (df2['date'] == '2022-05-06')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'KIR') & (df2['date'] == '2022-01-04')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'KNA') & (df2['date'] == '2022-02-11')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'KOR') & (df2['date'].between('2021-03-30', '2021-04-05'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'KOR') & (df2['date'] == '2021-10-17')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'KOR') & (df2['date'] == '2022-05-09')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'KOR') & (df2['date'] == '2022-05-31')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'KWT') & (df2['date'] == '2021-03-16')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'KWT') & (df2['date'] == '2021-04-28')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'KWT') & (df2['date'] == '2022-02-10')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'LBN') & (df2['date'] == '2021-08-31')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'LBR') & (df2['date'] == '2021-04-14')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'LBR') & (df2['date'] == '2021-10-08')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'LBR') & (df2['date'].between('2021-12-31', '2022-02-10'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'LBY') & (df2['date'] == '2022-05-26')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'LBY') & (df2['date'] == '2022-05-19')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'LBY') & (df2['date'].between('2022-04-17', '2022-05-15'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'LSO') & (df2['date'].between('2021-05-21', '2021-05-24'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'LSO') & (df2['date'] == '2022-03-04')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'LUX') & (df2['date'].between('2021-07-18', '2021-07-25'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'LUX') & (df2['date'] == '2021-10-31')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'LUX') & (df2['date'] == '2022-02-13')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'LVA') & (df2['date'].between('2021-07-11', '2021-07-18'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MAR') & (df2['date'] == '2022-04-26')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MAR') & (df2['date'] == '2021-07-27')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MAR') & (df2['date'] == '2022-05-23')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MAR') & (df2['date'] == '2022-05-29')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MAR') & (df2['date'] == '2022-04-17')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MAR') & (df2['date'].between('2022-04-27', '2022-05-08'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MCO') & (df2['date'] == '2021-07-04')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MCO') & (df2['date'] == '2022-01-02')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MDV') & (df2['date'] == '2021-10-15')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MHL') & (df2['date'] == '2021-05-07')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MKD') & (df2['date'] == '2021-10-31')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MKD') & (df2['date'] == '2022-06-05')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MLI') & (df2['date'].between('2022-04-17', '2022-04-26'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MLI') & (df2['date'] == '2022-05-08')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MLT') & (df2['date'].between('2021-08-15', '2021-08-22'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MLT') & (df2['date'] == '2021-09-27')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MNE') & (df2['date'] == '2021-08-15')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MNE') & (df2['date'] == '2022-02-27')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MNE') & (df2['date'] == '2022-05-22')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MNE') & (df2['date'] == '2022-05-29')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MNE') & (df2['date'] == '2022-06-05')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MOZ') & (df2['date'] == '2022-04-09')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MOZ') & (df2['date'] == '2022-05-08')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MRT') & (df2['date'] == '2021-11-14')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MRT') & (df2['date'] == '2022-01-04')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MRT') & (df2['date'] == '2022-04-06')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MRT') & (df2['date'].between('2022-03-21', '2022-05-01'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MUS') & (df2['date'] == '2022-05-08')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MUS') & (df2['date'] == '2022-05-29')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MUS') & (df2['date'].between('2022-04-17', '2022-05-15'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'MYS') & (df2['date'] == '2021-09-20')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'NAM') & (df2['date'].between('2021-08-16', '2021-09-26'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'NAM') & (df2['date'].between('2021-10-11', '2021-11-13'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'NAM') & (df2['date'] == '2022-05-29')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'NAM') & (df2['date'].between('2022-03-18', '2022-05-22'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'NER') & (df2['date'].between('2021-07-05', '2021-07-13'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'NER') & (df2['date'] == '2021-08-28')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'NIC') & (df2['date'] == '2021-08-09')), :]
#     df2 = df2.loc[~((df2['iso_code'] == 'NIC') & (df2['date'].between('2021-07-17', '2021-11-28')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'NIU') & (df2['date'] == '2021-07-16')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'NLD') & (df2['date'].between('2022-04-10', '2022-05-15'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'NPL') & (df2['date'] == '2021-04-28')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'NZL') & (df2['date'] == '2021-08-09')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'OMN') & (df2['date'] == '2021-09-27')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'OMN') & (df2['date'].between('2022-05-16', '2022-05-23'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'PAK') & (df2['date'] == '2021-11-14')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'PAK') & (df2['date'] == '2021-11-18')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'POL') & (df2['date'].between('2021-07-18', '2021-08-01'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'POL') & (df2['date'].between('2021-10-10', '2021-11-07'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'POL') & (df2['date'].between('2021-12-12', '2021-12-19'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'PHL') & (df2['date'] == '2021-07-26')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'PHL') & (df2['date'] == '2021-08-06')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'PLW') & (df2['date'] == '2021-05-05')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'PRI') & (df2['date'] == '2021-11-29')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'PRY') & (df2['date'] == '2022-02-25')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'PSE') & (df2['date'] == '2021-04-05')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'PSE') & (df2['date'] == '2021-03-02')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'PSE') & (df2['date'].between('2021-08-31', '2021-09-06'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'PSE') & (df2['date'].between('2021-10-18', '2021-10-25'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'PYF') & (df2['date'] == '2021-08-30')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'QAT') & (df2['date'] == '2021-08-11')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'QAT') & (df2['date'] == '2021-06-24')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'RUS') & (df2['date'] == '2021-05-09')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'RUS') & (df2['date'] == '2021-05-17')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'ROU') & (df2['date'] == '2021-06-27')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'ROU') & (df2['date'].between('2021-07-11', '2021-08-08'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'RWA') & (df2['date'].between('2021-04-28', '2021-06-17'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'RWA') & (df2['date'].between('2021-06-24', '2021-07-26'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'RWA') & (df2['date'] == '2021-08-23')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'RWA') & (df2['date'].between('2021-09-24', '2021-10-04'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'RWA') & (df2['date'] == '2021-10-18')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'RWA') & (df2['date'] == '2021-10-26')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'RWA') & (df2['date'] == '2021-11-10')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SAU') & (df2['date'] == '2021-03-16')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SAU') & (df2['date'] == '2021-07-11')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SAU') & (df2['date'] == '2022-04-14')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SDN') & (df2['date'].between('2021-04-28', '2021-05-09'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SDN') & (df2['date'] == '2021-12-08')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SDN') & (df2['date'] == '2022-04-17')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SDN') & (df2['date'].between('2022-04-24', '2022-05-29'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SEN') & (df2['date'].between('2021-06-14', '2021-06-17'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SEN') & (df2['date'].between('2021-10-18', '2021-10-20'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SEN') & (df2['date'].between('2021-12-09', '2021-12-21'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SEN') & (df2['date'] == '2022-02-09')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SEN') & (df2['date'] == '2022-03-01')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SEN') & (df2['date'].between('2022-02-22', '2022-04-26'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SEN') & (df2['date'] == '2022-05-01')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SEN') & (df2['date'] == '2022-05-08')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SGP') & (df2['date'].between('2021-10-02', '2021-10-22'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SLE') & (df2['date'].between('2021-07-05', '2021-07-13'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SOM') & (df2['date'] == '2021-04-21')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SOM') & (df2['date'] == '2022-05-29')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SOM') & (df2['date'] == '2022-05-22')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SOM') & (df2['date'] == '2022-06-05')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SOM') & (df2['date'] == '2022-04-17')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SRB') & (df2['date'] == '2021-04-18')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SSD') & (df2['date'] == '2022-01-01')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'STP') & (df2['date'].between('2022-03-07', '2022-05-01'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SVN') & (df2['date'] == '2021-07-18')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SVN') & (df2['date'].between('2022-02-13', '2022-02-20'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SVN') & (df2['date'].between('2022-05-15', '2022-05-29'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SVN') & (df2['date'] == '2022-06-05')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SXM') & (df2['date'] == '2022-05-13')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SXM') & (df2['date'].between('2022-05-20', '2022-06-10'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SYC') & (df2['date'] == '2021-03-04')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SYC') & (df2['date'] == '2021-08-25')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SYC') & (df2['date'].between('2021-04-21', '2021-04-26'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SYC') & (df2['date'] == '2021-07-10')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SYC') & (df2['date'] == '2022-05-08')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SYR') & (df2['date'] == '2022-04-08')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SYR') & (df2['date'] == '2022-04-26')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'SXM') & (df2['date'] == '2022-06-03')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'TCD') & (df2['date'] == '2021-10-21')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'TGO') & (df2['date'] == '2021-05-10')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'TKM') & (df2['date'].between('2021-04-04', '2021-09-05'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'TLS') & (df2['date'] == '2021-10-02')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'TUN') & (df2['date'].between('2021-12-28', '2022-02-27'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'TUN') & (df2['date'] == '2022-05-29')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'TUN') & (df2['date'] == '2022-05-30')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'TUN') & (df2['date'].between('2022-04-17', '2022-05-15'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'TUN') & (df2['date'].between('2022-05-22', '2022-06-05'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'TUR') & (df2['date'] == '2021-05-09')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'TUR') & (df2['date'] == '2021-05-17')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'UGA') & (df2['date'] == '2021-05-18')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'UGA') & (df2['date'] == '2021-10-13')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'UKR') & (df2['date'] == '2021-04-11')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'UKR') & (df2['date'] == '2021-05-23')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'UKR') & (df2['date'].between('2021-02-27', '2021-03-02'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'USA') & (df2['date'] == '2021-08-09')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'USA') & (df2['date'].between('2021-11-26', '2021-11-29'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'UZB') & (df2['date'].between('2021-05-12', '2021-06-13'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'VCT') & (df2['date'].between('2021-07-23', '2021-08-09'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'VUT') & (df2['date'] == '2021-08-16')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'VUT') & (df2['date'] == '2022-05-01')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'WSM') & (df2['date'] == '2021-06-25')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'XAA') & (df2['date'] == '2022-06-10')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'XAA') & (df2['date'].between('2022-05-13', '2022-06-03'))), :]
    df2 = df2.loc[~((df2['iso_code'] == 'XKX') & (df2['date'] == '2021-05-02')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'XKX') & (df2['date'] == '2021-05-30')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'XKX') & (df2['date'] == '2022-04-10')), :]
    df2 = df2.loc[~((df2['iso_code'] == 'YEM') & (df2['date'].between('2021-11-14', '2022-01-17'))), :]
    return df2, manual_fix_list


def fix_issues_at_least_one_dose(df2):
    print(" > Fix data issues with at_least_one_dose...")
    df2.loc[((df2['iso_code'] == 'AFG') & (df2['date'].between('2021-07-14', '2021-07-27'))), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'AFG') & (df2['date'] == '2021-10-31')), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'AGO') & (df2['date'] == '2021-08-23')), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'AGO') & (df2['date'] == '2021-10-26')), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'ALB') & (df2['date'] == '2021-04-24')), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'ALB') & (df2['date'] == '2021-04-28')), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'ALB') & (df2['date'] == '2021-05-04')), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'AND') & (df2['date'].between('2021-03-19', '2021-04-12'))), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'AZE') & (df2['date'].between('2021-02-16', '2021-03-09'))), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'BFA') & (df2['date'].between('2021-10-17', '2021-10-31'))), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'BMU') & (df2['date'] == '2021-05-14')), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'BMU') & (df2['date'] == '2021-06-25')), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'DJI') & (df2['date'] == '2021-06-23')), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'DJI') & (df2['date'] == '2021-08-17')), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'COG') & (df2['date'] == '2021-04-14')), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'COG') & (df2['date'] == '2021-07-26')), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'COG') & (df2['date'].between('2021-06-08', '2021-06-10'))), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'COG') & (df2['date'].between('2021-11-15', '2021-11-16'))), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'COM') & (df2['date'] == '2021-05-31')), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'COM') & (df2['date'].between('2021-08-20', '2021-09-06'))), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'DNK') & (df2['date'].between('2021-12-05', '2021-12-26'))), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'GMB') & (df2['date'] == '2021-09-02')), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'HUN') & (df2['date'].between('2021-06-20', '2021-06-27'))), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'LBR') & (df2['date'].between('2021-08-23', '2021-09-03'))), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'STP') & (df2['date'] == '2021-07-26')), 'at_least_one_dose'] = None
    df2.loc[((df2['iso_code'] == 'ZAF') & (df2['date'] == '2021-06-20')), 'at_least_one_dose'] = None
    return df2


def fix_issues_fully_vaccinated(df2):
    print(" > Fix data issues with fully_vaccinated...")
    df2.loc[((df2['iso_code'] == 'AFG') & (df2['date'] == '2021-08-30')), 'fully_vaccinated'] = None
    df2.loc[((df2['iso_code'] == 'AZE') & (df2['date'] == '2021-02-16')), 'fully_vaccinated'] = None
    df2.loc[((df2['iso_code'] == 'CAN') & (df2['date'] == '2021-04-23')), 'fully_vaccinated'] = None
    df2.loc[((df2['iso_code'] == 'COG') & (df2['date'].between('2021-07-12', '2021-08-03'))), 'fully_vaccinated'] = None
    df2.loc[((df2['iso_code'] == 'DMA') & (df2['date'] == '2021-07-16')), 'fully_vaccinated'] = None
    df2.loc[((df2['iso_code'] == 'DNK') & (df2['date'].between('2021-12-05', '2021-12-26'))), 'fully_vaccinated'] = None
    df2.loc[((df2['iso_code'] == 'DZA') & (df2['date'].between('2021-09-25', '2021-09-28'))), 'fully_vaccinated'] = None
    df2.loc[((df2['iso_code'] == 'EGY') & (df2['date'] == '2021-09-11')), 'fully_vaccinated'] = None
    df2.loc[((df2['iso_code'] == 'EGY') & (df2['date'] == '2021-09-16')), 'fully_vaccinated'] = None
    df2.loc[((df2['iso_code'] == 'EGY') & (df2['date'].between('2021-05-09', '2021-05-11'))), 'fully_vaccinated'] = None
    df2.loc[((df2['iso_code'] == 'GUY') & (df2['date'] == '2021-05-07')), 'fully_vaccinated'] = None
    df2.loc[((df2['iso_code'] == 'HND') & (df2['date'] == '2021-04-23')), 'fully_vaccinated'] = None
    df2.loc[((df2['iso_code'] == 'HND') & (df2['date'] == '2021-06-04')), 'fully_vaccinated'] = None
    df2.loc[((df2['iso_code'] == 'HND') & (df2['date'].between('2021-08-13', '2021-08-20'))), 'fully_vaccinated'] = None
    df2.loc[((df2['iso_code'] == 'HND') & (df2['date'].between('2021-09-03', '2021-09-10'))), 'fully_vaccinated'] = None
    df2.loc[((df2['iso_code'] == 'HUN') & (df2['date'] == '2021-08-01')), 'fully_vaccinated'] = None
    df2.loc[((df2['iso_code'] == 'PSE') & (df2['date'] == '2021-09-27')), 'fully_vaccinated'] = None
    df2.loc[((df2['iso_code'] == 'SEN') & (df2['date'] == '2022-05-08')), 'fully_vaccinated'] = None
    df2.loc[((df2['iso_code'] == 'STP') & (df2['date'].between('2021-07-05', '2021-07-13'))), 'fully_vaccinated'] = None
    return df2


def check_for_total_dose_decreases_1(df1):
    # first round of error checking, identify the cases where total doses decreases, this is the master set of known data issues and will go into the data_errors tab in the output Excel
    print(" > Check for Errors in Data: Finding decreases in the total doses in df1...")
    df_errors1 = df1[['iso_code', 'country_name', 'date', 'total_doses']].copy()
    df_errors1["total_doses_prev_period"] = df_errors1.sort_values(by=['date'], ascending=True). \
        groupby(['iso_code'])['total_doses'].shift(1).copy()
    df_errors1 = df_errors1.loc[(df_errors1["total_doses"] < df_errors1["total_doses_prev_period"]), :]
    df_errors1["total_doses_in_period"] = df_errors1["total_doses"] - df_errors1["total_doses_prev_period"]
    df_errors1["is_data_error"] = 1
    return df_errors1


def check_for_total_dose_decreases_2(df2):
    # second round of data issues, these have the manual fixes removed and "should be" one offs
    print(" > Check for Errors in Data: Finding decreases in the total doses in df2...")
    df_errors2 = df2[['iso_code', 'country_name', 'date', 'total_doses']].copy()
    df_errors2["total_doses_prev_period"] = df_errors2.sort_values(by=['date'], ascending=True). \
        groupby(['iso_code'])['total_doses'].shift(1).copy()
    df_errors2 = df_errors2.loc[(df_errors2["total_doses"] < df_errors2["total_doses_prev_period"]), :]
    df_errors2["total_doses_in_period"] =  df_errors2["total_doses"] - df_errors2["total_doses_prev_period"]
    df_errors2["to_remove"] = 1

    # check on what's still showing up as an error for total doses
    print(" > Check for Errors in Data: finding other errors...")
    df_errors1b = df_errors2.copy()
    df_errors1b = df_errors1b.rename({'to_remove': 'is_data_error'}, axis = 1)
    return df_errors2, df_errors1b


def check_for_first_dose_decreases(df2):
    print(" > Check for Errors in Data: First Dose Error Fix...")
    df_errors1st = df2[['iso_code', 'country_name', 'date', 'at_least_one_dose']].copy()
    df_errors1st["1st_dose_prev_period"] = df_errors1st.sort_values(by=['date'], ascending=True). \
        groupby(['iso_code'])['at_least_one_dose'].shift(1).copy()
    df_errors1st = df_errors1st.loc[(df_errors1st["at_least_one_dose"] < df_errors1st["1st_dose_prev_period"]), :]
    df_errors1st["total_doses_in_period"] =  df_errors1st["at_least_one_dose"] - df_errors1st["1st_dose_prev_period"]
    df_errors1st["to_remove_1st"] = 1
    return df_errors1st


def check_for_second_dose_decreases(df2):
    print(" > Check for Errors in Data: Second Dose Error Fix...")
    df_errors2nd = df2[['iso_code', 'country_name', 'date', 'fully_vaccinated']].copy()
    df_errors2nd["2nd_dose_prev_period"] = df_errors2nd.sort_values(by=['date'], ascending=True). \
        groupby(['iso_code'])['fully_vaccinated'].shift(1).copy()
    df_errors2nd = df_errors2nd.loc[(df_errors2nd["fully_vaccinated"] < df_errors2nd["2nd_dose_prev_period"]), :]
    df_errors2nd["total_doses_in_period"] =  df_errors2nd["fully_vaccinated"] - df_errors2nd["2nd_dose_prev_period"]
    df_errors2nd["to_remove_2nd"] = 1
    return df_errors2nd


def join_errors_with_df(df2, df_errors1, df_errors2, df_errors1st, df_errors2nd, manual_fix_list):
    print(" > Merging error columns with dataframe")
    df3 = df2.copy()
    df3.loc[:,'manual_adjustment'] = df3["iso_code"].apply(lambda x: 1 if x in manual_fix_list else 0).astype(int)
    df3 = df3.merge(df_errors1[['iso_code', 'date', 'is_data_error']], on = ['iso_code', 'date'], how = 'left')
    df3["is_data_error"].fillna(0, inplace = True)
    df3 = df3.merge(df_errors2[['iso_code', 'date', 'to_remove']], on = ['iso_code', 'date'], how = 'left')
    df3["to_remove"].fillna(0, inplace = True)
    df3 = df3.merge(df_errors1st[['iso_code', 'date', 'to_remove_1st']], on = ['iso_code', 'date'], how = 'left')
    df3["to_remove_1st"].fillna(0, inplace = True)
    df3 = df3.merge(df_errors2nd[['iso_code', 'date', 'to_remove_2nd']], on = ['iso_code', 'date'], how = 'left')
    df3["to_remove_2nd"].fillna(0, inplace = True)
    df3[['is_data_error', 'to_remove', 'to_remove_1st', 'to_remove_2nd']].astype(int)

    df3 = df3[['iso_code','date','country_name','total_doses','at_least_one_dose','fully_vaccinated','persons_booster_add_dose','date_accessed','date_week','is_latest_week_reported','manual_adjustment','is_data_error','to_remove','to_remove_1st','to_remove_2nd']]
    return df3


def export_data(df3):
    print(" > Saving analysis_vx_throughput_data_cleaned to csv file...")
    df3.to_csv('data/_input/supply_data/analysis_vx_throughput_data_cleaned.csv', index = False)
    print(" > Done")


# running the above functions
who, iso_mapping = import_data()
who = convert_data_types(who)
df1 = cleaning(who)
df1 = date_to_date_week(df1)
df1, df2 = map_iso_codes(df1, iso_mapping)
df2, manual_fix_list = fix_issues_total_doses(df2)
df2 = fix_issues_at_least_one_dose(df2)
df2 = fix_issues_fully_vaccinated(df2)
df_errors1 = check_for_total_dose_decreases_1(df1)
df_errors2, df_errors1b = check_for_total_dose_decreases_2(df2)
df_errors1st = check_for_first_dose_decreases(df2)
df_errors2nd = check_for_second_dose_decreases(df2)
df3 = join_errors_with_df(df2, df_errors1, df_errors2, df_errors1st, df_errors2nd, manual_fix_list)
export_data(df3)