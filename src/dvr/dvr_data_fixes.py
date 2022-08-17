import pandas as pd

def import_data(throughput_data, output_daily):
    print(' > Importing data for data_fixes.py ...')
    iso_mapping = pd.read_excel("data/_input/static/base_entitydetails.xlsx")
    iso_mapping.rename(
        {'NAMEWORKEN': 'country_name', 'CODE': 'iso_code'},
        axis = 1, 
        inplace = True)
    iso_mapping = iso_mapping[['country_name', 'iso_code']]
    raw = throughput_data
    who = output_daily
    return iso_mapping, raw, who


def clean_country_names(raw, iso_mapping):
    print(" > Matching ambiguous country names...")
    df1 = raw.copy()
    df1['total_doses'] = df1['total_doses'].astype(float)
    df1 = df1.loc[((~(df1['total_doses'].isna())) & (df1['total_doses'] > 0)), :]
    df1 = df1[['country_name', 'date', 'total_doses', 'at_least_one_dose', 'fully_vaccinated', 'date_accessed']]
    df1['date'] = pd.to_datetime(df1['date'], format = '%Y-%m-%d')
    df1.drop_duplicates(inplace = True)
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
    df1.loc[df1['country_name'] == 'Kosovo', 'iso_code'] = 'XKX'
    df1.fillna(0, inplace = True)
    return df1


def clean_who_data(who):
    print(" > Transforming WHO data before merge...")
    who1 = who[['iso_code', 'date', 'total_doses', 'at_least_one_dose', 'fully_vaccinated']]
    who1['date'] = pd.to_datetime(who1['date'], format = '%Y-%m-%d')
    who1['check'] = 1
    who1.fillna(0, inplace = True)
    return who1


def merge_dataframes(df1, who1):
    print(" > Merging dataframes for data fixes...")
    df2 = df1.merge(who1, on = ['iso_code', 'date', 'total_doses', 'at_least_one_dose', 'fully_vaccinated'], how = 'left')
    df2 = df2.loc[df2['check'].isna(), :]
    df2 = df2[['iso_code', 'country_name', 'date', 'total_doses', 'at_least_one_dose', 'fully_vaccinated', 'date_accessed']]
    return df2


def main(throughput_data, output_daily):
    iso_mapping, raw, who = import_data(throughput_data, output_daily)
    df1 = clean_country_names(raw, iso_mapping)
    who1 = clean_who_data(who)
    df2 = merge_dataframes(df1, who1)
    print(" > Closing Python Environment")