import pandas as pd

iso_mapping = pd.read_csv("data/_input/supply_data/iso_mapping.csv")
raw = pd.read_csv("data/_input/supply_data/analysis_vx_throughput_data.csv")
who = pd.read_csv("data/_input/supply_data/analysis_vx_throughput_output_daily.csv")

df1 = raw.copy()
df1 = df1.loc[((~(df1['total_doses'].isna())) & (df1['total_doses'] > 0)), :]
df1 = df1[['country_name', 'date', 'total_doses', 'at_least_one_dose', 'fully_vaccinated', 'date_accessed']]
df1['date'] = pd.to_datetime(df1['date'], format = '%Y-%m-%d')
df1.drop_duplicates(inplace = True)
df1.merge(iso_mapping, on = 'country_name', how = 'left')
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

df1.fillna(0, inplace = True)

who1 = who[['iso_code', 'date', 'total_doses', 'at_least_one_dose', 'fully_vaccinated']]
who1['check'] = 1
who1.fillna(0, inplace = True)

df2 = df1.merge(who1, on = ['iso_code', 'date', 'total_doses', 'at_least_one_dose', 'fully_vaccinated'], how = 'left')
df2 = df2.loc[df2['check'].isna(), :]
df2 = df2[['iso_code', 'country_name', 'date', 'total_doses', 'at_least_one_dose', 'fully_vaccinated', 'date_accessed']]

df2.to_csv("data/_input/supply_data/analysis_vx_data_fixes.csv", index=False)