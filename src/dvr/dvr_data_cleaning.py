from pickle import TRUE
import pandas as pd
import numpy as np
import datetime
import os
import matplotlib.pyplot as plt
from matplotlib.ticker import FormatStrFormatter
import seaborn as sns
import warnings
warnings.filterwarnings("ignore")
sns.set(rc={"figure.dpi":300, 'savefig.dpi':300})

def create_path(newpath_child):
    parent_dir = 'data'
    newpath = os.path.join(parent_dir, newpath_child)
    if not os.path.exists(newpath):
        print(" > Creating new folder: data/", newpath_child)
        os.makedirs(newpath)
        print(" > New folder created.")

def clean_path(folder):
    create_path(folder)
    print(" > Deleting any pre-existing plots from data/cleaning_log/...")
    parent_dir = 'data'
    path = parent_dir + '/' + folder
    for f in os.listdir(path):
        try:
            os.remove(os.path.join(path, f))
        except:
            for j in os.listdir(os.path.join(path, f)):
                try:
                    os.remove(os.path.join(os.path.join(path, f), j))
                except:
                    for k in os.listdir(os.path.join(os.path.join(path, f), j)):
                        os.remove(os.path.join(os.path.join(os.path.join(path, f), j), k))


def import_data(throughput_data):
    print(" > Loading dataset...")
    #who = pd.read_csv("data/_input/supply_data/analysis_vx_throughput_data.csv")
    who = throughput_data
    iso_mapping = pd.read_excel("data/_input/static/base_entitydetails.xlsx")
    iso_mapping.rename(
        {'NAMEWORKEN': 'country_name', 'CODE': 'iso_code'},
        axis = 1, 
        inplace = True)
    iso_mapping = iso_mapping[['country_name', 'iso_code']]
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
    who = who[who['total_doses'] >= 0]

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
    df1.loc[df1['country_name'] == 'Bonaire, Sint Eustatius And Saba/Saba', 'iso_code'] = 'SAB' # changed from BES1
    df1.loc[df1['country_name'] == 'Bonaire, Sint Eustatius And Saba/Sint Eustatius', 'iso_code'] = 'STA' # changed from BES1
    df1.loc[df1['country_name'] == 'Bonaire, Sint Eustatius And Saba/Bonaire', 'iso_code'] = 'BON' # changed from XAA
    df1.loc[df1['country_name'] == 'Sint Maarten', 'iso_code'] = 'SXM'
    df1.loc[df1['country_name'] == 'Pitcairn Islands', 'iso_code'] = 'PCN'
    df1.loc[df1['country_name'] == 'Northern Mariana Islands (Commonwealth Of The)', 'iso_code'] = 'MNP'
    df1.loc[df1['country_name'] == 'The United Kingdom', 'iso_code'] = 'GBR'
    df1.loc[df1['country_name'] == 'Côte D’Ivoire', 'iso_code'] = 'CIV'
    df1.loc[df1['country_name'] == 'Falkland Islands (Malvinas)', 'iso_code'] = 'FLK'
    df1.loc[df1['country_name'] == 'Liechtenstein', 'iso_code'] = 'LIE'
    df1.loc[df1['country_name'] == 'Kosovo', 'iso_code'] = 'XKX'

    print(" > Identifying countries that have not reported for the latest week...")
    max_date_week = df1['date_week'].max()
    df1['max_date_week'] = df1.groupby('iso_code')['date_week'].transform('max')
    df1['is_latest_week_reported'] = 0
    df1.loc[df1['max_date_week'] == max_date_week, 'is_latest_week_reported'] = 1
    df1.drop('max_date_week', axis = 1, inplace = True)
    
    print(" > Remove missing values...")
    df1 = df1.fillna(0)
    df2 = df1.copy()
    return df1, df2


def monotonic(series):
    """
    This function checks whether a list of numbers is monotonically decreasing.
    """
    if len(series) <= 1:
        return True
    else:
        if series[0] >= series[1]:
            series.pop(0)
            return monotonic(series)
        else:
            return False


def filter_country_data(df2, country):
    '''
    This function filters out the data for one respective country. 
    '''
    country_data = df2.loc[df2['iso_code'] == country, :].copy()
    country_data.sort_values(by = ['date'], ascending = False, inplace = True)
    return country_data


def printing_log(country_data, log):
    '''
    This function prints out the number of changes per country.
    '''
    country_data.reset_index(drop = True, inplace = True)
    country_code = country_data.loc[0,'iso_code']
    country_name = country_data.loc[0,'country_name']
    n_changes = len(log.loc[log['iso_code'] == country_code, :])
    print(" > {n_changes:3n} obs. removed from {country_code:s} ({country_name})" \
        .format(n_changes = n_changes, country_code = country_code, country_name = country_name))


def delete_row(country_data, df, row, log, reset_index = True):
    """
    This function deletes a specified row from `country_data`,
    adds a flag to the respective row in `df`,
    and reports the deleted row in `log`.
    """
    if reset_index:
        country_data.reset_index(drop = True, inplace = True)
        country_name = country_data.loc[row,'iso_code']
    country_code = country_data.loc[row,'iso_code']
    date = country_data.loc[row,'date']
    df.loc[((df['iso_code'] == country_code) & (df['date'] == date)),'to_delete_automized_clean'] = 1
    country_data.drop(row, axis = 0, inplace = True)
    addition = pd.DataFrame({'iso_code': [country_code], 'date': [date]})
    log = pd.concat([log, addition], ignore_index = True)
    return country_data, df, log


def deep_clean(country_data, row, df, log, var_to_clean_iloc):
    """
    This function checks for the best way to deal with more complicated cases,
    that is, when the next observation is lower than at least two previous observations.

    The logic used is:
    1. How many rows would have to be deleted before and including the current
        observation to remove the decrease in "total_doses"? Return count.
    2. How many rows would have to be deleted after the current observation
        to have an increase from the current to the next observation? Return count.
    3. If count of 1. is greater than 2., then remove the next observation.
    4. If count of 1. is smaller than 2., then remove the current observation.
    """
    count_previous_larger = 0
    count_after_smaller = 0

    row_backwards_check = row
    row_forward_check = row - 1
    not_exhausted = True
    next_value = country_data.iloc[min(row - 1, len(country_data) - 1), var_to_clean_iloc]
    current_value = country_data.iloc[min(row, len(country_data) - 1), var_to_clean_iloc]
    while (next_value < country_data.iloc[min(row_backwards_check, len(country_data) - 1), var_to_clean_iloc]) and not_exhausted:
        count_previous_larger += 1
        row_backwards_check += 1
        if row_backwards_check > len(country_data) - 1:
            not_exhausted = False
    not_exhausted = True
    while (current_value > country_data.iloc[max(row_forward_check, 0), var_to_clean_iloc]) and not_exhausted:
        count_after_smaller += 1
        row_forward_check -= 1
        if row_forward_check < 0:
            not_exhausted = False
    row_to_delete = row
    if count_previous_larger > count_after_smaller:
        row_to_delete -= 1
    return delete_row(country_data, df, row_to_delete, log)


def row_check(country_data, row, df, log, var_to_clean_iloc):
    """
    This is a recursive function that checks a row of the `country_data` dataframe.
    It determines whether an observation should be deleted if there is a decrease in total_doses.
    The logical steps are:
    1. Is current observation (t) larger than the next observation (t+1)?
        a. If true, is the previous observation (t-1) also larger than the next observation (t+1)? 
            i. If true, perfrom deep clean (see function above.)
            ii. If false, delete current observation. (Using the inductive bias that recent data is better.)
        b. If false, do not delete observation.
    """
    ## check previous
    if len(country_data) > row:
        country_data, df, log = row_check(country_data, row + 1, df, log, var_to_clean_iloc)
    else:
        return country_data, df, log
    
    ## check itself
    previous_value = country_data.iloc[min(row + 1, len(country_data) - 1), var_to_clean_iloc]
    current_value = country_data.iloc[min(row, len(country_data) - 1), var_to_clean_iloc]
    next_value = country_data.iloc[max(row - 1, 0), var_to_clean_iloc]

    if current_value > next_value: # is it larger than next one?
        if previous_value > next_value: # is previous larger than next?
            country_data, df, log = deep_clean(country_data, row, df, log, var_to_clean_iloc)
        else:
            country_data, df, log = delete_row(country_data, df, row, log)
    return country_data, df, log


def export_plots_of_changes(df2, uncleaned, country, log, var_to_clean, folder):
    """
    This function produces a lineplot comparing the cleaned and uncleaned 'total_doses' for a country.
    
    1. Filter data for the respective country.
    2. Create a list of all changes and group them if there are closer than 20 observations apart.
    3. Produce a plot for all groups of changes.
    4. Export the plots to `data/logged_changes`.
    """
    create_path("cleaning_log/" + folder)
    country_data = df2.loc[df2['iso_code'] == country, :].copy()
    uncleaned_c = uncleaned.loc[uncleaned['iso_code'] == country, :].copy()
    country_data.sort_values(by = ['date'], ascending = False, inplace = True)
    country_data = country_data.loc[country_data['to_delete_automized_clean'] == 0, :]
    country_data['type_line'] = '_cleaned'
    uncleaned_c['type_line'] = '_original'
    background_data = uncleaned_c[['date', 'total_doses', 'at_least_one_dose',
       'fully_vaccinated', 'persons_booster_add_dose', 'type_line']]
    background_data1 = background_data[['date', 'total_doses', 'type_line']]
    background_data2 = background_data[['date', 'at_least_one_dose', 'type_line']]
    background_data3 = background_data[['date', 'fully_vaccinated', 'type_line']]
    background_data4 = background_data[['date', 'persons_booster_add_dose', 'type_line']]
    background_data1['type_col'] = 'Total Doses'
    background_data2['type_col'] = 'At Least One Dose'
    background_data3['type_col'] = 'Fully Vaccinated'
    background_data4['type_col'] = 'Persons Booster Add Dose'
    background_data1.columns = ['date', var_to_clean, 'type_line', 'type_col']
    background_data2.columns = ['date', var_to_clean, 'type_line', 'type_col']
    background_data3.columns = ['date', var_to_clean, 'type_line', 'type_col']
    background_data4.columns = ['date', var_to_clean, 'type_line', 'type_col']
    background_data = pd.concat([background_data1, background_data2, background_data3, background_data4])
    background_data = background_data.loc[~(background_data['type_col'] == var_to_clean.replace('_', ' ').title()), :]
    country_data['type_col'] = var_to_clean.replace('_', ' ').title() + ' cleaned'
    uncleaned_c['type_col'] = var_to_clean.replace('_', ' ').title() + ' original'
    plot_data = pd.concat(
        [background_data,
        uncleaned_c[['date', var_to_clean, 'type_line', 'type_col']],
        country_data[['date', var_to_clean, 'type_line', 'type_col']]],
        ignore_index = True)
    plot_data.rename({var_to_clean: 'y'}, axis = 1, inplace = True)
    plot_data['y'] = plot_data['y'].copy()/1000000
    yaxis = var_to_clean.replace('_', ' ').title() + ' (in million)'
    customPalette = sns.color_palette(["#AAAAAA", "#C1C1C1", "#D3D3D3", "#6495ED", "#FFA500"])

    changes = list(log.loc[log['iso_code'] == country, 'date'])
    changes.sort()
    uncleaned_c.sort_values(by = ['date'], ascending = False, inplace = True)
    uncleaned_c.reset_index(drop = True, inplace = True)
    min_date = uncleaned_c['date'].min()
    max_date = uncleaned_c['date'].max()
    group_together = False
    count = 0
    y_jump_list = []
    y_to_show = []
    for date_change in range(len(changes)):
        y_original = float(uncleaned_c.loc[uncleaned_c['date'] == changes[date_change], var_to_clean])
        row_low = max(list(uncleaned_c['date']).index(changes[date_change]) - 1, 0)
        row_high = min(list(uncleaned_c['date']).index(changes[date_change]) + 1, len(uncleaned_c['date']) - 1)
        y_cleaned = float((uncleaned_c.loc[row_low, var_to_clean] + uncleaned_c.loc[row_high, var_to_clean])/2)
        y_jump = np.abs(y_original - y_cleaned)
        y_jump_list.append(y_jump)
        y_to_show += [y_original,
            float((uncleaned_c.loc[row_low, var_to_clean])),
            float(uncleaned_c.loc[row_high, var_to_clean])]
        date_to = uncleaned_c.loc[max(list(uncleaned_c['date']).index(changes[date_change]) - 15, 0), 'date']
        if group_together:
            date_from = date_from_grouped
        else:
            date_from = uncleaned_c.loc[min(list(uncleaned_c['date']).index(changes[date_change]) + 16, len(uncleaned_c['date']) - 1), 'date']
        if date_change < len(changes) - 1:
            date_from_next_change = uncleaned_c.loc[min(list(uncleaned_c['date']).index(changes[date_change + 1]) + 16, len(uncleaned_c['date']) - 1), 'date']
            if date_to > (date_from_next_change - datetime.timedelta(days = 2)):
                group_together = True
                date_from_grouped = date_from
            else:
                group_together = False
        else:
            group_together = False
        if not group_together:
            y_jump_max = max(y_jump_list)/1000000
            y_jump_list = []
            count += 1
            plot_data_range = plot_data.loc[plot_data['date'] >= date_from, :].copy()
            plot_data_range = plot_data_range.loc[plot_data['date'] <= date_to, :].copy()

            ## zooming in where possible to give impression of continuous data
            zoom_lower_bound = datetime.timedelta(days = -2)
            zoom_upper_bound = datetime.timedelta(days = -2)
            if min_date + datetime.timedelta(days = 15) < date_from:
                zoom_lower_bound = datetime.timedelta(days = 2)
            if max_date - datetime.timedelta(days = 15) > date_to:
                zoom_upper_bound = datetime.timedelta(days = 2)
            
            plt.clf()
            fig, ax = plt.subplots()
            g = sns.lineplot(
                data = plot_data_range,
                y = 'y',
                x = 'date',
                palette = customPalette,
                hue = 'type_col',
                style = 'type_line',
            ).set(
                title = country + ": Change " + str(count),
                xlabel = None,
                ylabel = yaxis
            )
            handles, labels = ax.get_legend_handles_labels()
            ax.legend(handles=handles[1:6], labels=labels[1:6], prop={'size': 8})
            ymin, ymax = plt.ylim()
            zoom = True
            if (ymax - ymin > float(y_jump_max) * 80) & zoom:
                y_low = min(y_to_show) / 1000000
                y_high = max(y_to_show) / 1000000
                y_diff = y_high - y_low
                y_high += y_diff * 5
                y_low -= y_diff * 5
                y_low = max(y_low, -0.5)
                y_to_show = []
                if y_low < 0:
                    y_low = y_high / (-10)
                plt.ylim(y_low, y_high)

            plt.xticks(rotation = 25)
            plt.xlim((date_from + zoom_lower_bound, date_to - zoom_upper_bound))
            plt.subplots_adjust(bottom = 0.2, left = 0.15)
            path = 'data/cleaning_log/' + folder + '/cleaning_' + country
            if count > 1:
                path += '_' + str(count)
            plt.savefig(path)


def automized_cleaning(df2, uncleaned_df, var_to_clean, delete_errors):
    """
    This automatized cleaning function loops through all countries to
    1. check whether the total_doses are monotonically increasing over time,
    2. call the recursive "row check" function if there is a decrease in doses,
    3. produce figures of the changes made.
    """
    print(" > Starting the automized cleaning process...")
    print(" > Initializing variables...")
    log = pd.DataFrame({'country': [], 'date': []})
    pd.set_option('mode.chained_assignment', None)
    df2['to_delete_automized_clean'] = 0
    var_to_clean_iloc = df2.columns.get_loc(var_to_clean)
    
    print(" > Looping through all countries to check for decreases in", var_to_clean,"...")
    countries = df2['iso_code'].unique()
    countries = np.sort(countries)
    for country in countries:
        country_data = filter_country_data(df2, country)
        if not monotonic(list(country_data[var_to_clean])):
            while not monotonic(list(country_data[var_to_clean])):
                row = 0
                country_data, df2, log = row_check(country_data, row, df2, log, var_to_clean_iloc)
            printing_log(country_data.copy(), log.copy())
            export_plots_of_changes(df2, uncleaned_df, country, log, var_to_clean, var_to_clean + '/decrease_cleaning')
    print(" > Saving plots of cleaned changes to data/cleaning_log...")
    if delete_errors:
        df2 = df2.loc[df2['to_delete_automized_clean'] == 0, :]
    else:
        df2.loc[df2['to_delete_automized_clean'] == 1, var_to_clean] = None
    df2.drop('to_delete_automized_clean', axis = 1, inplace = True)
    print(" > Saving logged_changes to csv...")
    log.rename({'date': 'deleted dates'}, axis = 1, inplace = True)
    log.sort_values(by=['iso_code', 'deleted dates'], ascending=True)
    log.drop('country', axis = 1, inplace = True)
    log.to_csv('data/cleaning_log/' + var_to_clean + '/decrease_cleaning/logged_changes.csv', index = False)
    return df2


def main(auto_cleaning, throughput_data):
    who, iso_mapping = import_data(throughput_data)
    who = convert_data_types(who)
    df1 = cleaning(who)
    df1 = date_to_date_week(df1)
    df1, df2 = map_iso_codes(df1, iso_mapping)
    if auto_cleaning:
        clean_path(folder = "cleaning_log")
        uncleaned_df = df2.copy()
        df2 = automized_cleaning(df2, uncleaned_df, var_to_clean = 'total_doses', delete_errors = False)
        df2 = automized_cleaning(df2, uncleaned_df, var_to_clean = 'at_least_one_dose', delete_errors = False)
        df2 = automized_cleaning(df2, uncleaned_df, var_to_clean = 'fully_vaccinated', delete_errors = False)
        df2 = automized_cleaning(df2, uncleaned_df, var_to_clean = 'persons_booster_add_dose', delete_errors = False)
    return df2
