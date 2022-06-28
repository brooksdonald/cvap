import vxrate_data_ingestion, vxrate_supply_data, vxrate_output_daily, vxrate_data_cleaning, vxrate_data_fixes
from vxrate_supply_data import *

def run_vaccination_rate():
    df = import_data()
    df = validate_and_filter(df)
    df_uti = create_date_column(df)
    df_uti = aggregate_util_data(df_uti)
    df_uti = add_timestamp(df_uti)
    export_data(df_uti)
