# rows 1723 - 1972

run_eda_adm_cov <- function(
    adm_dvr_latest,
    entity_characteristics,
    population,
    uptake_gender_data,
    who_dashboard,
    sup_rec,
    b_dp,
    sup_rec_dose_prod,
    b_fin_fund_del_sum,
    date_refresh,
    target_hcwold,
    combined_three,
    overall_fin_cumul_long,
    adm_all_long,
    population_pin
) {
    source("eda/adm_cov/adm_consolidate.r")
    source("eda/adm_cov/adm_timeseries.r")
    # source("eda/adm_cov/adm_elig_booster.r")
  
    print(" > Starting local environment for vxrate...")

    print(" >  Extracting consolidated vxrate summary...")
    # c_vxrate_latest_red <- extract_vxrate_details(c_vxrate_latest)
    print(" > Done.")

    print(" > Merging dataframes...")
    a_data <- merge_dataframes(
        entity_env$entity_characteristics,
        admcov_env$adm_dvr_latest,
        entity_env$population,
        cov_disag_env$uptake_gender_data,
        add_data_env$who_dashboard,
        supply_env$sup_rec,
        demand_plan_env$b_dp,
        supply_env$sup_rec_jj,
        finance_env$b_fin_fund_del_sum,
        pin_env$population_pin
    )
    print(" > Done.")

    print(" > Calculating merged data")
    datalist <- transform_vxrate_merge(a_data, date_refresh)
    a_data <- datalist$a_data
    print(" > Done.")
    
    timeseries <- merge_timeseries(a_data, combined_three, target_hcwold, overall_fin_cumul_long)
    
    return(environment())
}