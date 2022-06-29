# rows 1723 - 1972

run_vxrate <- function(
    c_vxrate_latest,
    entity_characteristics,
    population_data,
    uptake_gender_data,
    b_who_dashboard,
    b_smartsheet,
    supply_secured,
    delivery_courses_doses,
    b_dp,
    c_delivery_product,
    b_fin_fund_del_sum,
    env = .GlobalEnv
) {
    source("eda/vxrate/vxrate_consolidate.r")

    print(" > Starting local environment for vxrate...")

    print(" >  Extracting consolidated vxrate summary...")
    c_vxrate_latest_red <- extract_vxrate_details(c_vxrate_latest)
    print(" > Done.")

    print(" > Obtaining WHO Dashboard...")
    b_who_dashboard <- load_who_dashboard()
    print(" > Done.")

    print(" > Merging dataframes...")
    a_data <- merge_dataframes(
        entity_characteristics,
        c_vxrate_latest_red,
        population_data,
        uptake_gender_data,
        b_who_dashboard,
        b_smartsheet,
        supply_secured,
        delivery_courses_doses,
        b_dp,
        c_delivery_product,
        b_fin_fund_del_sum
    )
    print(" > Done.")

    print(" > Calculating merged data")
    a_data <- transform_vxrate_merge(a_data)
    print(" > Done.")

    return(environment())
}
