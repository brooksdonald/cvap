# rows 119 - 596

run_adm_cov <- function(entity_characteristics,
    refresh_date, dvr_data, adm_api, auto_cleaning, refresh_supply_timeseries) {
    source("src/adm_cov/dvr_current.r")
    source("src/adm_cov/dvr_prev.r")
    source("src/adm_cov/supply_timeseries.r")

    path_to_timeseries_data <- "data/input/test/"

    print(" > Starting local environment for vaccinations")

    # receive the current month in "%Y-%m" format
    current_month <- substr(refresh_date, 1, 7)

    print(" > Daily current vaccinations")
    b_vxrate <- load_b_vxrate(dvr_data, adm_api, auto_cleaning)
    b_vxrate <- transform_current_vxrate(
        b_vxrate,
        entity_characteristics,
        refresh_date)
    b_vxrate_pub <- transform_current_vxrate_pub(b_vxrate, auto_cleaning)
    b_vxrate_amc <- transform_subset_amc(b_vxrate)
    c_vxrate_sept_t10 <- transform_sept21_pop_tgt(b_vxrate)
    c_vxrate_dec_t2040 <- transform_dec21_pop_tgt(b_vxrate)
    c_vxrate_jun_t70 <- transform_jun22_pop_tgt(b_vxrate)
    c_vxrate_eom <- transform_abspt_by_month(b_vxrate, current_month)
    d_absorption <- absorption_sum_by_month(c_vxrate_eom, current_month)
    c_vxrate_latest <- latest_sum_table(b_vxrate, c_vxrate_latest)
    c_vxrate_lastweek <- last_week_sum_table(
        b_vxrate,
        c_vxrate_latest,
        c_vxrate_lastweek
    )
    c_vxrate_lastmonth <- last_month_sum_table(
        b_vxrate,
        c_vxrate_latest,
        c_vxrate_lastmonth
    )
    c_vxrate_twomonth <- two_month_sum_table(
        b_vxrate,
        c_vxrate_latest,
        c_vxrate_twomonth
    )
    datalist1 <- absorption_per_country(
        c_vxrate_eom,
        current_month
    )
    d_absorb_red <- datalist1$d_absorb_red
    d_absorption_country_new <- new_absorption_countries(
        c_vxrate_eom,
        current_month
    )
    print(" > Done.")

    if (refresh_supply_timeseries) {

        print(" > Supply timeseries")
        sec_overall_long <- load_secured_expected(
            path_to_timeseries_data
        )
        overall_cumul_long <- load_supply_received(
            path_to_timeseries_data
        )
        overall_long <- transform_monthly_supply_received(
            overall_cumul_long
        )
        admin_red <- load_administration(
            d_absorption_country_new,
            entity_characteristics
        )
        export_supply_xlsx(
            sec_overall_long,
            overall_long,
            overall_cumul_long,
            admin_red
        )
        print(" > Done. Exported to data/input/interim/supply.xlsx")

    } else {

        print(" > Importing supply timeseries from data/input/interim/supply.xlsx")
        overall_cumul_long <- load_cum_from_xlsx()
        overall_long <- load_monthly_from_xlsx()
        print(" > Done.")

    }

    datalist2 <- first_supplies(
        d_absorb_red,
        datalist1$d_absorption_country,
        overall_long
    )
    combined <- datalist2$combined
    combined_three <- second_supplies(
        d_absorption_country_new,
        combined,
        d_absorb_red,
        entity_characteristics,
        datalist2$b_supply_red,
        overall_cumul_long
    )
    print(" > Done.")

    print(" > Last week, last month, vaccinations and 13jan data")
    print(" > Last week's data")
    b_vxrate_lw_sum <- load_lw_data(c_vxrate_lastweek)
    datalist3 <- transform_lw_data(b_vxrate_lw_sum, c_vxrate_latest)
    b_vxrate_change_lw <- datalist3$b_vxrate_change_lw
    c_vxrate_latest <- datalist3$c_vxrate_latest
    print(" > Done.")

    print(" > Last month's data")
    b_vxrate_lm_sum <- load_lm_data(c_vxrate_lastmonth)
    c_vxrate_latest <- merge_with_summary(c_vxrate_latest, b_vxrate_lm_sum)
    print(" > Done.")

    print(" > Last 2 months data")
    b_vxrate_2m_sum <- load_l2m_data(c_vxrate_twomonth)
    c_vxrate_latest <- merge_with_summary(c_vxrate_latest, b_vxrate_2m_sum) 

    print(" > Week of 13 Jan")
    b_vxrate_13jan <- load_13jan_data()
    c_vxrate_latest <- merge_with_summary(c_vxrate_latest, b_vxrate_13jan)

    print(" > Done.")
    return(environment())
}