# rows 119 - 596

run_vaccines <- function(entity_characteristics,
    refresh_date) {
    source("src/vaccines/vaccines_daily_vxrate.r")
    source("src/vaccines/vaccines_monthly_vxrate.r")
    source("src/entity/entity_characteristics.r")

    print(" > Starting local environment for vaccinations")

    print(" > Daily current vaccinations")
    b_vxrate <- load_b_vxrate()
    b_vxrate <- transform_current_vxrate(
        b_vxrate, entity_characteristics, refresh_date)
    b_vxrate_pub <- transform_current_vxrate_pub(b_vxrate)
    b_vxrate_amc <- transform_subset_amc(b_vxrate)
    b_vxrate_amc_smooth <- transform_smooth_timeseries(
        b_vxrate_amc, b_vxrate_pub, refresh_date)
    c_vxrate_sept_t10 <- transform_sept21_pop_tgt(b_vxrate)
    c_vxrate_dec_t2040 <- transform_dec21_pop_tgt(b_vxrate)
    c_vxrate_eom <- transform_abspt_by_month(b_vxrate)
    d_absorption <- absorption_sum_by_month(c_vxrate_eom)
    c_vxrate_latest <- latest_sum_table(b_vxrate, c_vxrate_latest)
    datalist1 <- absorption_per_country(c_vxrate_eom)
    d_absorb_red <- datalist1$d_absorb_red
    datalist2 <- first_supplies(d_absorb_red, datalist1$d_absorption_country)
    combined <- datalist2$combined
    d_absorption_country_new <- new_absorption_countries(c_vxrate_eom)
    combined_three <- second_supplies(d_absorption_country_new, combined,
        d_absorb_red, entity_characteristics, datalist2$b_supply_red)
    print(" > Done.")

    print(" > Last week, last month, vaccinations and 13jan data")
    print(" > Last week's data")
    b_vxrate_lw_sum <- load_lw_data()
    datalist3 <- transform_lw_data(b_vxrate_lw_sum, c_vxrate_latest)
    b_vxrate_change_lw <- datalist3$b_vxrate_change_lw
    c_vxrate_latest <- datalist3$c_vxrate_latest
    print(" > Done.")

    print(" > Last month's data")
    b_vxrate_lm_sum <- load_lm_data()
    c_vxrate_latest <- merge_with_summary(c_vxrate_latest, b_vxrate_lm_sum)
    print(" > Done.")


    print(" > Last 2 months data")
    b_vxrate_2m_sum <- load_l2m_data()
    c_vxrate_latest <- merge_with_summary(c_vxrate_latest, b_vxrate_2m_sum)

    print(" > Week of 13 Jan")
    b_vxrate_13jan <- load_13jan_data()
    c_vxrate_latest <- merge_with_summary(c_vxrate_latest, b_vxrate_13jan)

    print(" > Done.")
    return(environment())

}