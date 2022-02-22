# rows 119 - 596

source("src/vaccines/vaccines_daily_vxrate.r")
source("src/vaccines/vaccines_monthly_vxrate.r")

run_vaccines <- function(entity_characteristics, env = .GlobalEnv) {
    print(" > Starting local environment for vaccinations")

    print(" > Daily current vaccinations")
    b_vxrate <- load_b_vxrate()
    b_vxrate <- transform_current_vxrate(b_vxrate)
    b_vxrate_pub <- transform_current_vxrate_pub(b_vxrate) # TODO I've passed b_vxrate and removed b_vxrate_pub. Should it be?
    b_vxrate_amc <- transform_subset_amc(b_vxrate) # TODO I've passed b_vxrate and removed b_vxrate_amc. Should it be?
    c_vxrate_sept <- transform_sept21_pop_tgt(c_vxrate_sept)
    c_vxrate_dec <- transform_dec21_pop_tgt(c_vxrate_dec)
    c_vxrate_eom <- transform_abspt_by_month(c_vxrate_eom)
    d_absorption <- absorption_sum_by_month(d_absorption)
    c_vxrate_latest <- latest_sum_table(c_vxrate_latest)
    print(" > Done.")

    print(" > Last week, last month and vaccinations")
    print(" > Last week's data")
    b_vxrate_lw_sum <- load_lw_data()
    b_vxrate_lw_sum <- transform_lw_data(b_vxrate_lw_sum)

    print(" > Last month's data")
    b_vxrate_lm_sum <- load_lm_data()
    b_vxrate_lm_sum <- transform_lm_data(b_vxrate_lm_sum)


    print(" > Last 2 months data")
    b_vxrate_2m_sum <- load_l2m_data()
    b_vxrate_2m_sum <- transform_l2m_data(b_vxrate_2m_sum)

    print(" > Done.")

    print(" > Loading vaccines data back to global environment...")
    env$b_vxrate <- b_vxrate
    env$b_vxrate_pub <- b_vxrate_pub
    env$b_vxrate_amc <- b_vxrate_amc
    env$c_vxrate_sept <- c_vxrate_sept
    env$c_vxrate_dec <- c_vxrate_dec
    env$c_vxrate_eom <- c_vxrate_eom
    env$d_absorption <- d_absorption
    env$c_vxrate_latest <- c_vxrate_latest
    env$b_vxrate_lw_sum <- b_vxrate_lw_sum
    env$b_vxrate_lm_sum <- b_vxrate_lm_sum
    env$b_vxrate_2m_sum <- b_vxrate_2m_sum

    return(environment())

}