# rows 2698 - 3280

run_consolidate <- function(a_data, env = .GlobalEnv) {
    source("consolidate/consolidate_base_file.r")
    source("consolidate/consolidate_vxrate.r")
    source("consolidate/consolidate_targets.r")
    source("consolidate/consolidate_supplies_utilization.r")
    source("consolidate/consolidate_tables.r")

    print(" > Starting local environment for consolidation summary")
    print(" > Consolidating base file...")
    condense_list <- load_base_condense_file(a_data)
    print(" > Done.")

    print(" > Consolidating Vaccination rate...")
    vrcat_list <- vxrate(condense_list)
    print(" > Done.")

    print(" > Consolidating targets...")
    tgt_list <- targets(condense_list)
    print(" > Done.")

    print(" > Consolidating Supplies & Product utilization...")
    supp_list <- supplies_cons(condense_list)
    print(" > Done.")

    print(" > Creating values table...")
    z_values <- values_table()
    print(" > Done.")

    print(" > Change count tables, daily vxrate % change category...")
    f_dvr_change_count <- vxrate_change_cat(a_data, b_vxrate_change_lw)
    print(" > Done.")

    print(" > DVR change: Africa...")
    f_dvr_change_count_af <- dvr_change_af()
    print(" > Done.")

    print(" > Coverage category change...")
    f_cov_change_count <- cov_cat_change(a_data)
    print(" > Done.")

    print(" > Coverage category change Africa...")
    f_cov_change_count_af <- cov_cat_af(a_data)
    print(" > Done.")
    
    print(" > Loading eda consolidation data back to global environment...") 
    env$vrcat_list <- vrcat_list
    env$e_vrcat_all <- vrcat_list[["all"]]
    env$e_trend_all <- vrcat_list[["trend"]]
    env$e_booster_all <- vrcat_list[["booster"]]
    env$tgt_list <- tgt_list
    env$e_tar_past_all <- tgt_list[["tenperc"]]
    env$e_tar_cur_all <- tgt_list[["seventyperc"]]
    env$e_tar_cur_scale_all <- tgt_list[["scale"]]
    env$e_ndvp_all <- tgt_list[["ndvp"]]
    env$supp_list <- supp_list
    env$e_secdelpu_all <- supp_list[["all"]]
    env$e_cov_all <- supp_list[["coverage"]]
    env$z_values <- z_values
    env$f_dvr_change_count <- f_dvr_change_count
    env$f_cov_change_count <- f_cov_change_count
    env$f_dvr_change_count_af <- f_dvr_change_count_af
    env$f_cov_change_count_af <- f_cov_change_count_af
    print(" > Ok.")
    return(environment())
}
