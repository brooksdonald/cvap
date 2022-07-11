# rows 2698 - 3280

run_consolidate <- function(a_data, a_data_amc, a_data_africa,
    a_data_csc, a_data_ifc, b_vxrate_change_lw) {
    source("data/interim/consolidate/consolidate_base_file.r")
    source("data/interim/consolidate/consolidate_vxrate.r")
    source("data/interim/consolidate/consolidate_targets.r")
    source("data/interim/consolidate/consolidate_supplies_utilization.r")
    source("data/interim/consolidate/consolidate_tables.r")

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
    z_values <- values_table(a_data, a_data_amc, a_data_africa,
        a_data_csc, a_data_ifc)
    print(" > Done.")

    print(" > Change count tables, daily vxrate % change category...")
    datalist <- vxrate_change_cat(a_data, b_vxrate_change_lw)
    f_dvr_change_count <- datalist$f_dvr_change_count
    f_dvr_change_count_af <- datalist$f_dvr_change_count_af
    print(" > Done.")

    print(" > DVR change: Africa...")
    f_dvr_change_count_af <- dvr_change_af(f_dvr_change_count_af)
    print(" > Done.")

    print(" > Coverage category change...")
    f_cov_change_count <- cov_cat_change(a_data)
    print(" > Done.")

    print(" > Coverage category change Africa...")
    f_cov_change_count_af <- cov_cat_af(a_data)
    print(" > Done.")
    
    print(" > Loading eda consolidation data back to global environment...") 
    e_vrcat_all <- vrcat_list[["all"]]
    e_trend_all <- vrcat_list[["trend"]]
    e_booster_all <- vrcat_list[["booster"]]
    e_tar_past_all <- tgt_list[["tenperc"]]
    e_tar_cur_all <- tgt_list[["seventyperc"]]
    e_tar_cur_scale_all <- tgt_list[["scale"]]
    e_ndvp_all <- tgt_list[["ndvp"]]
    e_secdelpu_all <- supp_list[["all"]]
    e_cov_all <- supp_list[["coverage"]]
    print(" > Ok.")
    return(environment())
}