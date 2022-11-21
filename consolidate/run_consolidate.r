
run_consolidate <- function(a_data, a_data_amc, a_data_africa,
    a_data_csc, a_data_ifc, b_vxrate_change_lw, refresh_date) {
    print(" > Starting local environment for consolidation summary...")
    source("consolidate/consolidate_filter.r")
    source("consolidate/consolidate_adm.r")
    source("consolidate/consolidate_covtar.r")
    source("consolidate/consolidate_covcom_amc.r")
    source("consolidate/consolidate_covcom_csc.r")
    source("consolidate/consolidate_supply_util.r")
    source("consolidate/consolidate_values.r")
    source("consolidate/consolidate_change.r")
    source("consolidate/consolidate_last_month.r")

    print(" > Consolidating base file...")
    condense_list <- load_base_condense_file(a_data)
    print(" > Done.")

    print(" > Consolidating vaccination rate...")
    vrcat_list <- vxrate(condense_list)
    print(" > Done.")

    print(" > Consolidating targets...")
    tgt_list <- targets(condense_list)
    print(" > Done.")
    
    print(" > Consolidating comparisons for AMC participants...")
    com_list_amc <- comparisons_amc(condense_list)
    print(" > Done.")
    
    print(" > Consolidating comparisons for CS countries...")
    com_list_csc <- comparisons_csc(condense_list)
    print(" > Done.")
    
    print(" > Consolidating Supplies & Product utilization...")
    supp_list <- supplies_cons(condense_list)
    print(" > Done.")

    print(" > Creating values table...")
    z_values <- values_table(a_data, a_data_amc, a_data_africa,
        a_data_csc, a_data_ifc, refresh_date)
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
    
    print(" > Coverage category change Africa...")
    base_data_lm_change <- transform_last_month_data(last_month_env$base_data_lm)
    print(" > Done.")
    
    print(" > Loading eda consolidation data back to global environment...") 
    e_vrcat_all <- vrcat_list[["all"]]
    e_trend_all <- vrcat_list[["trend"]]
    e_booster_all <- vrcat_list[["booster"]]
    e_booster_hcw <- vrcat_list[["booster_hcw"]]
    e_tar_past_all <- tgt_list[["tenperc"]]
    e_tar_cur_all <- tgt_list[["seventyperc"]]
    e_tar_cur_scale_all <- tgt_list[["scale"]]
    e_ndvp_all <- tgt_list[["ndvp"]]
    e_secdelpu_all <- supp_list[["all"]]
    e_cov_all <- supp_list[["coverage"]]
    e_cov_com_hcw_all <- com_list_amc[["com_hcw"]]
    e_cov_com_60p_all <- com_list_amc[["com_60p"]]
    e_cov_com_hcw_csc <- com_list_csc[["com_hcw"]]
    e_cov_com_60p_csc <- com_list_csc[["com_60p"]]
    print(" > Done.")
    
    print(" > Returning to global environment.")
    return(environment())
}
