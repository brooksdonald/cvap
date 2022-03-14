# Write to Excel

write_to_excel <- function() {
    all_df <- list(
        "0_base_data" = a_data,
        "1_absorption_month" = d_absorption,
        "1_adm_dvr_long" = b_vxrate_amc,
        "1_adm_all_long" = b_vxrate_pub,
        "1_delivery_doses" = supply_received_doses,
        "2_base_data_amc" = a_data_amc,
        "2_base_data_hic" = a_data_hic,
        "2_dvr_perchange_count" = f_dvr_change_count,
        "2_cov_change_count" = f_cov_change_count,
        "8_dvr_cat" = e_vrcat_all,
        "8_dvr_lm_trend" = e_trend_all,
        "8_tarpast_cat" = e_tar_past_all,
        "8_curtar_cat" = e_tar_cur_all,
        "8_curtar_scale_cat" = e_tar_cur_scale_all,
        "8_booster_status" = e_booster_all,
        "8_secdelpu_cat" = e_secdelpu_all,
        "8_cov_cat" = e_cov_all,
        "8_scaleup_cat" = e_tar_cur_scale_all,
        "8_ndvp_tar_cat" = e_ndvp_all,
        "9_values" = z_values
    )
    write.xlsx(all_df, "data/output/demo_master.xlsx")


}
