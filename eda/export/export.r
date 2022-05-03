# Write to Excel

write_to_excel <- function() {
    all_df <- list(
        "0_base_data" = a_data,
        "1_absorption_month" = d_absorption,
        "1_absorption_month_country" = combined,
        "1_stock" = combined_three,
        "1_adm_dvr_long" = b_vxrate_amc,
        "1_adm_long_smoohth" = b_vxrate_amc_smooth,
        "1_adm_all_long" = b_vxrate_pub,
        "1_delivery_doses" = supply_received_by_product,
        "1_secview" = z_temp,
        "1_secview_lm" = z_temp_lm,
        "1_secview_all" = z_secview_long,
        "1_funding_source" = b_fin_fund_del_source,
        "2_dvr_perchange_count" = f_dvr_change_count,
        "2_cov_change_count" = f_cov_change_count,
        "2_dvr_perchange_count_af" = f_dvr_change_count_af,
        "2_cov_change_count_af" = f_cov_change_count_af,
        "8_dvr_cat" = e_vrcat_all,
        "8_dvr_lm_trend" = e_trend_all,
        "8_tarpast_cat" = e_tar_past_all,
        "8_curtar_cat" = e_tar_cur_all,
        "8_curtar_scale_cat" = e_tar_cur_scale_all,
        "8_booster_status" = e_booster_all,
        "8_secdelpu_cat" = e_secdelpu_all,
        "8_cov_cat" = e_cov_all,
        "8_ndvp_tar_cat" = e_ndvp_all,
        "9_values" = z_values
    )
    write_xlsx(all_df, "data/output/220505_output_powerbi.xlsx")
    write_xlsx(api, "data/output/220503_output_api.xlsx")


}
