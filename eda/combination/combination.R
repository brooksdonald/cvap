supply_admin_summary <- function(a_data) {
  # Create supply summary table
  z_temp_sec <- select(a_data, c("a_iso","a_covax_status","a_csl_status","a_ifc_status","a_name_short","sec_total_dose"))
  colnames(z_temp_sec) <- c("a_iso", "a_covax_status","a_csl_status","a_ifc_status","a_name_short", "doses")
  z_temp_sec$category <- "Secured"
  
  z_temp_secdel <- select(a_data, c("a_iso","a_covax_status","a_csl_status","a_ifc_status","a_name_short","sec_tobedel_dose"))
  colnames(z_temp_secdel) <- c("a_iso", "a_covax_status","a_csl_status","a_ifc_status","a_name_short", "doses")
  z_temp_secdel$category <- "4) Secured, not yet received"
  
  z_temp_secdel_wast <- select(a_data, c("a_iso","a_covax_status","a_csl_status","a_ifc_status","a_name_short","sec_tobedel_dose"))
  colnames(z_temp_secdel_wast) <- c("a_iso", "a_covax_status","a_csl_status","a_ifc_status","a_name_short", "doses")
  z_temp_secdel_wast$category <- "3) Secured, not yet received"
  
  z_temp_del <- select(a_data, c("a_iso","a_covax_status","a_csl_status","a_ifc_status","a_name_short","del_dose_total"))
  colnames(z_temp_del) <- c("a_iso", "a_covax_status","a_csl_status","a_ifc_status","a_name_short", "doses")
  z_temp_del$category <- "Received"
  
  z_temp_deladm <- select(a_data, c("a_iso","a_covax_status","a_csl_status","a_ifc_status","a_name_short","pu_del_rem"))
  colnames(z_temp_deladm) <- c("a_iso", "a_covax_status","a_csl_status","a_ifc_status","a_name_short", "doses")
  z_temp_deladm$category <- "3) Received, not yet utilized"
  
  z_temp_deladmwast <- select(a_data, c("a_iso","a_covax_status","a_csl_status","a_ifc_status","a_name_short","pu_del_rem_wast"))
  colnames(z_temp_deladmwast) <- c("a_iso", "a_covax_status","a_csl_status","a_ifc_status","a_name_short", "doses")
  z_temp_deladmwast$category <- "2) Received, not yet utilized"
  
  z_temp_wast <- select(a_data, c("a_iso","a_covax_status","a_csl_status","a_ifc_status","a_name_short","del_dose_wast"))
  colnames(z_temp_wast) <- c("a_iso", "a_covax_status","a_csl_status","a_ifc_status","a_name_short", "doses")
  z_temp_wast$category <- "2) Assumed wastage"
  
  z_temp_adm <- select(a_data, c("a_iso","a_covax_status","a_csl_status","a_ifc_status","a_name_short","adm_td"))
  colnames(z_temp_adm) <- c("a_iso", "a_covax_status","a_csl_status","a_ifc_status","a_name_short", "doses")
  z_temp_adm$category <- "1) Administered"
  
  z_temp <- bind_rows(z_temp_sec, z_temp_secdel) %>%
    bind_rows(., z_temp_del) %>%
    bind_rows(., z_temp_secdel_wast) %>%
    bind_rows(., z_temp_deladm) %>%
    bind_rows(., z_temp_deladmwast) %>%  
    bind_rows(., z_temp_wast) %>%
    bind_rows(., z_temp_adm)
  
  # Last month
  
  z_temp_sec_lm <- select(a_data, c("a_iso","a_covax_status","a_csl_status","a_ifc_status","a_name_short","sec_total_dose_lm"))
  colnames(z_temp_sec_lm) <- c("a_iso", "a_covax_status","a_csl_status","a_ifc_status","a_name_short", "doses")
  z_temp_sec_lm$category <- "Secured"
  
  z_temp_secdel_lm <- select(a_data, c("a_iso","a_covax_status","a_csl_status","a_ifc_status","a_name_short","sec_tobedel_dose_lm"))
  colnames(z_temp_secdel_lm) <- c("a_iso", "a_covax_status","a_csl_status","a_ifc_status","a_name_short", "doses")
  z_temp_secdel_lm$category <- "4) Secured, not yet received"
  
  z_temp_secdel_wast_lm <- select(a_data, c("a_iso","a_covax_status","a_csl_status","a_ifc_status","a_name_short","sec_tobedel_dose_lm"))
  colnames(z_temp_secdel_wast_lm) <- c("a_iso", "a_covax_status","a_csl_status","a_ifc_status","a_name_short", "doses")
  z_temp_secdel_wast_lm$category <- "3) Secured, not yet received"
  
  z_temp_del_lm <- select(a_data, c("a_iso","a_covax_status","a_csl_status","a_ifc_status","a_name_short","del_dose_total_lm"))
  colnames(z_temp_del_lm) <- c("a_iso", "a_covax_status","a_csl_status","a_ifc_status","a_name_short", "doses")
  z_temp_del_lm$category <- "Received"
  
  z_temp_deladmwast_lm <- select(a_data, c("a_iso","a_covax_status","a_csl_status","a_ifc_status","a_name_short","pu_del_rem_wast_lm"))
  colnames(z_temp_deladmwast_lm) <- c("a_iso", "a_covax_status","a_csl_status","a_ifc_status","a_name_short", "doses")
  z_temp_deladmwast_lm$category <- "2) Received, not yet utilized"
  
  z_temp_adm_lm <- select(a_data, c("a_iso","a_covax_status","a_csl_status","a_ifc_status","a_name_short","adm_td_lm"))
  colnames(z_temp_adm_lm) <- c("a_iso", "a_covax_status","a_csl_status","a_ifc_status","a_name_short", "doses")
  z_temp_adm_lm$category <- "1) Administered"
  
  z_temp_lm <- bind_rows(z_temp_sec_lm, z_temp_secdel_lm) %>%
    bind_rows(., z_temp_del_lm) %>%
    bind_rows(., z_temp_secdel_wast_lm) %>%
    bind_rows(., z_temp_deladmwast_lm) %>%  
    bind_rows(., z_temp_adm_lm)
  
  z_temp_lm_long <- z_temp_lm
  z_temp_lm_long$type <- "Last month total"
  
  z_temp_lm_short <- select(z_temp_lm, c("a_iso", "doses","category"))
  colnames(z_temp_lm_short) <- c("a_iso","doses_lm", "category")
  
  # Combine this month and last month
  
  z_secview <- left_join(z_temp, z_temp_lm_short, by = c("a_iso", "category"))
  
  z_secview <- z_secview %>%
    mutate(change = doses - doses_lm)
  
  z_secview_long <- select(z_secview, -c("doses","doses_lm"))
  z_secview_long$type <- "Change since last month"
  colnames(z_secview_long) <- c("a_iso","a_covax_status","a_csl_status","a_ifc_status","a_name_short","category","doses","type")
  
  z_secview_long <- bind_rows(z_secview_long, z_temp_lm_long)
  
  return(a_data)
  
}