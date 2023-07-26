load_admdvr_lw <- function(adm_dvr) {
  print(">> Preparing last week subset from timeseries...")
  adm_dvr_lw <- adm_dvr %>%
    filter(adm_last_week == "Yes") %>%
    select(a_iso,
           dvr_4wk_td,
           dvr_4wk_td_lm,
           adm_fv) %>%
    rename(dvr_4wk_td_lw = dvr_4wk_td,
           dvr_4wk_td_lw_lm = dvr_4wk_td_lm,
           adm_fv_lw = adm_fv)
  
  print(">> Done.")
  return(adm_dvr_lw)
}

load_admdvr_lm <- function(adm_dvr) {
  print(">> Preparing last month subset from timeseries...")
  adm_dvr_lm <- adm_dvr %>%
    filter(adm_last_month == "Yes") %>%
    select(a_iso,
           adm_td,
           adm_a1d,
           adm_fv,
           adm_booster) %>%
    rename(adm_td_lm = adm_td,
           adm_a1d_lm = adm_a1d,
           adm_fv_lm = adm_fv,
           adm_booster_lm = adm_booster)

  print(">> Done.")
  return(adm_dvr_lm) 
}

load_admdvr_2m <- function(adm_dvr) {
  print(">> Preparing two month subset from timeseries...")
  adm_dvr_2m <- adm_dvr %>%
    filter(adm_two_month == "Yes") %>%
    select(a_iso,
           adm_td,
           adm_a1d,
           adm_fv) %>%
    rename(adm_td_2m = adm_td,
           adm_a1d_2m = adm_a1d,
           adm_fv_2m = adm_fv)
  
  print(">> Done.")
  return(adm_dvr_2m)
}

transform_admdvr_lw <- function(adm_dvr_lw, adm_dvr_latest) {
    print(" >> Transform last week data...")

    tags <- c("1) < (-25)%", "2) (-25)-0%", "3) 0-25%", "4) > 25%")
    adm_dvr_lw_change <- adm_dvr_lw %>%
        mutate(dvr_4wk_td_change_lw_lm = dvr_4wk_td_lw - dvr_4wk_td_lw_lm) %>%
        mutate(dvr_4wk_td_change_lw_lm_per =
            dvr_4wk_td_change_lw_lm / dvr_4wk_td_lw_lm) %>%
        mutate(dvr_4wk_td_change_lw_lm_per_cat =
            cut(
                dvr_4wk_td_change_lw_lm_per,
                breaks = c(-Inf, -0.25, 0, 0.25, Inf),
                include.lowest = TRUE,
                right = FALSE,
                labels = tags)) %>%
        mutate(dvr_4wk_td_change_lw_lm_per_cat = replace_na(
            dvr_4wk_td_change_lw_lm_per_cat,
            tags[2]))


    ## Select relevant columns for dvr category count change table
    adm_dvr_lw_change <-
        select(
          adm_dvr_lw_change,
            "a_iso",
            "dvr_4wk_td_change_lw_lm_per_cat"
        )

    ## Select relevant columns for coverage category count change table
    b_vxrate_cov <- select(b_vxrate_lw_sum, "a_iso", "adm_fv_lw")

    c_vxrate_latest <- merge_with_summary(c_vxrate_latest, b_vxrate_cov)
    datalist <- list("c_vxrate_latest" = c_vxrate_latest,
        "b_vxrate_change_lw" = b_vxrate_change_lw)
    return(datalist)
}

merge_with_summary <- function(adm_dvr_latest, adm_dvr_lw, adm_dvr_lm, adm_dvr_2m) {
    print(">> Merge historical data with latest dataframe...")
    ## Merge with current summary dataset
    adm_dvr_latest <- left_join(adm_dvr_latest, adm_dvr_lw, by = "a_iso") %>%
      left_join(., adm_dvr_lm, by = "a_iso") %>%
      left_join(., adm_dvr_2m, by = "a_iso")
    
    print(">> Done.")
    return(adm_dvr_latest)
}