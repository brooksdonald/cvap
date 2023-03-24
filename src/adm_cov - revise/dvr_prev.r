
load_lw_data <- function(latest_sum_table) {
  print(" >> Loading last week dataset...")
  
  print(" >> Selecting relevant columns...")
  lw_data <- select(
    latest_sum_table, c(
      "a_iso",
      "dvr_4wk_td",
      "dvr_4wk_td_lm",
      "adm_fv"
      )
    )
  
  print(" >> Renaming last week dataset columns...")
  colnames(lw_data) <- c(
      "a_iso",
      "dvr_4wk_td_lw",
      "dvr_4wk_td_lw_lm",
      "adm_fv_lw"
  )

  print(" >> Function 'load_lw_data' done")
  return(lw_data)
}

load_lm_data <- function(last_month_sum_table) {
  print(" >> Loading last month dataset...")

  print(" >> Selecting relevant columns...")
  lm_data <- select(
    last_month_sum_table, c(
      "a_iso",
      "adm_td",
      "adm_fv",
      "adm_a1d",
      "adm_booster"
      )
    )

  print(" >> Renaming columns...")
  colnames(lm_data) <- c(
    "a_iso",
    "adm_td_lm",
    "adm_fv_lm",
    "adm_a1d_lm",
    "adm_booster_lm"
    )

  print(" >> Function 'load_lm_data' done")
  return(lm_data) 
}

load_l2m_data <- function(two_month_sum_table) {
  print(" >> Loading last two month dataset...")
  
  print(" >> Selecting relevant columns...")
  l2m_data <- select(
    two_month_sum_table, c(
      "a_iso",
      "adm_td",
      "adm_fv",
      "adm_a1d"
      )
    )

  print(" >> Renaming columns...")
  colnames(l2m_data) <- c(
    "a_iso",
    "adm_td_2m",
    "adm_fv_2m",
    "adm_a1d_2m"
    )

  print(" >> Function 'load_l2m_data' done")
  return(l2m_data)
}

transform_lw_data <- function(last_week_sum_table, latest_sum_table) {
  print(" >> Transform last week data...")

  tags <- c("1) < (-25)%", "2) (-25)-0%", "3) 0-25%", "4) > 25%")
  last_week_sum_table_temp <- last_week_sum_table %>%
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

  print(" >> Selecting relevant coluns for category count change table...")
  last_week_sum_table_temp <-
    select(
      last_week_sum_table_temp,
      "a_iso",
      "dvr_4wk_td_change_lw_lm_per_cat"
      )

  print(" >> Selecting relevant coluns for coverage category count change table...")
  last_week_sum_table_cov <- select(last_week_sum_table, "a_iso", "adm_fv_lw")

  latest_sum_table <- merge_with_summary(latest_sum_table, last_week_sum_table_cov)
  datalist <- list("c_vxrate_latest" = latest_sum_table,
                   "b_vxrate_change_lw" = last_week_sum_table_temp)
  
  print(" >> Function 'transform_lw_data' done")
  return(datalist)
}

merge_with_summary <- function(latest_sum_table, vxrate_data) {
  print(" >> Join historical data...")
  latest_sum_table <- left_join(latest_sum_table, vxrate_data, by = "a_iso")
  
  print(" >> Function 'merge_with_summary' done")
  return(latest_sum_table)
}
