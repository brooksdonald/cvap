
load_lw_data <- function(c_vxrate_lastweek) {
    print(" >> Loading last week dataset...")
    #b_vxrate_lw_sum <- data.frame(
     #   read_excel(
      #      "data/input/base_dvr_lastweek.xlsx",
       #     sheet = "data_summary"
      #  )
    #)

    # Last week dataset
    ## Select relevant columns and rename
    print(" >> Selecting last week data...")
    b_vxrate_lw_sum <- select(
      c_vxrate_lastweek,
        c(
            "a_iso",
            "dvr_4wk_td",
            "dvr_4wk_td_lm",
            "adm_fv"
        )
    )
    print(" >> Renaming last week dataset columns...")
    colnames(b_vxrate_lw_sum) <- c(
        "a_iso",
        "dvr_4wk_td_lw",
        "dvr_4wk_td_lw_lm",
        "adm_fv_lw"
    )

  return(b_vxrate_lw_sum)
}

load_lm_data <- function(c_vxrate_lastmonth) {
    print(" >> Loading last month dataset...")
    #b_vxrate_lm_sum <- data.frame(
     #   read_excel(
      #      "data/input/base_dvr_lastmonth.xlsx",
       #     sheet = "data_summary"
        #)
    #)

    print(" >> Selecting relevant columns from last month data...")
    b_vxrate_lm_sum <- select(
      c_vxrate_lastmonth,
        c(
            "a_iso",
            "adm_td",
            "adm_fv",
            "adm_a1d",
            "adm_booster"
        )
    )

    print(" >> Renaming columns...")
    colnames(b_vxrate_lm_sum) <-
    c(
        "a_iso",
        "adm_td_lm",
        "adm_fv_lm",
        "adm_a1d_lm",
        "adm_booster_lm"
    )

    return(b_vxrate_lm_sum) 

}

load_l2m_data <- function(c_vxrate_twomonth) {
    print(" >> Loading last two month dataset...")
    #b_vxrate_2m_sum <- data.frame(
     #   read_excel(
      #      "data/input/base_dvr_twomonth.xlsx",
       #     sheet = "data_summary"
       # )
    #)

    # Two month dataset
    ## Select relevant columns and rename
    print(" >> Selecting last 2 month data...")
    b_vxrate_2m_sum <- select(
        c_vxrate_twomonth,
        c(
            "a_iso",
            "adm_td",
            "adm_fv",
            "adm_a1d"
        )
    )

    print(" >> Renaming columns...")
    colnames(b_vxrate_2m_sum) <- c(
        "a_iso",
        "adm_td_2m",
        "adm_fv_2m",
        "adm_a1d_2m"
    )

    return(b_vxrate_2m_sum)
}

transform_lw_data <- function(b_vxrate_lw_sum, c_vxrate_latest) {
    print(" >> Transform last week data...")

    tags <- c("1) < (-25)%", "2) (-25)-0%", "3) 0-25%", "4) > 25%")
    b_vxrate_change_lw <- b_vxrate_lw_sum %>%
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
    b_vxrate_change_lw <-
        select(
            b_vxrate_change_lw,
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

merge_with_summary <- function(c_vxrate_latest, b_vxrate) {
    print(" >> Transform historical data...")
    ## Merge with current summary dataset
    c_vxrate_latest <-
    left_join(c_vxrate_latest, b_vxrate, by = "a_iso")
    return(c_vxrate_latest)
}