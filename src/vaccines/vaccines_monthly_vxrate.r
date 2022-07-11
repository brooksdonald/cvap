
load_lw_data <- function(c_vxrate_lastweek) {
    print(" >> Loading last week dataset...")
    #b_vxrate_lw_sum <- data.frame(
     #   read_excel(
      #      "data/_input/base_dvr_lastweek.xlsx",
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
      #      "data/_input/base_dvr_lastmonth.xlsx",
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
      #      "data/_input/base_dvr_twomonth.xlsx",
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

load_13jan_data <- function() {
    print("  >> Loading week of 13 Jan dataset...")
    b_vxrate_13jan <- data.frame(
        read_excel(
            "data/_input/static/base_dvr_weekof13jan.xlsx",
            sheet = "data_summary"
        )
    )
    ## Select relevant columns and rename
    b_vxrate_13jan <- select(
        b_vxrate_13jan,
        c(
            "iso_code",
            "total_doses",
            "fully_vaccinated",
            "at_least_one_dose",
            "persons_booster_add_dose"
        )
    )
    print(" >> Renaming columns...")
    colnames(b_vxrate_13jan) <- c(
        "a_iso",
        "adm_td_13jan",
        "adm_fv_13jan",
        "adm_a1d_13jan",
        "adm_booster_13jan"
    )
    return(b_vxrate_13jan)
}

transform_lw_data <- function(b_vxrate_lw_sum, c_vxrate_latest) {
    print(" >> Transform last week data...")
    ## Calculate percent change and category
    #TODO Refactor this mutate by adding the cut function
    b_vxrate_change_lw <<- b_vxrate_lw_sum %>%
        mutate(dvr_4wk_td_change_lw_lm = dvr_4wk_td_lw - dvr_4wk_td_lw_lm) %>%
            mutate(dvr_4wk_td_change_lw_lm_per = dvr_4wk_td_change_lw_lm / dvr_4wk_td_lw_lm) %>%
                mutate(
                    dvr_4wk_td_change_lw_lm_per_cat = if_else(is.na(dvr_4wk_td_change_lw_lm_per), "2) (-25)-0%",
                      if_else(
                        dvr_4wk_td_change_lw_lm_per <= -0.25,
                        "1) < (-25)%",
                        if_else(
                            dvr_4wk_td_change_lw_lm_per >= 0.25,
                            "4) > 25%",
                            if_else(
                                dvr_4wk_td_change_lw_lm_per <= 0,
                                "2) (-25)-0%",
                                if_else(dvr_4wk_td_change_lw_lm_per > 0, "3) 0-25%",

                                NA_character_
                            )
                        )
                    )
                )
            ))


    ## Select relevant columns for dvr category count change table
    b_vxrate_change_lw <<-
        select(
            b_vxrate_change_lw,
            "a_iso",
            "dvr_4wk_td_change_lw_lm_per_cat"
        )

    ## Select relevant columns for coverage category count change table
    b_vxrate_cov <- select(b_vxrate_lw_sum, "a_iso", "adm_fv_lw")

    c_vxrate_latest <-
    left_join(c_vxrate_latest, b_vxrate_cov, by = "a_iso")

    return(c_vxrate_latest)
}

transform_lm_data <- function(c_vxrate_latest, b_vxrate_lm_sum) {
    print(" >> Transform last month data...")
    ## Merge with current summary dataset
    c_vxrate_latest <-
    left_join(c_vxrate_latest, b_vxrate_lm_sum, by = "a_iso")
    return(c_vxrate_latest)
}

transform_l2m_data <- function(c_vxrate_latest, b_vxrate_2m_sum) {
    print(" >> Transform last two months data...")
    ## Merge with current summary dataset
    c_vxrate_latest <-
    left_join(c_vxrate_latest, b_vxrate_2m_sum, by = "a_iso")
    return(c_vxrate_latest)
}

transform_13jan_data <- function(c_vxrate_latest, b_vxrate_13jan) {
    print(" >> Transform week of the 13 jan...")
    ## Merge with current summary dataset
    c_vxrate_latest <- left_join(c_vxrate_latest, b_vxrate_13jan, by = "a_iso")
    return(c_vxrate_latest)
}