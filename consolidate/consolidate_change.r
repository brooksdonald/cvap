
vxrate_change_cat <- function(a_data, b_vxrate_change_lw) {
    print(" >> Generating supplies lists for AMC participants, Africa, America, and Eastern Meditteranean region...")
  
    print(" >> Selecting relevant columns...")
    c_data_dvr_lm_cat <- select(
      a_data, c(
        "a_iso",
        "dvr_4wk_td_change_lm_per_cat",
        "a_covax_status",
        "intro_status",
        "a_continent"
        )
      )
    
    print(" >> Joining data...")
    f_dvr_change_count <- left_join(c_data_dvr_lm_cat, b_vxrate_change_lw, by = "a_iso")
    
    print(" >> Filtering data for African region...")
    f_dvr_change_count_af <- filter(
        f_dvr_change_count,
        a_continent == "Africa" &
        intro_status == "Product introduced"
    )
    
    print(" >> Filtering data for AMC participants...")
    f_dvr_change_count <- filter(
        f_dvr_change_count,
        a_covax_status == "AMC" &
        intro_status == "Product introduced"
    )
    
    print(" >> Selecting relevant columns...")
    f_dvr_change_count <- select(
      f_dvr_change_count, c(
        "a_iso",
        "dvr_4wk_td_change_lm_per_cat",
        "dvr_4wk_td_change_lw_lm_per_cat"
        )
      )
    
    print(" >> Renaming columns...")
    colnames(f_dvr_change_count) <- c(
      "a_iso",
      "dvr_change_cat_lm_cur",
      "dvr_change_cat_lm_lw"
      )

    print(" >> Summarizing current week daily vaccination rate data...")
    f_dvr_change_count_cur <- f_dvr_change_count %>%
      group_by(dvr_change_cat_lm_cur) %>%
      summarise(count_cur = n())
      colnames(f_dvr_change_count_cur) <- c(
        "cat",
        "count_cur"
        )
      
      print(" >> Summarizing last week daily vaccination rate data...")
      f_dvr_change_count_lw <- f_dvr_change_count %>%
        group_by(dvr_change_cat_lm_lw) %>%
      summarise(count_lw = n())
      colnames(f_dvr_change_count_lw) <- c(
        "cat",
        "count_lw"
        )
      
    print(" >> Joining current and last week data...")
    f_dvr_change_count <- left_join(
      f_dvr_change_count_cur, f_dvr_change_count_lw, by = "cat")
    
    print(" >> Generating change since last week variable...")
    f_dvr_change_count <- f_dvr_change_count %>%
      mutate(count_change = count_cur - count_lw)
    
    print(" >> Adding data to datalist...")
    datalist <- list("f_dvr_change_count" = f_dvr_change_count,
                     "f_dvr_change_count_af" = f_dvr_change_count_af)
    
    print(" >> Function 'dvr_change_af' done")
    return(datalist)
}

dvr_change_af <- function(f_dvr_change_count_af) {
  print(" >> Generating daily vaccination rate change data...")  
  print(" >> Selecting relevant columns...")
  f_dvr_change_count_af <-
    select(
        f_dvr_change_count_af,
        c(
            "a_iso",
            "dvr_4wk_td_change_lm_per_cat",
            "dvr_4wk_td_change_lw_lm_per_cat"
        )
    )
  
  print(" >> Renaming columns...")
  colnames(f_dvr_change_count_af) <- c(
        "a_iso",
        "dvr_change_cat_lm_cur",
        "dvr_change_cat_lm_lw"
    )
  
  print(" >> Summarizing current week daily vaccination rate data...")
  f_dvr_change_count_cur_af <- f_dvr_change_count_af %>%
    group_by(dvr_change_cat_lm_cur) %>%
    summarise(count_cur = n())
    colnames(f_dvr_change_count_cur_af) <- c("cat", "count_cur")

    print(" >> Summarizing last week daily vaccination rate data...")
    f_dvr_change_count_lw_af <- f_dvr_change_count_af %>%
    group_by(dvr_change_cat_lm_lw) %>%
    summarise(count_lw = n())
    colnames(f_dvr_change_count_lw_af) <- c("cat", "count_lw")

    print(" >> Joining current and last week data...")
    f_dvr_change_count_af <-
    left_join(f_dvr_change_count_cur_af, f_dvr_change_count_lw_af, by = "cat")

    print(" >> Generating change since last week variable...")
    f_dvr_change_count_af <- f_dvr_change_count_af %>%
    mutate(count_change = count_cur - count_lw)
    
    print(" >> Function 'dvr_change_af' done")
    return(f_dvr_change_count_af)
}

cov_cat_change <- function(a_data) {
  print(" >> Generating coverage category change data...")  
  
  print(" >> Filtering data...")  
  f_cov_change_count <-
    filter(
        a_data,
        a_covax_status == "AMC" &
        intro_status == "Product introduced"
    )

    print(" >> Selecting relevant columns...")
    f_cov_change_count <- select(
      f_cov_change_count,
      c(
        "a_iso",
        "cov_total_fv_cat",
        "cov_total_fv_lw_cat"
      )
    )

    print(" >> Summarizing current week coverage category data...")
    f_cov_change_count_cur <- f_cov_change_count %>%
        group_by(cov_total_fv_cat) %>%
        summarise(count_cur = n())
    
    print(" >> Renaming columns...")
    colnames(f_cov_change_count_cur) <- c(
            "cat",
            "count_cur"
        )

    print(" >> Summarizing last week coverage category data...")
    f_cov_change_count_lw <- f_cov_change_count %>%
        group_by(cov_total_fv_lw_cat) %>%
        summarise(count_lw = n())
        colnames(f_cov_change_count_lw) <- c("cat", "count_lw")

    print(" >> Joining current and last week data...")
    f_cov_change_count <-
      left_join(f_cov_change_count_cur, f_cov_change_count_lw, by = "cat")

    print(" >> Generating change since last week variable...")
    f_cov_change_count <- f_cov_change_count %>%
    mutate(count_change = count_cur - count_lw)

    print(" >> Function 'cov_cat_change' done")
    return(f_cov_change_count)
}

cov_cat_af <- function(a_data) {
  print(" >> Generating coverage category change data (Africa region)...")  
  
  print(" >> Filtering data...")  
  f_cov_change_count_af <-
    filter(
        a_data,
        a_continent == "Africa" &
        intro_status == "Product introduced"
    )
    
    print(" >> Selecting relevant columnds...")
    f_cov_change_count_af <-
    select(
        f_cov_change_count_af,
        c(
            "a_iso",
            "cov_total_fv_cat",
            "cov_total_fv_lw_cat"
        )
    )
    
    print(" >> Summarizing current week coverage category data...")
    f_cov_change_count_cur_af <- f_cov_change_count_af %>%
    group_by(cov_total_fv_cat) %>%
    summarise(count_cur = n())
    colnames(f_cov_change_count_cur_af) <- c("cat", "count_cur")

    print(" >> Summarizing last week coverage category data...")
    f_cov_change_count_lw_af <- f_cov_change_count_af %>%
    group_by(cov_total_fv_lw_cat) %>%
    summarise(count_lw = n())
    colnames(f_cov_change_count_lw_af) <- c("cat", "count_lw")

    print(" >> Joining current and last week data...")
    f_cov_change_count_af <-
    left_join(f_cov_change_count_cur_af, f_cov_change_count_lw_af, by = "cat")

    print(" >> Generating change since last week variable...")
    f_cov_change_count_af <- f_cov_change_count_af %>%
    mutate(count_change = count_cur - count_lw)
    
    print(" >> Function 'cov_cat_af' done")
    return(f_cov_change_count_af)
}
