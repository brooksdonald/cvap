load_secured_expected <- function(path) {
  # Load datasets

  df_list1 <- helper_load_list_of_files(
    c(
      "data/_input/test/211005_imf-who-covid-19-vaccine-supply-tracker.xlsx",
      "data/_input/test/211102_imf-who-covid-19-vaccine-supply-tracker.xlsx",
      "data/_input/test/211201_imf-who-covid-19-vaccine-supply-tracker.xlsx",
      "data/_input/test/220105_IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx"
    ),
    sheets = "data",
    date_format = "%y%m%d"
  )

  df_list2 <- helper_load_list_of_files(
    c(
      "data/_input/test/220202_IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx",
      "data/_input/test/220302_IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx",
      "data/_input/test/220406_IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx",
      "data/_input/test/220505_IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx",
      "data/_input/test/220602_IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx",
      "data/_input/test/220701_IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx"
    ),
    sheets = "supply_tracker",
    date_format = "%y%m%d"
  )
  df_list <- append(df_list1, df_list2)


  ## alternative automatic read in of files

  # reading in list of files in directory
  print(" >> Selecting files to read in...")
  files <- unlist(list.files(
    path = "data/_input/test/",
    # add to this pattern if the file name changes in the future
    pattern = "IMF|imf"
  ))

  # checking for duplicate months
  months <- substr(files, 1, 4)
  if (sum(duplicated(months))) {
    stop(paste0("Error:
      Two files in ",
      path,
      " with 'imf' in the name have the same year month combination."))
  }

  # checking for reasonable years and months
  years <- substr(files, 1, 2)
  months <- substr(files, 3, 4)
  if (
    sum(as.numeric(years) > 40) |
    sum(as.numeric(years) < 21) |
    sum(as.numeric(months) > 12) |
    sum(as.numeric(months) < 1)) {
      stop("One of IMF datasets does not conform to year-month format
        (must be, e.g., 2108 for Aug 2021)")
    }

  # finding the right sheet to read in
  sheet_vector <- c()
  for (file in files) {
    sheets <- excel_sheets(path = paste0(path, file))
    # if sheet names change in the future, simply add to this list
    possible_sheet_names <- c("supply_tracker", "data")
    for (name in possible_sheet_names) {
      if (name %in% sheets) {
        relevant_sheet <- name
      }
    }
    # throw error if there is no sheet with one of the names above
    if (is.na(relevant_sheet)) {
      stop(paste0("Error: ", file, " does not have a sheet with a right name.
        Add new sheet name to the list of
        possible sheet names in supply_timeseries.r"))
    }
    sheet_vector <- append(sheet_vector, relevant_sheet)
    remove(relevant_sheet)
  }

  print(" >> Read in supply_tracker data...")
  df_list <- helper_load_list_of_files(
    files = paste0(path, files),
    sheets = sheet_vector,
    date_format = "%y%m%d"
  )


  # base_sec_eosep <-
  #   data.frame(read_excel("data/_input/test/211005_imf-who-covid-19-vaccine-supply-tracker.xlsx",
  #                         sheet = "data"))

  # base_sec_eooct <-
  #   data.frame(read_excel("data/_input/test/211102_imf-who-covid-19-vaccine-supply-tracker.xlsx",
  #                         sheet = "data"))

  # base_sec_eonov <-
  #   data.frame(read_excel("data/_input/test/211201_imf-who-covid-19-vaccine-supply-tracker.xlsx",
  #                         sheet = "data"))

  # base_sec_eodec <-
  #   data.frame(read_excel("data/_input/test/220105_IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx",
  #                         sheet = "data"))

  # base_sec_eojan <-
  #   data.frame(read_excel("data/_input/test/220202_IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx",
  #                         sheet = "supply_tracker"))

  # base_sec_eofeb <-
  #   data.frame(read_excel("data/_input/test/220302_IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx",
  #                         sheet = "supply_tracker"))

  # base_sec_eomar <-
  #   data.frame(read_excel("data/_input/test/220406_IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx",
  #                         sheet = "supply_tracker"))

  # base_sec_eoapr <-
  #   data.frame(read_excel("data/_input/test/220505_IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx",
  #                         sheet = "supply_tracker"))

  # base_sec_eomay <-
  #   data.frame(read_excel("data/_input/test/220602_IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx",
  #                         sheet = "supply_tracker"))

  # base_sec_eojun <-
  #   data.frame(read_excel("data/_input/test/220701_IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx",
  #                         sheet = "supply_tracker"))

  # ...

  # Reduce dataframe to required columns
  print(" >> Transforming secured data...")
  df_list_trans <- list()
  for (i in seq_along(df_list)) {
    df <- df_list[[i]]

    # finding the right column to extract
    col_vaccine_secured <- grep("secured", tolower(colnames(df)), value = TRUE)
    col_not_to_select <- c(
      grep("adjustment", col_vaccine_secured, value = TRUE),
      grep("population", col_vaccine_secured, value = TRUE)
    )
    column_name <- setdiff(col_vaccine_secured, col_not_to_select)

    if (length(column_name) > 1) {
      stop(paste0(
        "Error in supply_timeseries.r:
        Multiple columns match the criteria for automatic selection.
        Please delete one of the following from the latest file:
        ",
        paste(as.character(column_name), collapse = ", ")
      ))
    }

    # adding selected columns to a list of dataframes
    df <- df %>%
      rename_all(., .funs = tolower) %>%
      select(all_of(c("iso3", column_name, "month_name"))) %>%
      rename(sec = !!as.name(column_name)) %>%
      rename(iso = iso3)

    # multiplying column with the respective factor
    if (grepl("courses", column_name, fixed = TRUE)) {
      df$sec <- df$sec * 2
    }
    df$sec <- df$sec * 1000000
    df$type <- "Secured" # never used
    df_list_trans[[i]] <- df
  }

  # sec_eosep_slim <- select(base_sec_eosep, c("ISO3","Secured.Vaccine..millions.of.courses."))
  # sec_eosep_slim <- sec_eosep_slim %>%
  #   mutate(sec_eosep = (Secured.Vaccine..millions.of.courses.) * 1000000 * 2)
  # sec_eosep_slim <- select(sec_eosep_slim, -c("Secured.Vaccine..millions.of.courses."))

  # sec_eooct_slim <- select(base_sec_eooct, c("ISO3","Secured.Vaccine..millions.of.courses."))
  # sec_eooct_slim <- sec_eooct_slim %>%
  #   mutate(sec_eooct = (Secured.Vaccine..millions.of.courses.) * 1000000 * 2 )
  # sec_eooct_slim <- select(sec_eooct_slim, -c("Secured.Vaccine..millions.of.courses."))

  # sec_eonov_slim <- select(base_sec_eonov, c("ISO3","Secured.and.or.Expected.Vaccine..millions.of.courses."))
  # sec_eonov_slim <- sec_eonov_slim %>%
  #   mutate(sec_eonov = (Secured.and.or.Expected.Vaccine..millions.of.courses.) * 1000000 * 2)
  # sec_eonov_slim <- select(sec_eonov_slim, -c("Secured.and.or.Expected.Vaccine..millions.of.courses."))

  # sec_eodec_slim <- select(base_sec_eodec, c("ISO3","Secured.and.or.Expected.Vaccine..millions.of.courses."))
  # sec_eodec_slim <- sec_eodec_slim %>%
  #   mutate(sec_eodec = (Secured.and.or.Expected.Vaccine..millions.of.courses.) * 1000000 * 2)
  # sec_eodec_slim <- select(sec_eodec_slim, -c("Secured.and.or.Expected.Vaccine..millions.of.courses."))

  # sec_eojan_slim <- select(base_sec_eojan, c("ISO3","Secured.and.or.Expected.Vaccine..millions.of.courses."))
  # sec_eojan_slim <- sec_eojan_slim %>%
  #   mutate(sec_eojan = (Secured.and.or.Expected.Vaccine..millions.of.courses.) * 1000000 * 2)
  # sec_eojan_slim <- select(sec_eojan_slim, -c("Secured.and.or.Expected.Vaccine..millions.of.courses."))

  # sec_eofeb_slim <- select(base_sec_eofeb, c("ISO3","Secured.and.or.Expected.Vaccine..millions.of.courses."))
  # sec_eofeb_slim <- sec_eofeb_slim %>%
  #   mutate(sec_eofeb = (Secured.and.or.Expected.Vaccine..millions.of.courses.) * 1000000 * 2)
  # sec_eofeb_slim <- select(sec_eofeb_slim, -c("Secured.and.or.Expected.Vaccine..millions.of.courses."))

  # sec_eomar_slim <- select(base_sec_eomar, c("ISO3","Secured.and.or.Expected.Vaccine..millions.of.doses."))
  # sec_eomar_slim <- sec_eomar_slim %>%
  #   mutate(sec_eomar = (Secured.and.or.Expected.Vaccine..millions.of.doses.) * 1000000)
  # sec_eomar_slim <- select(sec_eomar_slim, -c("Secured.and.or.Expected.Vaccine..millions.of.doses."))

  # sec_eoapr_slim <- select(base_sec_eoapr, c("ISO3","Secured.and.or.Expected.Vaccine..millions.of.doses."))
  # sec_eoapr_slim <- sec_eoapr_slim %>%
  #   mutate(sec_eoapr = (Secured.and.or.Expected.Vaccine..millions.of.doses.) * 1000000)
  # sec_eoapr_slim <- select(sec_eoapr_slim, -c("Secured.and.or.Expected.Vaccine..millions.of.doses."))

  # sec_eomay_slim <- select(base_sec_eomay, c("ISO3","Secured.and.or.Expected.Vaccine..millions.of.doses."))
  # sec_eomay_slim <- sec_eomay_slim %>%
  #   mutate(sec_eomay = (Secured.and.or.Expected.Vaccine..millions.of.doses.) * 1000000)
  # sec_eomay_slim <- select(sec_eomay_slim, -c("Secured.and.or.Expected.Vaccine..millions.of.doses."))

  # sec_eojun_slim <- select(base_sec_eojun, c("ISO3","Secured.and.or.Expected.Vaccine..millions.of.doses."))
  # sec_eojun_slim <- sec_eojun_slim %>%
  #   mutate(sec_eojun = (Secured.and.or.Expected.Vaccine..millions.of.doses.) * 1000000)
  # sec_eojun_slim <- select(sec_eojun_slim, -c("Secured.and.or.Expected.Vaccine..millions.of.doses."))

  # # Join reduced dataframes
  # sec_overall <-
  #   left_join(sec_eosep_slim, sec_eooct_slim, by = "ISO3") %>%
  #   left_join(., sec_eonov_slim, by = "ISO3") %>%
  #   left_join(., sec_eodec_slim, by = "ISO3") %>%
  #   left_join(., sec_eojan_slim, by = "ISO3") %>%
  #   left_join(., sec_eofeb_slim, by = "ISO3") %>%
  #   left_join(., sec_eomar_slim, by = "ISO3") %>%
  #   left_join(., sec_eoapr_slim, by = "ISO3") %>%
  #   left_join(., sec_eomay_slim, by = "ISO3") %>%
  #   left_join(., sec_eojun_slim, by = "ISO3")

  # # Calculate change from month-month
  # sec_overall_cumul <- sec_overall

  # sec_overall <- sec_overall %>%
  #   mutate("2021-10" = sec_eooct - sec_eosep) %>%
  #   mutate("2021-11" = sec_eonov - sec_eooct) %>%
  #   mutate("2021-12" = sec_eodec - sec_eonov) %>%
  #   mutate("2022-01" = sec_eojan - sec_eodec) %>%
  #   mutate("2022-02" = sec_eofeb - sec_eojan) %>%
  #   mutate("2022-03" = sec_eomar - sec_eofeb) %>%
  #   mutate("2022-04" = sec_eoapr - sec_eomar) %>%
  #   mutate("2022-05" = sec_eomay - sec_eoapr) %>%
  #   mutate("2022-06" = sec_eojun - sec_eomay)

  # # Select only change columns
  # sec_overall <- select(sec_overall, c("ISO3",
  #                                     "2021-10",
  #                                     "2021-11",
  #                                     "2021-12",
  #                                     "2022-01",
  #                                     "2022-02",
  #                                     "2022-03",
  #                                     "2022-04",
  #                                     "2022-05",
  #                                     "2022-06"
  #                                     ))

  # # Round figures to whole numbers
  # sec_overall <- sec_overall %>%
  #   mutate_if(is.numeric, round)

  # # Create long dataframes by month
  # sec_overall_sep <- select(sec_overall_cumul, "ISO3","sec_eosep")
  # sec_overall_sep$month_name <- "2021-09"
  # sec_overall_sep$type <- "Secured"
  # colnames(sec_overall_sep) <- c("iso","value","month_name","type")

  # sec_overall_oct <- select(sec_overall_cumul, "ISO3","sec_eooct")
  # sec_overall_oct$month_name <- "2021-10"
  # sec_overall_oct$type <- "Secured"
  # colnames(sec_overall_oct) <- c("iso","value","month_name","type")

  # sec_overall_nov <- select(sec_overall_cumul, "ISO3","sec_eonov")
  # sec_overall_nov$month_name <- "2021-11"
  # sec_overall_nov$type <- "Secured"
  # colnames(sec_overall_nov) <- c("iso","value","month_name","type")

  # sec_overall_dec <- select(sec_overall_cumul, "ISO3","sec_eodec")
  # sec_overall_dec$month_name <- "2021-12"
  # sec_overall_dec$type <- "Secured"
  # colnames(sec_overall_dec) <- c("iso","value","month_name","type")

  # sec_overall_jan <- select(sec_overall_cumul, "ISO3","sec_eojan")
  # sec_overall_jan$month_name <- "2022-01"
  # sec_overall_jan$type <- "Secured"
  # colnames(sec_overall_jan) <- c("iso","value","month_name","type")

  # sec_overall_feb <- select(sec_overall_cumul, "ISO3","sec_eofeb")
  # sec_overall_feb$month_name <- "2022-02"
  # sec_overall_feb$type <- "Secured"
  # colnames(sec_overall_feb) <- c("iso","value","month_name","type")

  # sec_overall_mar <- select(sec_overall_cumul, "ISO3","sec_eomar")
  # sec_overall_mar$month_name <- "2022-03"
  # sec_overall_mar$type <- "Secured"
  # colnames(sec_overall_mar) <- c("iso","value","month_name","type")

  # sec_overall_apr <- select(sec_overall_cumul, "ISO3","sec_eoapr")
  # sec_overall_apr$month_name <- "2022-04"
  # sec_overall_apr$type <- "Secured"
  # colnames(sec_overall_apr) <- c("iso","value","month_name","type")

  # sec_overall_may <- select(sec_overall_cumul, "ISO3","sec_eomay")
  # sec_overall_may$month_name <- "2022-05"
  # sec_overall_may$type <- "Secured"
  # colnames(sec_overall_may) <- c("iso","value","month_name","type")

  # sec_overall_jun <- select(sec_overall_cumul, "ISO3","sec_eojun")
  # sec_overall_jun$month_name <- "2022-06"
  # sec_overall_jun$type <- "Secured"
  # colnames(sec_overall_jun) <- c("iso","value","month_name","type")

  # # ...

  # # Combine monthly long dataframes
  # sec_overall_long <- rbind(sec_overall_sep,
  #                           sec_overall_oct,
  #                           sec_overall_nov,
  #                           sec_overall_dec,
  #                           sec_overall_jan,
  #                           sec_overall_feb,
  #                           sec_overall_mar,
  #                           sec_overall_apr,
  #                           sec_overall_may,
  #                           sec_overall_jun
  #                           )

  sec_overall_long <- df_list_trans %>%
    bind_rows() %>%
    mutate_if(is.numeric, round)


  # Rename columns
  # colnames(sec_overall_long) <- c("iso","sec","month_name","type")
  return(sec_overall_long)
}


load_supply_received <- function(path) {
  # Supply received ---------------------------------------------------------

  # Load datasets
  df_list <- helper_load_list_of_files(
    c(
      "data/_input/test/20210726_UNICEF_DeliveryTable.xlsx",
      "data/_input/test/20210824_UNICEF_DeliveryTable.xlsx",
      "data/_input/test/20210927_UNICEF_DeliveryTable.xlsx",
      "data/_input/test/20211027_UNICEF_DeliveryTable.xlsx",
      "data/_input/test/20211130_UNICEF_DeliveryTable.xlsx",
      "data/_input/test/20211228_UNICEF_DeliveryTable.xlsx",
      "data/_input/test/20220126_UNICEF_DeliveryTable.xlsx",
      "data/_input/test/20220228_UNICEF_DeliveryTable.xlsx",
      "data/_input/test/20220328_UNICEF_DeliveryTable.xlsx",
      "data/_input/test/20220426_UNICEF_DeliveryTable.xlsx",
      "data/_input/test/20220530_UNICEF_DeliveryTable.xlsx",
      "data/_input/test/20220627_UNICEF_DeliveryTable.xlsx"
    ),
    sheets = "Delivery_Table",
    date_format = "%Y%m%d"
  )


  # reading in list of files in directory
  print(" >> Selecting files to read in...")
  files <- unlist(list.files(
    path = "data/_input/test/",
    # add to this pattern if the file name changes in the future
    pattern = "UNICEF|unicef"
  ))

  # checking for duplicate months
  months <- substr(files, 1, 6)
  if (sum(duplicated(months))) {
    stop(paste0("Error:
      Two files in ",
      path,
      " with 'imf' in the name have the same year month combination."))
  }

  # checking for reasonable years and months
  years <- substr(files, 1, 4)
  months <- substr(files, 5, 6)
  if (
    sum(as.numeric(years) > 2040) |
    sum(as.numeric(years) < 2021) |
    sum(as.numeric(months) > 12) |
    sum(as.numeric(months) < 1)) {
      stop("One of IMF datasets does not conform to year-month format
        (must be, e.g., 2108 for Aug 2021)")
    }

  # finding the right sheet to read in
  sheet_vector <- c()
  for (file in files) {
    sheets <- excel_sheets(path = paste0(path, file))
    # if sheet names change in the future, simply add to this list
    possible_sheet_names <- c("Delivery_Table")
    for (name in possible_sheet_names) {
      if (name %in% sheets) {
        relevant_sheet <- name
      }
    }
    # throw error if there is no sheet with one of the names above
    if (is.na(relevant_sheet)) {
      stop(paste0("Error: ", file, " does not have a sheet with a right name.
        Add new sheet name to the list of
        possible sheet names in supply_timeseries.r"))
    }
    sheet_vector <- append(sheet_vector, relevant_sheet)
    remove(relevant_sheet)
  }

  # import files from excel
  print(" >> Reading in UNICEF delivery table data...")
  df_list <- helper_load_list_of_files(
    files = paste0(path, files),
    sheets = sheet_vector,
    date_format = "%Y%m%d"
  )

  # base_eojul <-
  #   data.frame(read_excel("data/_input/test/20210726_UNICEF_DeliveryTable.xlsx",
  #                         sheet = "Delivery_Table"))

  # base_eoaug <-
  #   data.frame(read_excel("data/_input/test/20210824_UNICEF_DeliveryTable.xlsx",
  #                         sheet = "Delivery_Table"))

  # base_eosep <-
  #   data.frame(read_excel("data/_input/test/20210927_UNICEF_DeliveryTable.xlsx",
  #                         sheet = "Delivery_Table"))

  # base_eooct <-
  #   data.frame(read_excel("data/_input/test/20211027_UNICEF_DeliveryTable.xlsx",
  #                         sheet = "Delivery_Table"))

  # base_eonov <-
  #   data.frame(read_excel("data/_input/test/20211130_UNICEF_DeliveryTable.xlsx",
  #                         sheet = "Delivery_Table"))

  # base_eodec <-
  #   data.frame(read_excel("data/_input/test/20211228_UNICEF_DeliveryTable.xlsx",
  #                         sheet = "Delivery_Table"))

  # base_eojan <-
  #   data.frame(read_excel("data/_input/test/20220126_UNICEF_DeliveryTable.xlsx",
  #                         sheet = "Delivery_Table"))

  # base_eofeb <-
  #   data.frame(read_excel("data/_input/test/20220228_UNICEF_DeliveryTable.xlsx",
  #                         sheet = "Delivery_Table"))

  # base_eomar <-
  #   data.frame(read_excel("data/_input/test/20220328_UNICEF_DeliveryTable.xlsx",
  #                         sheet = "Delivery_Table"))

  # base_eoapr <-
  #   data.frame(read_excel("data/_input/test/20220426_UNICEF_DeliveryTable.xlsx",
  #                         sheet = "Delivery_Table"))

  # base_eomay <-
  #   data.frame(read_excel("data/_input/test/20220530_UNICEF_DeliveryTable.xlsx",
  #                         sheet = "Delivery_Table"))

  # base_eojun <-
  #   data.frame(read_excel("data/_input/test/20220627_UNICEF_DeliveryTable.xlsx",
  #                         sheet = "Delivery_Table"))

  print(" >> Transforming delivery data...")
  for (i in seq_along(df_list)) {
    # adding country codes to each observation
    colnames(df_list[[i]]) <- tolower(colnames(df_list[[i]]))
    df_list[[i]]$iso <- countrycode(
      df_list[[i]]$country.territory,
      origin = "country.name",
      destination = "iso3c",
      warn = TRUE
    )
    # fixing Kosovo, NAs, and summing supply per month and country
    options(dplyr.summarise.inform = FALSE)
    df_list[[i]] <- df_list[[i]] %>%
      mutate(iso = replace(iso, country.territory == "Kosovo", "XKX")) %>%
      drop_na(iso) %>%
      select(
        iso,
        total.doses.delivered,
        month_name
      ) %>%
      group_by(iso, month_name) %>%
      summarize(supply = sum(total.doses.delivered))
  }

  # appending all dataframes together
  print(" >> Combining monthly delivery data...")
  overall_cumul_long <- df_list %>%
    bind_rows() %>%
    arrange(month_name, iso)

  # base_eojul$iso <-
  #   countrycode(base_eojul$Country.territory,
  #     origin = "country.name",
  #     destination = "iso3c", warn = TRUE
  #   )

  # base_eojul <-
  #   base_eojul %>% mutate(iso = replace(iso, Country.territory == "Kosovo", "XKX"))
  # base_eojul <- base_eojul[!(is.na(base_eojul$iso)), ]
  # base_eojul <- select(base_eojul, -c("Country.territory"))

  # colnames(base_eojul) <- c(
  #   "product", "bimultilat", "donations", "covax", "unknown", "total_jul", "iso"
  # )

  # # August
  # base_eoaug$iso <-
  #   countrycode(base_eoaug$Country.territory,
  #     origin = "country.name",
  #     destination = "iso3c", warn = TRUE
  #   )

  # base_eoaug <-
  #   base_eoaug %>% mutate(iso = replace(iso, Country.territory == "Kosovo", "XKX"))
  # base_eoaug <- base_eoaug[!(is.na(base_eoaug$iso)), ]
  # base_eoaug <- select(base_eoaug, -c("Country.territory"))

  # colnames(base_eoaug) <- c(
  #   "product", "bimultilat", "donations", "covax", "avat", "unknown", "total_aug", "iso"
  # )

  # # September
  # base_eosep$iso <-
  #   countrycode(base_eosep$Country.territory,
  #     origin = "country.name",
  #     destination = "iso3c", warn = TRUE
  #   )

  # base_eosep <-
  #   base_eosep %>% mutate(iso = replace(iso, Country.territory == "Kosovo", "XKX"))
  # base_eosep <- base_eosep[!(is.na(base_eosep$iso)), ]
  # base_eosep <- select(base_eosep, -c("Country.territory"))

  # colnames(base_eosep) <- c(
  #   "product", "bimultilat", "donations", "covax", "avat", "unknown", "total_sep", "iso"
  # )

  # # October
  # base_eooct$iso <-
  #   countrycode(base_eooct$Country.territory,
  #     origin = "country.name",
  #     destination = "iso3c", warn = TRUE
  #   )

  # base_eooct <-
  #   base_eooct %>% mutate(iso = replace(iso, Country.territory == "Kosovo", "XKX"))
  # base_eooct <- base_eooct[!(is.na(base_eooct$iso)), ]
  # base_eooct <- select(base_eooct, -c("Country.territory"))

  # colnames(base_eooct) <- c(
  #   "product", "bimultilat", "donations", "covax", "avat", "unknown", "total_oct", "iso"
  # )

  # # November
  # base_eonov$iso <-
  #   countrycode(base_eonov$Country.territory,
  #     origin = "country.name",
  #     destination = "iso3c", warn = TRUE
  #   )

  # base_eonov <-
  #   base_eonov %>% mutate(iso = replace(iso, Country.territory == "Kosovo", "XKX"))
  # base_eonov <- base_eonov[!(is.na(base_eonov$iso)), ]
  # base_eonov <- select(base_eonov, -c("Country.territory"))

  # colnames(base_eonov) <- c(
  #   "product", "bimultilat", "donations", "covax", "avat", "unknown", "total_nov", "iso"
  # )

  # # December
  # base_eodec$iso <-
  #   countrycode(base_eodec$Country.territory,
  #     origin = "country.name",
  #     destination = "iso3c", warn = TRUE
  #   )

  # base_eodec <-
  #   base_eodec %>% mutate(iso = replace(iso, Country.territory == "Kosovo", "XKX"))
  # base_eodec <- base_eodec[!(is.na(base_eodec$iso)), ]
  # base_eodec <- select(base_eodec, -c("Country.territory"))

  # colnames(base_eodec) <- c(
  #   "product", "bimultilat", "donations", "covax", "avat", "unknown", "total_dec", "iso"
  # )

  # # January
  # base_eojan$iso <-
  #   countrycode(base_eojan$Country.territory,
  #     origin = "country.name",
  #     destination = "iso3c", warn = TRUE
  #   )

  # base_eojan <-
  #   base_eojan %>% mutate(iso = replace(iso, Country.territory == "Kosovo", "XKX"))
  # base_eojan <- base_eojan[!(is.na(base_eojan$iso)), ]
  # base_eojan <- select(base_eojan, -c("Country.territory"))

  # colnames(base_eojan) <- c(
  #   "product", "bimultilat", "donations", "covax", "avat", "unknown", "total_jan", "iso"
  # )

  # # February
  # base_eofeb$iso <-
  #   countrycode(base_eofeb$Country.territory,
  #     origin = "country.name",
  #     destination = "iso3c", warn = TRUE
  #   )

  # base_eofeb <-
  #   base_eofeb %>% mutate(iso = replace(iso, Country.territory == "Kosovo", "XKX"))
  # base_eofeb <- base_eofeb[!(is.na(base_eofeb$iso)), ]
  # base_eofeb <- select(base_eofeb, -c("Country.territory"))

  # colnames(base_eofeb) <- c(
  #   "product", "bimultilat", "donations", "covax", "avat", "unknown", "total_feb", "iso"
  # )

  # # March
  # base_eomar$iso <-
  #   countrycode(base_eomar$Country.territory,
  #     origin = "country.name",
  #     destination = "iso3c", warn = TRUE
  #   )

  # base_eomar <-
  #   base_eomar %>% mutate(iso = replace(iso, Country.territory == "Kosovo", "XKX"))
  # base_eomar <- base_eomar[!(is.na(base_eomar$iso)), ]
  # base_eomar <- select(base_eomar, -c("Country.territory"))

  # colnames(base_eomar) <- c(
  #   "product", "bimultilat", "donations", "covax", "avat", "unknown", "total_mar", "iso"
  # )

  # # April
  # base_eoapr$iso <-
  #   countrycode(base_eoapr$Country.territory,
  #     origin = "country.name",
  #     destination = "iso3c", warn = TRUE
  #   )

  # base_eoapr <-
  #   base_eoapr %>% mutate(iso = replace(iso, Country.territory == "Kosovo", "XKX"))
  # base_eoapr <- base_eoapr[!(is.na(base_eoapr$iso)), ]
  # base_eoapr <- select(base_eoapr, -c("Country.territory"))

  # colnames(base_eoapr) <- c(
  #   "product", "bimultilat", "donations", "covax", "avat", "unknown", "total_apr", "iso"
  # )

  # # May
  # base_eomay$iso <-
  #   countrycode(base_eomay$Country.territory,
  #     origin = "country.name",
  #     destination = "iso3c", warn = TRUE
  #   )

  # base_eomay <-
  #   base_eomay %>% mutate(iso = replace(iso, Country.territory == "Kosovo", "XKX"))
  # base_eomay <- base_eomay[!(is.na(base_eomay$iso)), ]
  # base_eomay <- select(base_eomay, -c("Country.territory"))

  # colnames(base_eomay) <- c(
  #   "product", "bimultilat", "donations", "covax", "avat", "unknown", "total_may", "iso"
  # )

  # # June
  # base_eojun$iso <-
  #   countrycode(base_eojun$Country.territory,
  #     origin = "country.name",
  #     destination = "iso3c", warn = TRUE
  #   )

  # base_eojun <-
  #   base_eojun %>% mutate(iso = replace(iso, Country.territory == "Kosovo", "XKX"))
  # base_eojun <- base_eojun[!(is.na(base_eojun$iso)), ]
  # base_eojun <- select(base_eojun, -c("Country.territory"))

  # colnames(base_eojun) <- c(
  #   "product", "bimultilat", "donations", "covax", "avat", "unknown", "total_jun", "iso"
  # )

  # # Reduce dataframes to required columns
  # eojun_slim <- select(base_eojun, c("iso", "product", "total_jun"))
  # eojun_slim <- eojun_slim %>%
  #   group_by(iso) %>%
  #   summarize(total_jun = sum(total_jun))

  # eomay_slim <- select(base_eomay, c("iso", "product", "total_may"))
  # eomay_slim <- eomay_slim %>%
  #   group_by(iso) %>%
  #   summarize(total_may = sum(total_may))

  # eoapr_slim <- select(base_eoapr, c("iso", "product", "total_apr"))
  # eoapr_slim <- eoapr_slim %>%
  #   group_by(iso) %>%
  #   summarize(total_apr = sum(total_apr))

  # eomar_slim <- select(base_eomar, c("iso", "product", "total_mar"))
  # eomar_slim <- eomar_slim %>%
  #   group_by(iso) %>%
  #   summarize(total_mar = sum(total_mar))

  # eofeb_slim <- select(base_eofeb, c("iso", "product", "total_feb"))
  # eofeb_slim <- eofeb_slim %>%
  #   group_by(iso) %>%
  #   summarize(total_feb = sum(total_feb))

  # eojan_slim <- select(base_eojan, c("iso", "product", "total_jan"))
  # eojan_slim <- eojan_slim %>%
  #   group_by(iso) %>%
  #   summarize(total_jan = sum(total_jan))

  # eodec_slim <- select(base_eodec, c("iso", "product", "total_dec"))
  # eodec_slim <- eodec_slim %>%
  #   group_by(iso) %>%
  #   summarize(total_dec = sum(total_dec))

  # eonov_slim <- select(base_eonov, c("iso", "product", "total_nov"))
  # eonov_slim <- eonov_slim %>%
  #   group_by(iso) %>%
  #   summarize(total_nov = sum(total_nov))

  # eooct_slim <- select(base_eooct, c("iso", "product", "total_oct"))
  # eooct_slim <- eooct_slim %>%
  #   group_by(iso) %>%
  #   summarize(total_oct = sum(total_oct))

  # eosep_slim <- select(base_eosep, c("iso", "product", "total_sep"))
  # eosep_slim <- eosep_slim %>%
  #   group_by(iso) %>%
  #   summarize(total_sep = sum(total_sep))

  # eoaug_slim <- select(base_eoaug, c("iso", "product", "total_aug"))
  # eoaug_slim <- eoaug_slim %>%
  #   group_by(iso) %>%
  #   summarize(total_aug = sum(total_aug))

  # eojul_slim <- select(base_eojul, c("iso", "product", "total_jul"))
  # eojul_slim <- eojul_slim %>%
  #   group_by(iso) %>%
  #   summarize(total_jul = sum(total_jul))

  # # ...

  # # Merge dataframes
  # del_overall <-
  #   left_join(eojul_slim, eoaug_slim, by = "iso") %>%
  #   left_join(., eosep_slim, by = "iso") %>%
  #   left_join(., eooct_slim, by = "iso") %>%
  #   left_join(., eonov_slim, by = "iso") %>%
  #   left_join(., eodec_slim, by = "iso") %>%
  #   left_join(., eojan_slim, by = "iso") %>%
  #   left_join(., eofeb_slim, by = "iso") %>%
  #   left_join(., eomar_slim, by = "iso") %>%
  #   left_join(., eoapr_slim, by = "iso") %>%
  #   left_join(., eomay_slim, by = "iso") %>%
  #   left_join(., eojun_slim, by = "iso")

  # return(del_overall)
  return(overall_cumul_long)
}

# transform_cum_supply_received <- function(del_overall) {

#   # Create cumulative monthly long form dataframes
#   # del_overall_cumul <- del_overall

#   # overall_cumul_jul <- select(del_overall_cumul, "iso", "total_jul")
#   # overall_cumul_jul$month_name <- "2021-07"
#   # colnames(overall_cumul_jul) <- c("iso", "supply", "month_name")

#   # overall_cumul_aug <- select(del_overall_cumul, "iso", "total_aug")
#   # overall_cumul_aug$month_name <- "2021-08"
#   # colnames(overall_cumul_aug) <- c("iso", "supply", "month_name")

#   # overall_cumul_sep <- select(del_overall_cumul, "iso", "total_sep")
#   # overall_cumul_sep$month_name <- "2021-09"
#   # colnames(overall_cumul_sep) <- c("iso", "supply", "month_name")

#   # overall_cumul_oct <- select(del_overall_cumul, "iso", "total_oct")
#   # overall_cumul_oct$month_name <- "2021-10"
#   # colnames(overall_cumul_oct) <- c("iso", "supply", "month_name")

#   # overall_cumul_nov <- select(del_overall_cumul, "iso", "total_nov")
#   # overall_cumul_nov$month_name <- "2021-11"
#   # colnames(overall_cumul_nov) <- c("iso", "supply", "month_name")

#   # overall_cumul_dec <- select(del_overall_cumul, "iso", "total_dec")
#   # overall_cumul_dec$month_name <- "2021-12"
#   # colnames(overall_cumul_dec) <- c("iso", "supply", "month_name")

#   # overall_cumul_jan <- select(del_overall_cumul, "iso", "total_jan")
#   # overall_cumul_jan$month_name <- "2022-01"
#   # colnames(overall_cumul_jan) <- c("iso", "supply", "month_name")

#   # overall_cumul_feb <- select(del_overall_cumul, "iso", "total_feb")
#   # overall_cumul_feb$month_name <- "2022-02"
#   # colnames(overall_cumul_feb) <- c("iso", "supply", "month_name")

#   # overall_cumul_mar <- select(del_overall_cumul, "iso", "total_mar")
#   # overall_cumul_mar$month_name <- "2022-03"
#   # colnames(overall_cumul_mar) <- c("iso", "supply", "month_name")

#   # overall_cumul_apr <- select(del_overall_cumul, "iso", "total_apr")
#   # overall_cumul_apr$month_name <- "2022-04"
#   # colnames(overall_cumul_apr) <- c("iso", "supply", "month_name")

#   # overall_cumul_may <- select(del_overall_cumul, "iso", "total_may")
#   # overall_cumul_may$month_name <- "2022-05"
#   # colnames(overall_cumul_may) <- c("iso", "supply", "month_name")

#   # overall_cumul_jun <- select(del_overall_cumul, "iso", "total_jun")
#   # overall_cumul_jun$month_name <- "2022-06"
#   # colnames(overall_cumul_jun) <- c("iso", "supply", "month_name")

#   # Merge monthly cumulative long form dataframes
#   overall_cumul_long <- rbind(
#     overall_cumul_jul,
#     overall_cumul_aug,
#     overall_cumul_sep,
#     overall_cumul_oct,
#     overall_cumul_nov,
#     overall_cumul_dec,
#     overall_cumul_jan,
#     overall_cumul_feb,
#     overall_cumul_mar,
#     overall_cumul_apr,
#     overall_cumul_may,
#     overall_cumul_jun
#   )

#   return(overall_cumul_long)
# }


transform_monthly_supply_received <- function(overall_cumul_long) {

  # Calculate monthly changes
  # del_overall <- del_overall %>%
  #   mutate("2022-06" = total_jun - total_may) %>%
  #   mutate("2022-05" = total_may - total_apr) %>%
  #   mutate("2022-04" = total_apr - total_mar) %>%
  #   mutate("2022-03" = total_mar - total_feb) %>%
  #   mutate("2022-02" = total_feb - total_jan) %>%
  #   mutate("2022-01" = total_jan - total_dec) %>%
  #   mutate("2021-12" = total_dec - total_nov) %>%
  #   mutate("2021-11" = total_nov - total_oct) %>%
  #   mutate("2021-10" = total_oct - total_sep) %>%
  #   mutate("2021-09" = total_sep - total_aug) %>%
  #   mutate("2021-08" = total_aug - total_jul)

  # # Replace blanks with 0
  # del_overall <- del_overall %>%
  #   mutate(across(where(is.numeric), ~ replace_na(., 0)))

  # # Reduce to monthly changes
  # overall_red <- select(del_overall, c(
  #   "iso",
  #   "2021-08",
  #   "2021-09",
  #   "2021-10",
  #   "2021-11",
  #   "2021-12",
  #   "2022-01",
  #   "2022-02",
  #   "2022-03",
  #   "2022-04",
  #   "2022-05",
  #   "2022-06",
  # ))

  # # Create monthly change long form dataframes
  # overall_aug <- select(del_overall, "iso", "2021-08")
  # overall_aug$month_name <- "2021-08"
  # overall_aug$type <- "Received"
  # colnames(overall_aug) <- c("iso", "value", "month_name", "type")

  # overall_sep <- select(del_overall, "iso", "2021-09")
  # overall_sep$month_name <- "2021-09"
  # overall_sep$type <- "Received"
  # colnames(overall_sep) <- c("iso", "value", "month_name", "type")

  # overall_oct <- select(del_overall, "iso", "2021-10")
  # overall_oct$month_name <- "2021-10"
  # overall_oct$type <- "Received"
  # colnames(overall_oct) <- c("iso", "value", "month_name", "type")

  # overall_nov <- select(del_overall, "iso", "2021-11")
  # overall_nov$month_name <- "2021-11"
  # overall_nov$type <- "Received"
  # colnames(overall_nov) <- c("iso", "value", "month_name", "type")

  # overall_dec <- select(del_overall, "iso", "2021-12")
  # overall_dec$month_name <- "2021-12"
  # overall_dec$type <- "Received"
  # colnames(overall_dec) <- c("iso", "value", "month_name", "type")

  # overall_jan <- select(del_overall, "iso", "2022-01")
  # overall_jan$month_name <- "2022-01"
  # overall_jan$type <- "Received"
  # colnames(overall_jan) <- c("iso", "value", "month_name", "type")

  # overall_feb <- select(del_overall, "iso", "2022-02")
  # overall_feb$month_name <- "2022-02"
  # overall_feb$type <- "Received"
  # colnames(overall_feb) <- c("iso", "value", "month_name", "type")

  # overall_mar <- select(del_overall, "iso", "2022-03")
  # overall_mar$month_name <- "2022-03"
  # overall_mar$type <- "Received"
  # colnames(overall_mar) <- c("iso", "value", "month_name", "type")

  # overall_apr <- select(del_overall, "iso", "2022-04")
  # overall_apr$month_name <- "2022-04"
  # overall_apr$type <- "Received"
  # colnames(overall_apr) <- c("iso", "value", "month_name", "type")

  # overall_may <- select(del_overall, "iso", "2022-05")
  # overall_may$month_name <- "2022-05"
  # overall_may$type <- "Received"
  # colnames(overall_may) <- c("iso", "value", "month_name", "type")

  # overall_jun <- select(del_overall, "iso", "2022-06")
  # overall_jun$month_name <- "2022-06"
  # overall_jun$type <- "Received"
  # colnames(overall_jun) <- c("iso", "value", "month_name", "type")

  # # Merge monthly change long form dataframes
  # overall_long <- rbind(
  #   overall_jun, overall_may, overall_apr, overall_mar, overall_feb,
  #   overall_jan, overall_dec, overall_nov, overall_oct,
  #   overall_sep, overall_aug
  # )
  print(" >> Calculating monthly difference in received supply...")
  overall_long <- overall_cumul_long %>%
    mutate(supply = replace_na(supply, 0)) %>%
    # calculating difference between months
    arrange(month_name) %>%
    group_by(iso) %>%
    mutate(value = supply - lag(supply)) %>%
    # adding type
    mutate(type = "Received") %>%
    # removing supply column and observation from July 2021
    select(-supply) %>%
    filter(
      month_name != first(overall_cumul_long$month_name))

  return(overall_long)
}


load_administration <- function(d_absorption_country_new, entity_characteristics) {
  # Administration ----------------------------------------------------------

  # Load datasets
  # base_admin <-
  #   data.frame(read_excel("data/output/output_master.xlsx",
  #                         sheet = "1_cum_absorb_month_country"))
  # base_admin <- d_absorption_country_new

  # b_details <-
  #   data.frame(read_excel("data/_input/static/base_entitydetails.xlsx",
  #                         sheet = "data"))

  # Reduce number of rows/rename columns
  # b_details_red <-
  #   select(
  #     b_details,
  #     c(
  #       "CODE",
  #       "COVAX",
  #       "CSC"
  #     )
  #   )

  # colnames(b_details_red) <-
  #   c(
  #     "iso",
  #     "a_covax_status",
  #     "a_csc_status"
  #   )

  # Reduce & rename dataframe to required columns
  # admin_red <- select(base_admin, c("iso", "a_csc_status", "a_covax_status", "absorbed", "month_name"))
  # colnames(admin_red) <- c("iso", "a_csc_status", "a_covax_status", "value", "month_name")

  # Merge base details with adminstration data
  print(" >> Loading administration data...")
  admin_red <- entity_characteristics %>%
    select(
      c(
        "a_iso",
        "a_covax_status",
        "a_csc_status"
      )) %>%
    rename(iso = a_iso) %>%
    right_join(d_absorption_country_new, by = "iso") %>%
    select(iso, a_csc_status, a_covax_status, absorbed, month_name) %>%
    rename(value = absorbed)

  return(admin_red)
}

export_supply_xlsx <- function(
  sec_overall_long, overall_long, overall_cumul_long, admin_red) {

  # Merge supply secured / supply received / administration
  print(" >> Exporting supply data...")
  combined_long <- full_join(
      sec_overall_long,
      overall_cumul_long,
      by = c("iso", "month_name")) %>%
    full_join(.,
      admin_red,
      by = c("iso", "month_name"))

  # Remove type
  combined_long <- select(combined_long, -type)

  # Cap values to positives
  combined_long <- combined_long %>%
    mutate(sec_tobedel = pmax(sec - supply, 0)) %>%
    mutate(del_tobeadmin = pmax(supply - value, 0))

  # Making a datalist
  datalist <- list(
    "data" = overall_long,
    "supply" = overall_cumul_long,
    "all" = combined_long
  )

  # Export to excel
  write_xlsx(
    datalist,
    "data/_input/static/supply.xlsx"
  )
}

load_cum_from_xlsx <- function() {
  overall_cumul_long <- data.frame(
    read_excel("data/_input/static/supply.xlsx",
      sheet = "supply"
    )
  )
  return(overall_cumul_long)
}

load_monthly_from_xlsx <- function() {
  overall_long <- data.frame(
    read_excel("data/_input/static/supply.xlsx",
      sheet = "data"
    )
  )
  return(overall_long)
}
