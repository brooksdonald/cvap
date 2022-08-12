load_secured_expected <- function(path) {
  # Load datasets

  # df_list1 <- helper_load_list_of_files(
  #   c(
  #     "data/_input/test/211005_imf-who-covid-19-vaccine-supply-tracker.xlsx",
  #     "data/_input/test/211102_imf-who-covid-19-vaccine-supply-tracker.xlsx",
  #     "data/_input/test/211201_imf-who-covid-19-vaccine-supply-tracker.xlsx",
  #     "data/_input/test/220105_IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx"
  #   ),
  #   sheets = "data",
  #   date_format = "%y%m%d",
  #   month_lag = 1
  # )

  # df_list2 <- helper_load_list_of_files(
  #   c(
  #     "data/_input/test/220202_IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx",
  #     "data/_input/test/220302_IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx",
  #     "data/_input/test/220406_IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx",
  #     "data/_input/test/220505_IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx",
  #     "data/_input/test/220602_IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx",
  #     "data/_input/test/220701_IMF-WHO COVID-19 Vaccine Supply Tracker.xlsx"
  #   ),
  #   sheets = "supply_tracker",
  #   date_format = "%y%m%d",
  #   month_lag = 1
  # )
  # df_list <- append(df_list1, df_list2)


  ## alternative automatic read in of files

  # reading in list of files in directory
  print(" >> Selecting files to read in...")
  files <- unlist(list.files(
    path = path,
    # add to this pattern if the file name changes in the future
    pattern = "IMF|imf"
  ))

  # checking for duplicate months
  print(" >> Checking if data is valid...")
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
  print(" >> Finding the right sheet to read in...")
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
    date_format = "%y%m%d",
    month_lag = 1
  )

  ## end of automated read-in

  # Reduce dataframe to required columns
  print(" >> Transforming secured data...")
  df_list_trans <- list()
  for (i in seq_along(df_list)) {
    df <- df_list[[i]]

    # finding the right column to extract and columns to be excluded
    col_vaccine_secured <- grep("secured", tolower(colnames(df)), value = TRUE)
    col_not_to_select <- c(
      grep("adjustment", col_vaccine_secured, value = TRUE),
      grep("population", col_vaccine_secured, value = TRUE),
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

  sec_overall_long <- df_list_trans %>%
    bind_rows() %>%
    mutate_if(is.numeric, round)

  return(sec_overall_long)
}


load_supply_received <- function(path) {
  # Supply received ---------------------------------------------------------

  # Load datasets
  # df_list <- helper_load_list_of_files(
  #   c(
  #     "data/_input/test/20210726_UNICEF_DeliveryTable.xlsx",
  #     "data/_input/test/20210824_UNICEF_DeliveryTable.xlsx",
  #     "data/_input/test/20210927_UNICEF_DeliveryTable.xlsx",
  #     "data/_input/test/20211027_UNICEF_DeliveryTable.xlsx",
  #     "data/_input/test/20211130_UNICEF_DeliveryTable.xlsx",
  #     "data/_input/test/20211228_UNICEF_DeliveryTable.xlsx",
  #     "data/_input/test/20220126_UNICEF_DeliveryTable.xlsx",
  #     "data/_input/test/20220228_UNICEF_DeliveryTable.xlsx",
  #     "data/_input/test/20220328_UNICEF_DeliveryTable.xlsx",
  #     "data/_input/test/20220426_UNICEF_DeliveryTable.xlsx",
  #     "data/_input/test/20220530_UNICEF_DeliveryTable.xlsx",
  #     "data/_input/test/20220627_UNICEF_DeliveryTable.xlsx"
  #   ),
  #   sheets = "Delivery_Table",
  #   date_format = "%Y%m%d"
  # )

  # alternative automated way to read in files

  # reading in list of files in directory
  print(" >> Selecting files to read in...")
  files <- unlist(list.files(
    path = path,
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

  # end of automated files

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

  return(overall_cumul_long)
}

transform_monthly_supply_received <- function(overall_cumul_long) {

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
