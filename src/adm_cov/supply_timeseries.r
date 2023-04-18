
load_secured_expected <- function(path) {
  print(" >> Loading demand planning data")
  
  print(" >> Selecting files to read in...")
  files <- unlist(list.files(
    path = path,
    pattern = "IMF|imf"
  ))
  
  print(" >> Checking if data is valid...")
  months <- substr(files, 1, 4)
  if (sum(duplicated(months))) {
    stop(paste0("Error:
      Two files in ",
                path,
                " with 'imf' in the name have the same year month combination."))
  }
  
  print(" >> Checking for reasonable years and months...")
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
  
  print(" >> Finding the right sheet to read in...")
  sheet_vector <- c()
  for (file in files) {
    sheets <- excel_sheets(path = paste0(path, file))
    possible_sheet_names <- c("supply_tracker", "data")
    for (name in possible_sheet_names) {
      if (name %in% sheets) {
        relevant_sheet <- name
      }
    }
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
  
  print(" >> Transforming secured data...")
  df_list_trans <- list()
  print(" >> Finding columns to extract and to exclude...")
  for (i in seq_along(df_list)) {
    df <- df_list[[i]]
    
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
    
    print(" >> Adding selected columns to list of dataframes...")
    df <- df %>%
      rename_all(., .funs = tolower) %>%
      select(all_of(c("iso3", column_name, "month_name"))) %>%
      rename(sec = !!as.name(column_name)) %>%
      rename(iso = iso3) %>%
      helper_rename_KOS_to_XKX("iso")
    
    if (grepl("courses", column_name, fixed = TRUE)) {
      df$sec <- df$sec * 2
    }
    df$sec <- df$sec * 1000000
    df$type <- "Secured" # never used
    df_list_trans[[i]] <- df
  }
  
  overall_secured_expected <- df_list_trans %>%
    bind_rows() %>%
    mutate_if(is.numeric, round)
  
  print(" >> Function 'load_secured_expected' done")  
  return(overall_secured_expected)
}


load_supply_received <- function(path) {
  print(" >> Loading demand planning data")
  
  print(" >> Selecting files to read in...")
  files <- unlist(list.files(
    path = path,
    pattern = "UNICEF|unicef"
  ))
  
  print(" >> Checking if data is valid...")
  months <- substr(files, 1, 6)
  if (sum(duplicated(months))) {
    stop(paste0("Error:
      Two files in ",
                path,
                " with 'imf' in the name have the same year month combination."))
  }
  
  print(" >> Checking for reasonable years and months...")
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
  
  print(" >> Finding the right sheet to read in...")
  sheet_vector <- c()
  for (file in files) {
    sheets <- excel_sheets(path = paste0(path, file))
    possible_sheet_names <- c("Delivery_Table")
    for (name in possible_sheet_names) {
      if (name %in% sheets) {
        relevant_sheet <- name
      }
    }
    if (is.na(relevant_sheet)) {
      stop(paste0("Error: ", file, " does not have a sheet with a right name.
        Add new sheet name to the list of
        possible sheet names in supply_timeseries.r"))
    }
    sheet_vector <- append(sheet_vector, relevant_sheet)
    remove(relevant_sheet)
  }
  
  print(" >> Reading in UNICEF delivery table data...")
  df_list <- helper_load_list_of_files(
    files = paste0(path, files),
    sheets = sheet_vector,
    date_format = "%Y%m%d"
  )
  
  print(" >> Transforming delivery data...")
  for (i in seq_along(df_list)) {
    colnames(df_list[[i]]) <- tolower(colnames(df_list[[i]]))
    df_list[[i]]$iso <- countrycode(
      df_list[[i]]$country.territory,
      origin = "country.name",
      destination = "iso3c",
      warn = TRUE
    )
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
  
  print(" >> Combining monthly delivery data...")
  overall_supply_received <- df_list %>%
    bind_rows() %>%
    arrange(month_name, iso)
  
  print(" >> Function 'load_supply_received' done")  
  return(overall_supply_received)
}


transform_supply_received <- function(overall_supply_received) {
  print(" >> Transforming monthly received supply data...")
  
  print(" >> Calculating monthly difference in received supply...")
  overall_long <- overall_supply_received %>%
    mutate(supply = replace_na(supply, 0)) %>%
    arrange(month_name) %>%
    group_by(iso) %>%
    mutate(value = supply - lag(supply)) %>%
    mutate(type = "Received") %>%
    filter(
      month_name != first(overall_supply_received$month_name))
  
  print(" >> Function 'transform_supply_received' done")  
  return(overall_long)
}


load_administration <- function(new_absorption_countries, entity_characteristics) {
  print(" >> Loading administration data...")
  administration_red <- entity_characteristics %>%
    select(
      c(
        "a_iso",
        "a_covax_status",
        "a_csc_status"
      )) %>%
    rename(iso = a_iso) %>%
    right_join(new_absorption_countries, by = "iso") %>%
    select(iso, a_csc_status, a_covax_status, absorbed, month_name) %>%
    rename(value = absorbed)
  
  print(" >> Function 'load_administration' done")  
  return(administration_red)
}


export_supply_xlsx <- function(
    overall_secured_expected, overall_long, overall_supply_received, administration_red) {
  print(" >> Merging supply data...")  
  
  print(" >> Exporting supply data...")
  combined_long <- full_join(
    overall_secured_expected,
    overall_supply_received,
    by = c("iso", "month_name")) %>%
    full_join(.,
              administration_red,
              by = c("iso", "month_name"))
  
  print(" >> Selecting relevant columns...")
  combined_long <- select(combined_long, -type)
  
  print(" >> Capping values...")
  combined_long <- combined_long %>%
    mutate(sec_tobedel = pmax(sec - supply, 0)) %>%
    mutate(del_tobeadmin = pmax(supply - value, 0))
  
  print(" >> Creating list of dataframes...")
  datalist <- list(
    "data" = overall_long,
    "supply" = overall_supply_received,
    "all" = combined_long
  )
  
  print(" >> Exporting to excel...")
  write_xlsx(
    datalist,
    "data/input/interim/supply.xlsx"
  )
  
  print(" >> Function 'export_supply_xlsx' done")  
}


load_supply_cum_xlsx <- function() {
  print(" >> Loading supply (cumulative) data...")
  overall_cumul_long <- data.frame(
    read_excel("data/input/interim/supply.xlsx",
               sheet = "supply"
    )
  )
  
  print(" >> Function 'load_supply_cum_xlsx' done")  
  return(overall_cumul_long)
}


load_supply_month_xlsx <- function() {
  print(" >> Loading supply (monthly) data...")  
  overall_long <- data.frame(
    read_excel("data/input/interim/supply.xlsx",
               sheet = "data"
    )
  )
  
  print(" >> Function 'load_supply_month_xlsx' done")  
  return(overall_long)
}
