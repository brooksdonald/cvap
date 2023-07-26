load_ts_sup_sec <- function(path) {
  print(">> Selecting supply secured files to load...")
  files <- unlist(list.files(path = path,
                             # add to this pattern if the file name changes in the future
                             pattern = "IMF|imf"))
  
  # checking for duplicate months
  print(">> Checking if supply secured files are valid...")
  months <- substr(files, 1, 4)
  if (sum(duplicated(months))) {
    stop(
      paste0(
        "Error:
      Two files in ",
        path,
        " with 'imf' in the name have the same year month combination."
      )
    )
  }
  
  # checking for reasonable years and months
  years <- substr(files, 1, 2)
  months <- substr(files, 3, 4)
  if (sum(as.numeric(years) > 40) |
      sum(as.numeric(years) < 21) |
      sum(as.numeric(months) > 12) |
      sum(as.numeric(months) < 1)) {
    stop(
      "One of the IMF datasets does not conform to year-month format
        (must be, e.g., 2108 for Aug 2021)"
    )
  }
  
  # finding the right sheet to read in
  print(">> Checking if supply secured files have correct sheet...")
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
      stop(
        paste0(
          "Error: ",
          file,
          " does not have a sheet with a right name.
        Add new sheet name to the list of
        possible sheet names in supply_timeseries.r"
        )
      )
    }
    sheet_vector <- append(sheet_vector, relevant_sheet)
    remove(relevant_sheet)
  }
  
  print(">> Loading supply secured data...")
  df_list <- helper_load_list_of_files(
    files = paste0(path, files),
    sheets = sheet_vector,
    date_format = "%y%m%d",
    month_lag = 1
  )
  
  ## end of automated read-in
  
  # Reduce dataframe to required columns
  print(">> Transforming supply secured data...")
  df_list_trans <- list()
  for (i in seq_along(df_list)) {
    df <- df_list[[i]]
    
    # finding the right column to extract and columns to be excluded
    col_vaccine_secured <-
      grep("secured", tolower(colnames(df)), value = TRUE)
    col_not_to_select <- c(
      grep("adjustment", col_vaccine_secured, value = TRUE),
      grep("population", col_vaccine_secured, value = TRUE)
    )
    column_name <- setdiff(col_vaccine_secured, col_not_to_select)
    
    if (length(column_name) > 1) {
      stop(
        paste0(
          "Error in supply_timeseries.r:
        Multiple columns match the criteria for automatic selection.
        Please delete one of the following from the latest file:
        ",
          paste(as.character(column_name), collapse = ", ")
        )
      )
    }
    
    # adding selected columns to a list of dataframes
    df <- df %>%
      rename_all(., .funs = tolower) %>%
      select(all_of(c("iso3", column_name, "month_name"))) %>%
      rename(value = !!as.name(column_name)) %>%
      rename(iso = iso3) %>%
      helper_rename_KOS_to_XKX("iso")
    
    # multiplying column with the respective factor
    if (grepl("courses", column_name, fixed = TRUE)) {
      df$value <- df$value * 2
    }
    df$value <- df$value * 1000000
    df$type <- "secured" # never used
    df$measure <- "cumulative"
    df_list_trans[[i]] <- df
  }
  
  print(">> Merging monthly supply secured data...")
  sup_sec_ts <- df_list_trans %>%
    bind_rows() %>%
    mutate_if(is.numeric, round)
  
  print(">> Done.")
  return(sup_sec_ts)
}

load_ts_sup_rec <- function(path) {
  # reading in list of files in directory
  print(">> Selecting supply received files to load...")
  files <- unlist(list.files(path = path,
                             # add to this pattern if the file name changes in the future
                             pattern = "UNICEF|unicef"))
  
  # checking for duplicate months
  months <- substr(files, 1, 6)
  if (sum(duplicated(months))) {
    stop(
      paste0(
        "Error:
      Two files in ",
        path,
        " with 'unicef' in the name have the same year month combination."
      )
    )
  }
  
  # checking for reasonable years and months
  years <- substr(files, 1, 4)
  months <- substr(files, 5, 6)
  if (sum(as.numeric(years) > 2040) |
      sum(as.numeric(years) < 2021) |
      sum(as.numeric(months) > 12) |
      sum(as.numeric(months) < 1)) {
    stop(
      "One of the UNICEF datasets does not conform to year-month format
        (must be, e.g., 2108 for Aug 2021)"
    )
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
      stop(
        paste0(
          "Error: ",
          file,
          " does not have a sheet with a right name.
        Add new sheet name to the list of
        possible sheet names in supply_timeseries.r"
        )
      )
    }
    sheet_vector <- append(sheet_vector, relevant_sheet)
    remove(relevant_sheet)
  }
  
  # import files from excel
  print(">> Loading supply received data...")
  df_list <- helper_load_list_of_files(
    files = paste0(path, files),
    sheets = sheet_vector,
    date_format = "%Y%m%d"
  )
  
  # end of automated files
  
  print(">> Transforming supply received data...")
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
      select(iso,
             total.doses.delivered,
             month_name) %>%
      group_by(iso, month_name) %>%
      summarize(value = sum(total.doses.delivered))
  }
  
  # appending all dataframes together
  print(">> Merging monthly supply received data...")
  sup_rec_ts <- df_list %>%
    bind_rows() %>%
    arrange(month_name, iso) %>%
    mutate(type = "received",
           measure = "cumulative")
  
  print(">> Done.")
  return(sup_rec_ts)
}

transform_sup_rec <- function(sup_rec_ts) {
  print(">> Calculating monthly difference in supply received...")
  sup_rec_ts_add <- sup_rec_ts %>%
    select(-measure) %>%
    mutate(value = replace_na(value, 0)) %>%
    arrange(month_name) %>%
    group_by(iso) %>%
    mutate(value = value - lag(value)) %>%
    mutate(measure = "additive") %>%
    filter(month_name != first(month_name))
  
  print(">> Done.")
  return(sup_rec_ts_add)
}

transform_sup_sec <- function(sup_sec_ts) {
  print(">> Calculating monthly difference in supply secured...")
  sup_sec_ts_add <- sup_sec_ts %>%
    select(-measure) %>%
    mutate(value = replace_na(value, 0)) %>%
    arrange(month_name) %>%
    group_by(iso) %>%
    mutate(value = value - lag(value)) %>%
    mutate(measure = "additive") %>%
    filter(month_name != first(month_name))
  
  print(">> Done.")
  return(sup_sec_ts_add)
}

merge_export_sup_ts <-
  function(sup_sec_ts,
           sup_sec_ts_add,
           sup_rec_ts,
           sup_rec_ts_add) {
    print(">> Merging secured & received supply timeseries data (long-form)...")
    sup_ts_long <- rbind(sup_sec_ts,
                         sup_sec_ts_add,
                         sup_rec_ts,
                         sup_rec_ts_add)
    
    print(">> Merging secured & received supply timeseries data (wide-form)...")
    sup_ts_wide <-
      full_join(sup_sec_ts, sup_sec_ts_add, by = c("iso", "month_name")) %>%
      full_join(., sup_rec_ts, by = c("iso", "month_name")) %>%
      full_join(., sup_rec_ts_add, by = c("iso", "month_name")) %>%
      select(iso,
             month_name,
             value.x,
             value.y,
             value.x.x,
             value.y.y) %>%
      rename(
        sec_cumul = value.x,
        sec_add = value.y,
        rec_cumul = value.x.x,
        rec_add = value.y.y
      )
    
    print(">> Preparing datalist for export...")
    datalist <- list("long" = sup_ts_long,
                     "wide" = sup_ts_wide)
    
    print(">> Exporting supply timeseries data to excel...")
    write_xlsx(datalist,
               "data/input/interim/supply.xlsx")
    
    print(">> Done.")
  }

load_sup_ts_long_xlsx <- function() {
  print(">> Loading supply data (long-form)...")
  sup_ts_long <-
    data.frame(read_excel("data/input/interim/supply.xlsx",
                          sheet = "long"))
  
  print(">> Done.")
  return(sup_ts_long)
}

load_sup_ts_wide_xlsx <- function() {
  print(">> Loading supply data (wide-form)...")
  sup_ts_wide <-
    data.frame(read_excel("data/input/interim/supply.xlsx",
                          sheet = "wide"))
  
  print(">> Done.")
  return(sup_ts_wide)
}
