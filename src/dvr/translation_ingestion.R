
# Get Data
get_data <- function(refresh_api) {
  
  print(" > Defining URLs for API calls...")
  urls <- c('https://frontdoor-l4uikgap6gz3m.azurefd.net/NCOV/VAC_REP_COUNTS',
            'https://frontdoor-l4uikgap6gz3m.azurefd.net/NCOV/VAC_REP_COUNTS_EUR')
  print(" > Done.")
  folder <- "data/input/interim"
  
  # 2 data sources for Throughput, one for Europe and one for ROW
  # read in separately, clean, and then bind together
  print(" > Read in data sources separately, clean, and then bind together...")
  
  df_list <- list()
  for (url in urls) {
    storage_name <- paste0(folder, "/", str_extract(url, "[^/]+$"), ".csv")
    if (refresh_api | !file.exists(storage_name)) {
      print(" > Downloading data from azurefd.net API...")
      tryCatch({
        response <- GET(url)
        data_json <- content(response, "text") %>% fromJSON(flatten = TRUE)
      }, error = function(e) {
        data_json <- fromJSON(e$response$content, flatten = TRUE)
      })
      df1 <- as.data.frame(data_json$value)
      df1 <- df1 %>%
        select(COUNTRY_FK, AS_OF_DATE, DOSES_ADMINISTERED, PERSONS_VACCINATED_ONE_PLUS_DOSE, PERSONS_VACCINATED_FULL,
               PERSONS_BOOSTER_ADD_DOSE, SOURCE) %>%
        filter(COUNTRY_FK != 'None') %>%
        rename(entity = COUNTRY_FK, date = AS_OF_DATE, total_doses = DOSES_ADMINISTERED,
               at_least_one_dose = PERSONS_VACCINATED_ONE_PLUS_DOSE, fully_vaccinated = PERSONS_VACCINATED_FULL,
               persons_booster_add_dose = PERSONS_BOOSTER_ADD_DOSE, source = SOURCE) %>%
        mutate(country_name = str_to_title(entity)) %>%
        distinct()
      print(" > Done.")
      print(paste(" > Saving API data to", folder, "..."))
      if (!file.exists(folder)) {
        dir.create(folder, recursive = TRUE)
        print(paste(" > Creating a new folder", folder, "..."))
      }
      write.csv(df1, storage_name, row.names = FALSE)
    } else {
      print(paste(" > Old API data is used from", storage_name, "..."))
      df1 <- read.csv(storage_name, stringsAsFactors = FALSE)
    }
    df_list[[url]] <- df1
  }
  
  print(" > Joining 2 df to one...")
  df3 <- bind_rows(df_list)
  print(" > Done.")
  return(df3)
}

fix_dates <- function(df3) {
  # Save df3 to variable df_data
  df_data <- distinct(df3)
  # Manually fix some data date issues
  print(" > Manually fixing data date issues...")
  df_data[df_data$entity == "ANGUILLA" & df_data$date == "2001-02-26", "date"] <- "2021-02-26"
  df_data[df_data$entity == "TRINIDAD AND TOBAGO" & df_data$date == "2001-02-26", "date"] <- "2021-02-26"
  df_data[df_data$entity == "PAKISTAN" & df_data$date == "2010-03-05", "date"] <- "2021-03-05"
  df_data[df_data$entity == "CANADA" & df_data$date == "2020-01-29", "date"] <- "2021-01-29"
  df_data[df_data$entity == "LIBYA" & df_data$date == "1900-05-18", "date"] <- "2021-05-18"
  df_data[df_data$entity == "LIBYA" & df_data$date == "1900-05-20", "date"] <- "2021-05-20"
  df_data[df_data$entity == "LIBYA" & df_data$date == "1900-05-25", "date"] <- "2021-05-25"
  df_data[df_data$entity == "LIBYA" & df_data$date == "1900-05-26", "date"] <- "2021-05-26"
  df_data[df_data$entity == "BRUNEI DARUSSALAM" & df_data$date == "2021-03-20", "date"] <- "2021-04-20"
  df_data[df_data$entity == "LIBYA" & df_data$date == "2021-01-08", "date"] <- "2022-01-08"
  df_data[df_data$entity == "MOROCCO" & df_data$date == "2021-01-04", "date"] <- "2021-01-04"
  df_data[df_data$entity == "MOROCCO" & df_data$date == "2021-01-08", "date"] <- "2021-01-08"
  print(" > Done.")
  
  # Date stamp dataset
  print(" > Getting the stamp dataset...")
  df_data$date_accessed <- as.Date(Sys.Date())
  print(" > Done.")
  return(df_data)
}

main <- function(refresh_api) {
  df3 <- get_data(refresh_api)
  df_data <- fix_dates(df3)
  return(df_data)
}
