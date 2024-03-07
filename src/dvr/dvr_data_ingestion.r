
get_data <- function(refresh_api) {
  print(">> Defining URLs for API calls...")
  urls <- c('https://xmart-api-public.who.int/WIISE/COV_UPTAKE_WEEKLY')
  print(">> Done.")
  
  folder <- "data/input/interim"
  
  print(">> Reading in general administration data from API...")
  df_list <- list()
  for (url in urls) {
    storage_name <- paste0(folder, "/", basename(url), ".csv")
    if (refresh_api || !file.exists(storage_name)) {
      print(">> Downloading data from API...")
      tryCatch({
        response <- GET(url)
        data_json <- fromJSON(content(response, "text"), flatten = TRUE)
      }, error = function(e) {
        data_json <- fromJSON(e$message, flatten = TRUE)
      })
      df1 <- data.frame(data_json$value)
      df1 <- type.convert(df1, as.is = TRUE)
      
      df1 <- df1 %>% 
        select(COUNTRY, 
               DATE, 
               DOSES_ADMINISTERED, 
               PERSONS_VACCINATED_ONE_PLUS_DOSE, 
               PERSONS_VACCINATED_COMPLETE, 
               PERSONS_VACCINATED_BOOSTER_ADD_DOSE, 
               SOURCE) %>%
        filter(COUNTRY != 'None') %>%
        rename(
          iso_code = COUNTRY,
          date = DATE,
          total_doses = DOSES_ADMINISTERED,
          at_least_one_dose = PERSONS_VACCINATED_ONE_PLUS_DOSE,
          fully_vaccinated = PERSONS_VACCINATED_COMPLETE,
          persons_booster_add_dose = PERSONS_VACCINATED_BOOSTER_ADD_DOSE,
          source = SOURCE
        )
      
      df1 <- distinct(df1)
      print(">> Done.")
      
      cat(">> Saving API data to ", folder, "...")
      if (!dir.exists(folder)) {
        cat(">> Creating a new folder ", folder, "/...")
        dir.create(folder, recursive = TRUE)
      }
      write_csv(df1, storage_name)
    } else {
      cat(">> Old API data is used from ", storage_name, "...\n")
      df1 <- read_csv(storage_name)
    }
    df_list[[length(df_list) + 1]] <- df1
  }
  
  print(">> Joining dataframes to one...")
  df3 <- bind_rows(df_list)
  print(">> Done.")
  
  return(df3)
}

fix_dates <- function(df3) {
  print(">> Selecting unique entries...")
  df_data <- distinct(data.frame(df3))
  
  print(">> Fixing date issues...")
  df_data <- df_data %>%
    mutate(date = case_when(
      iso_code == "AIA" & date == as.Date("2001-02-26") ~ as.Date("2021-02-26"),
      iso_code == "TTO" & date == as.Date("2001-02-26") ~ as.Date("2021-02-26"),
      iso_code == 'PAK' & date == as.Date("2010-03-05") ~ as.Date("2021-03-05"),
      iso_code == 'CAN' & date == as.Date("2020-01-29") ~ as.Date("2021-01-29"),
      iso_code == 'LBY' & date == as.Date("1900-05-18") ~ as.Date("2021-05-18"),
      iso_code == 'LBY' & date == as.Date("1900-05-20") ~ as.Date("2021-05-20"),
      iso_code == 'LBY' & date == as.Date("1900-05-25") ~ as.Date("2021-05-25"),
      iso_code == 'LBY' & date == as.Date("1900-05-26") ~ as.Date("2021-05-26"),
      iso_code == 'BRN' & date == as.Date("2021-03-20") ~ as.Date("2021-04-20"),
      iso_code == 'LBY' & date == as.Date("2021-01-08") ~ as.Date("2022-01-08"),
      iso_code == 'MAR' & date == as.Date("2021-01-04") ~ as.Date("2021-01-04"),
      iso_code == 'MAR' & date == as.Date("2021-01-08") ~ as.Date("2021-01-08"),
      TRUE ~ as.Date(date)
      )
    )
  print(">> Done.")
  
  return(df_data)
}

main <- function(refresh_api) {
  df3 <- get_data(refresh_api)
  df_data <- fix_dates(df3)
  return(df_data)
}
