
# Get Data
get_data <- function(refresh_api) {
  cat(" > Defining URLs for API calls...\n")
  urls <- c('https://frontdoor-l4uikgap6gz3m.azurefd.net/NCOV/VAC_REP_COUNTS',
            'https://frontdoor-l4uikgap6gz3m.azurefd.net/NCOV/VAC_REP_COUNTS_EUR')
  cat(" > Done.\n")
  folder <- "data/input/interim"
  
  # Read in data sources separately, clean, and then bind together
  cat(" > Read in data sources separately, clean, and then bind together...\n")
  
  df_list <- list()
  for (url in urls) {
    storage_name <- paste0(folder, "/", basename(url), ".csv")
    if (refresh_api || !file.exists(storage_name)) {
      cat(" > Downloading data from azurefd.net API...\n")
      response <- tryCatch({
        content(GET(url), "text", encoding = "UTF-8")
      }, error = function(e) {
        e
      })
      data_json <- fromJSON(response)
      df <- as.data.frame(data_json$value)
      df <- df %>% select(COUNTRY_FK, AS_OF_DATE, DOSES_ADMINISTERED, PERSONS_VACCINATED_ONE_PLUS_DOSE, PERSONS_VACCINATED_FULL, PERSONS_BOOSTER_ADD_DOSE, SOURCE)
      df <- df %>% filter(COUNTRY_FK != 'None')
      colnames(df) <- c('entity', 'date', 'total_doses', 'at_least_one_dose', 'fully_vaccinated', 'persons_booster_add_dose', 'source')
      df$country_name <- tolower(df$entity)
      df <- unique(df)
      cat(" > Done.\n")
      cat(" > Saving API data to ", folder, "...\n")
      if (!dir.exists(folder)) {
        cat(" > Creating a new folder ", folder, "/...\n")
        dir.create(folder, recursive = TRUE)
      }
      write_csv(df, storage_name)
    } else {
      cat(" > Old API data is used from ", storage_name, "...\n")
      df <- read_csv(storage_name)
    }
    df_list[[length(df_list) + 1]] <- df
  }
  
  cat(" > Joining 2 df to one...\n")
  df_combined <- bind_rows(df_list)
  cat(" > Done.\n")
  return(df_combined)
}

fix_dates <- function(df) {
  cat(" > Manually fixing data date issues...\n")
  # Manual date corrections here...
  # Use dplyr and lubridate to adjust dates as needed
  
  cat(" > Getting the stamp dataset...\n")
  df$date_accessed <- Sys.Date()
  cat(" > Done.\n")
  return(df)
}

main <- function(refresh_api) {
  df <- get_data(refresh_api)
  df_fixed <- fix_dates(df)
  return(df_fixed)
}

# Example usage:
# df_data <- main(TRUE)
