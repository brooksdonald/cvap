library(jsonlite)
library(httr)
library(dplyr)

# Get Data
get_data <- function(refresh_api) {
  cat(" > Defining URLs for API calls...\n")
  urls <- c(
    "https://frontdoor-l4uikgap6gz3m.azurefd.net/NCOV/VAC_REP_COUNTS",
    "https://frontdoor-l4uikgap6gz3m.azurefd.net/NCOV/VAC_REP_COUNTS_EUR"
  )
  cat(" > Done.\n")
  folder <- "data/input/interim"
  
  cat(" > Read in data sources separately, clean, and then bind together...\n")
  
  df_list <- list()
  for (url in urls) {
    storage_name <- paste(folder, "/", basename(url), ".csv", sep = "")
    if (refresh_api || !file.exists(storage_name)) {
      cat(" > Downloading data from azurefd.net API...\n")
      response <- GET(url)
      data_json <- content(response, as = "text", encoding = "UTF-8") %>%
        fromJSON(simplifyVector = TRUE)
      df1 <- as.data.frame(data_json$value, stringsAsFactors = FALSE)
      df1 <- df1 %>%
        select(COUNTRY_FK, AS_OF_DATE, DOSES_ADMINISTERED, PERSONS_VACCINATED_ONE_PLUS_DOSE,
               PERSONS_VACCINATED_FULL, PERSONS_BOOSTER_ADD_DOSE, SOURCE) %>%
        filter(COUNTRY_FK != 'None') %>%
        rename(entity = COUNTRY_FK, date = AS_OF_DATE, total_doses = DOSES_ADMINISTERED,
               at_least_one_dose = PERSONS_VACCINATED_ONE_PLUS_DOSE,
               fully_vaccinated = PERSONS_VACCINATED_FULL,
               persons_booster_add_dose = PERSONS_BOOSTER_ADD_DOSE, source = SOURCE) %>%
        mutate(country_name = tools::toTitleCase(entity))
      df1 <- df1[!duplicated(df1), ]
      cat(" > Done.\n")
      cat(" > Saving API data to", folder, "...\n")
      if (!file.exists(folder)) {
        dir.create(folder, recursive = TRUE)
        cat(" > Creating a new folder", folder, "/...\n")
      }
      write.csv(df1, storage_name, row.names = FALSE)
    } else {
      cat(" > Old API data is used from", storage_name, "...\n")
      df1 <- read.csv(storage_name, stringsAsFactors = FALSE)
    }
    df_list <- append(df_list, list(df1))
  }
  
  cat(" > Joining 2 df to one...\n")
  df3 <- bind_rows(df_list)
  cat(" > Done.\n")
  return(df3)
}

fix_dates <- function(df3) {
  # Save df3 to variable df_data
  df_data <- distinct(df3)
  # Manually fix some data date issues
  cat(" > Manually fixing data date issues...\n")
  df_data[df_data$entity == 'ANGUILLA' & df_data$date == '2001-02-26', 'date'] <- '2021-02-26'
  df_data[df_data$entity == 'TRINIDAD AND TOBAGO' & df_data$date == '2001-02-26', 'date'] <- '2021-02-26'
  df_data[df_data$entity == 'PAKISTAN' & df_data$date == '2010-03-05', 'date'] <- '2021-03-05'
  df_data[df_data$entity == 'CANADA' & df_data$date == '2020-01-29', 'date'] <- '2021-01-29'
  df_data[df_data$entity == 'LIBYA' & df_data$date == '1900-05-18', 'date'] <- '2021-05-18'
  df_data[df_data$entity == 'LIBYA' & df_data$date == '1900-05-20', 'date'] <- '2021-05-20'
  df_data[df_data$entity == 'LIBYA' & df_data$date == '1900-05-25', 'date'] <- '2021-05-25'
  df_data[df_data$entity == 'LIBYA' & df_data$date == '1900-05-26', 'date'] <- '2021-05-26'
  df_data[df_data$entity == 'BRUNEI DARUSSALAM' & df_data$date == '2021-03-20', 'date'] <- '2021-04-20'
  df_data[df_data$entity == 'LIBYA' & df_data$date == '2021-01-08', 'date'] <- '2022-01-08'
  df_data[df_data$entity == 'MOROCCO' & df_data$date == '2021-01-04', 'date'] <- '2021-01-04'
  df_data[df_data$entity == 'MOROCCO' & df_data$date == '2021-01-08', 'date'] <- '2021-01-08'
  cat(" > Done.\n")
  
  # Date stamp dataset
  cat(" > Getting the stamp dataset...\n")
  df_data$date_accessed <- as.character(Sys.Date())
  cat(" > Done.\n")
  return(df_data)
}

main <- function(refresh_api) {
  df3 <- get_data(refresh_api)
  df_data <- fix_dates(df3)
  return(df_data)
}
