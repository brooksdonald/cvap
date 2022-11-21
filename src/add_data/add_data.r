
load_who_dashboard <- function(refresh_api) {
    print(" >> Loading WHO vaccination data...")
    folder <- "data/input/interim"
    link <- "https://covid19.who.int/who-data/vaccination-data.csv"
    storage_name <- paste0(folder, "/", sub(".*/", "", link))
    if (refresh_api | !file.exists(storage_name)) {
      b_who_dashboard <- fread(link)
      
      print(" >> Selecting relevant columns...")
      b_who_dashboard <- select(
        b_who_dashboard, c(
          "ISO3",
          "NUMBER_VACCINES_TYPES_USED",
          "FIRST_VACCINE_DATE",
          "PERSONS_FULLY_VACCINATED_PER100"
          )
        )
      
      print(" >> Renaming Columns...")
      colnames(b_who_dashboard) <- c(
        "a_iso",
        "prod_inuse",
        "intro_date",
        "cov_total_fv_per100_whodb"
      )
      
      print(" >> Data is stored for future API calls...")
      if (!file.exists(folder)) dir.create(folder)
      write.csv(b_who_dashboard, file = storage_name, row.names = FALSE)
      } 
    else {
      print(paste0(" >? Old API data is used from ", folder, "..."))
      b_who_dashboard <- read.csv(storage_name)
      }
    
    print(" >> Function 'load_who_dashboard' done")
    return(b_who_dashboard)
}
