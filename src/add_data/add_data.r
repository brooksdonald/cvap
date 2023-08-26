load_base_data <- function(refresh_api) {
    who_dashboard <- load_who_dashboard(refresh_api)
    datalist <- list("who_dashboard" = who_dashboard)
    return(datalist)
}

load_who_dashboard <- function(refresh_api) {
    print(">> Loading WHO COVID-19 dashboard vaccination data...")
    folder <- "data/input/interim"
    link <- "https://covid19.who.int/who-data/vaccination-data.csv"
    storage_name <- paste0(folder, "/", sub(".*/", "", link))
    if (refresh_api | !file.exists(storage_name)) {
        who_dashboard <- fread(link)
        
        print(">> Selecting relevant data...")
        who_dashboard <- select(
            who_dashboard,
            c(
                "ISO3",
                "NUMBER_VACCINES_TYPES_USED",
                "FIRST_VACCINE_DATE",
                "PERSONS_LAST_DOSE_PER100"
            )
        )

        print(">> Renaming selected data...")
        colnames(who_dashboard) <- c(
            "a_iso",
            "adm_prod_inuse",
            "date_intro",
            "cov_total_fv_per100_whodb"
        )

        print(">> Data is stored for future API calls...")
        if (!file.exists(folder)) dir.create(folder)
        write.csv(who_dashboard, file = storage_name, row.names = FALSE)
    } else {
        print(paste0(" > Old API data is used from ", folder, "..."))
        who_dashboard <- read.csv(storage_name)
    }
    
    print(">> Done.")
    
    return(who_dashboard)
}