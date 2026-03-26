load_base_data <- function(refresh_api) {
    who_dashboard <- load_who_dashboard(refresh_api)
    datalist <- list("who_dashboard" = who_dashboard)
    return(datalist)
}

load_who_dashboard <- function(refresh_api) {
    print(">> Loading WHO COVID-19 dashboard vaccination data...")
    folder <- "data/input/interim"
    link <- "https://srhdpeuwpubsa.blob.core.windows.net/whdh/COVID/COV_VAC_UPTAKE_2021_2023.csv"
    storage_name <- paste0(folder, "/", sub(".*/", "", link))
    if (refresh_api | !file.exists(storage_name)) {
        who_dashboard <- fread(link)
        
        print(">> Selecting relevant data...")
        who_dashboard <- who_dashboard %>%
          group_by(COUNTRY) %>%
          filter(DATE == max(DATE)) %>%
          ungroup() %>%
          select(COUNTRY,
                COVID_VACCINE_DATE_INTRO_FIRST
            )

        print(">> Renaming selected data...")
        colnames(who_dashboard) <- c(
            "a_iso",
            "date_intro"
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