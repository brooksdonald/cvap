load_base_data <- function(refresh_api) {
    print(" >> Load base smartsheet & WHO dashboard...")
    b_smartsheet <- load_base_smartsheet()
    b_who_dashboard <- load_who_dashboard(refresh_api)
    datalist <- list("b_smartsheet" = b_smartsheet,
        "b_who_dashboard" = b_who_dashboard)
    return(datalist)
}

# Loading base smartsheet data
load_base_smartsheet <- function() {
    print(" >> Loading base smartsheet...")
    b_smartsheet <- data.frame(
        read_excel(
            "data/input/base_smartsheet.xlsx",
            sheet = "1. CRD-global-monitoring (PMO M"
        )
    )

    # IMR Smartsheet
    ## Select relevant columns and rename
    print(" >> Selecting base smartsheet data...")
    b_smartsheet <-
        select(
            b_smartsheet,
            c(
                "ISO3",
                "NDVP...Coverage.target..",
                "NDVP...Coverage.deadline",
                "At.risk.for.expiry"
            )
        )

    print(" >> Renaming Columns...")
    colnames(b_smartsheet) <- c(
        "a_iso",
        "ss_target",
        "ss_deadline",
        "expiry_risk"
    )

    return(b_smartsheet)

}

# WHO COVID-19 Dashboard

load_who_dashboard <- function(refresh_api) {
    print(" >> Loading WHO vaccination data...")
    folder <- "data/input/interim"
    link <- "https://covid19.who.int/who-data/vaccination-data.csv"
    storage_name <- paste0(folder, "/", sub(".*/", "", link))
    if (refresh_api | !file.exists(storage_name)) {
        b_who_dashboard <- fread(link)

        # WHO COVID-19 Dashboard
        ## Select relevant columns and rename
        print(" >> Selecting WHO vaccination data")
        b_who_dashboard <- select(
            b_who_dashboard,
            c(
                "ISO3",
                "NUMBER_VACCINES_TYPES_USED",
                "FIRST_VACCINE_DATE",
                "PERSONS_FULLY_VACCINATED_PER100"
            )
        )

        print(" >> Renaming Columns")
        colnames(b_who_dashboard) <- c(
            "a_iso",
            "prod_inuse",
            "intro_date",
            "cov_total_fv_per100_whodb"
        )

        print(" > Data is stored for future API calls...")
        if (!file.exists(folder)) dir.create(folder)
        write.csv(b_who_dashboard, file = storage_name, row.names = FALSE)
    } else {
        print(paste0(" > Old API data is used from ", folder, "..."))
        b_who_dashboard <- read.csv(storage_name)
    }
    print(" > Done.")
    return(b_who_dashboard)
}

transform_base_smartsheet <- function(b_smartsheet) {
    ## Rename expiry risk
    print(" >> Renaming expiry risk...")
    b_smartsheet$expiry_risk <- helper_replace_values_with_map(
        data = b_smartsheet$expiry_risk,
        values = c(
            "Red",
            "Yellow",
            "Green"
        ),
        map = c(
            "Doses at risk",
            "Under observation",
            "No doses at risk"
        ),
        na_fill = "Unknown"

    )
    b_smartsheet$expiry_risk_num <- helper_replace_values_with_map(
        data = b_smartsheet$expiry_risk,
        values = c(
            "Doses at risk",
            "Under observation",
            "No doses at risk"
        ),
        map = c(1, 2, 3),
        na_fill = 4,
    )


    ## Change country target field type to date
    b_smartsheet$ss_deadline <-
    as.Date(b_smartsheet$ss_deadline)

    return(b_smartsheet)

}