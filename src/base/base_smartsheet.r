load_base_data <- function() {
    print(" >> Load base smartsheet, WHO dashboard & concerted support list...")
    b_smartsheet <- load_base_smartsheet()
    b_who_dashboard <- load_who_dashboard()

    return(list(b_smartsheet, b_who_dashboard))
}

transform_base_data <- function(b_smartsheet) {
    print(" >> Transform Load base smartsheet...")
    b_smartsheet <- transform_base_smartsheet(b_smartsheet)
    
    return(b_smartsheet)
}

# Loading base smartsheet data
load_base_smartsheet <- function() {
    print(" >> Loading base smartsheet...")
    b_smartsheet <- data.frame(
        read_excel(
            "data/_input/base_smartsheet.xlsx",
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
                "At.risk.for.expiry",
                "Driving.factors"
            )
        )

    print(" >> Renaming Columns...")
    colnames(b_smartsheet) <- c(
        "a_iso",
        "ss_target",
        "ss_deadline",
        "expiry_risk",
        "note_ss_drivers"
    )

    return(b_smartsheet)

}

# WHO COVID-19 Dashboard

load_who_dashboard <- function() {
    print(" >> Loading WHO vaccination data...")
    b_who_dashboard <- fread(
        "https://covid19.who.int/who-data/vaccination-data.csv"
        )
        head(b_who_dashboard)

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
        "cov_total_fv_per100"
    )

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
        na_fill = 4
    )

    ## Change country target field type to date
    b_smartsheet$ss_deadline <-
    as.Date(b_smartsheet$ss_deadline)

    return(b_smartsheet)

}