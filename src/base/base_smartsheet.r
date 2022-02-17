load_base_data <- function() {
    print(" >> Load base smartsheet, WHO dashboard and concerted support list...")
    b_smartsheet <- load_base_smartsheet()
    b_who_dashboard <- load_who_dashboard()
    b_csl <- load_conc_supp_list()

    return(list(b_smartsheet, b_who_dashboard, b_csl))
}

transform_base_data <- function(b_smartsheet) {
    print(" >> Transform Load base smartsheet...")
    b_smartsheet <- transform_base_smartsheet(b_smartsheet)
    
}

# Loading base smartsheet data
load_base_smartsheet <- function() {
    print(" >> Loading base smartsheet...")
    b_smartsheet <- data.frame(
        read_excel(
            "data/_input/base_smartsheet.xlsx",
            sheet = "data"
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
                "NDVP.Target.Population",
                "At.risk.for.expiry",
                "Driving.factors"
            )
        )

    print(" >> Renaming Columns...")
    colnames(b_smartsheet) <-c(
        "iso",
        "ndvp_target",
        "ndvp_deadline",
        "ndvp_tarpop",
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
        "iso", 
        "prod_inuse", 
        "intro_date",
        "cov_total_fv_per100"
    )

    return(b_who_dashboard)

}

# Concerted support list (csl)

load_conc_supp_list <- function() {
    print(" >> Loading concerted support list data...")
    b_csl <- data.frame(
        read_excel(
            "data/_input/static/base_csl.xlsx",
            sheet = "Sheet1"
        )
    )

    ## Rename columns
    print(" >> Renaming Columns...")
    colnames(b_csl) <- c(
        "iso",
        "csl_status"
    )

    return(b_csl)

}

transform_base_smartsheet <- function(b_smartsheet) {
    ## Rename expiry risk
    c_smartsheet_red <- c_smartsheet_red %>%
    mutate(expiry_risk = if_else(
        expiry_risk == "Red",
        "Doses at risk",
        if_else(
        expiry_risk == "Yellow",
        "Doses under observation",
        if_else(expiry_risk == "Green", "No doses at risk",
                "Unknown")
        )
    ))

    ## Change country target field type to date
    c_smartsheet_red$ndvp_deadline <-
    as.Date(c_smartsheet_red$ndvp_deadline)

    return(b_smartsheet)

}