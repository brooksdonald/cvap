extract_supply_secured <- function() {
    print(" >> Reading supply secured data...")
    supply_input <-
        data.frame(read_excel("data/_input/base_supply_secured_summary.xlsx",
            sheet = "supply_tracker"
        ))

    # pick only relevant columns and rename
    supply_secured <-
        select(
            supply_input,
            c(
                "ISO3",
                "Secured.and.or.Expected.Vaccine..millions.of.courses.",
                "Bilateral.Deals..millions.of.courses.",
                "Bilateral.Donations..millions.of.courses.",
                "COVAX.Total..millions.of.courses.",
                "EU.Deal..millions.of.courses.",
                "Other.sources..millions.of.courses.",
                "Domestic.Supply..millions.of.courses."
            )
        )

    print(" >> Selecting and renaming columns...")
    colnames(supply_secured) <-
        c(
            "iso",
            "sec_total",
            "sec_bilat",
            "sec_donat",
            "sec_covax",
            "sec_eu",
            "sec_other",
            "sec_domestic"
        )

    return(supply_secured)
}


transform_supply_secured <- function(supply_secured, dataset_date) {
    ## supply secured

    print(" >> Transforming and cleaning supply secured...")
    multiplier <- 1000000
    columns_to_multiply <- list(
        "sec_total",
        "sec_bilat",
        "sec_donat",
        "sec_covax",
        "sec_other",
        "sec_eu",
        "sec_domestic"
    )
    print(" >> Multiplying by a million...")
    for (c in columns_to_multiply) {
        supply_secured <- mutate(supply_secured, UQ(rlang::sym(c)) :=
            UQ(rlang::sym(c)) * multiplier)
    }

    # FIXME replacing NA with 0 is NOT a good idea for data repr
    print(" >> Replacing NAs...")
    supply_secured <- supply_secured %>% replace(is.na(.), 0)
    print(" >> Rounding...")
    supply_secured <- supply_secured %>% mutate_if(is.numeric, round)

    # consolidate other secured sources for visualization
    print(" >> Summing others...")
    supply_secured <- supply_secured %>%
        mutate(sec_other_sum = sec_eu + sec_other + sec_domestic)

    print(" >> Adding dataset date...")
    supply_secured$sec_date <- as.Date(dataset_date) # as.Date("2022-01-20")
    # FIXME do I need to pass in date here?
    return(supply_secured)
}