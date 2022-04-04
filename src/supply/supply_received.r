
load_sup_rec <- function() {

    # Load current, lm, and 2m datasets
    print(" >> Reading data...")
    b_mdb <-
        data.frame(read_excel("data/_input/base_supply_received_current.xlsx",
            sheet = "Delivery_Table"
        ))

    b_mdb_lm <-
        data.frame(read_excel("data/_input/base_supply_received_lastmonth.xlsx",
            sheet = "Delivery_Table"
        ))

    b_mdb_2m <-
        data.frame(read_excel("data/_input/base_supply_received_twomonth.xlsx",
            sheet = "Delivery_Table"
        ))

    print(" >> Treating datasets...")

    b_mdb <- treat_country_name_datasource(b_mdb)

    # TODO should this be DRY as well?
    # TODO is there a way to optimise/automate this last and 2m creation?
    common_columns <- c("iso", "product", "total")

    b_mdb_lm <- treat_country_name_datasource(b_mdb_lm)
    b_mdb_lm <- b_mdb_lm %>% select(
        common_columns
    )
    colnames(b_mdb_lm)[3] <- "total_lm"

    b_mdb_2m <- treat_country_name_datasource(b_mdb_2m)
    b_mdb_2m <- b_mdb_2m %>% select(
        common_columns
    )
    colnames(b_mdb_2m)[3] <- "total_2m"

    # FIXME hardcoded date warning!
    b_mdb$del_date <- as.Date("2022-03-28")

    supply_received <- helper_join_dataframe_list(
        list(b_mdb, b_mdb_lm, b_mdb_2m),
        join_by = c("iso", "product")
    )

    return(supply_received)
}

transform_sup_rec_doses <- function(supply_received) {
    print(" >> Grouping all doses...")
    supply_received_doses <- supply_received %>%
        group_by(iso) %>%
        summarize_at(
            c(
                "bimultilat",
                "donations",
                "covax",
                "avat",
                "unknown",
                "total",
                "total_lm",
                "total_2m"
            ),
            sum,
            na.rm = TRUE
        )

    colnames(supply_received_doses) <- c(
        "iso",
        "del_dose_bilat",
        "del_dose_donat",
        "del_dose_covax",
        "del_dose_avat",
        "del_dose_unkwn",
        "del_dose_total",
        "del_dose_total_lm",
        "del_dose_total_2m"
    )

    print(" >> Calculate doses delivered
                since last and previous two months...")
    supply_received_doses <- supply_received_doses %>%
        mutate(del_dose_since_lm = del_dose_total - del_dose_total_lm)

    supply_received_doses <- supply_received_doses %>%
        mutate(del_dose_prior_2m = del_dose_total_2m)

    supply_received_doses <- supply_received_doses %>%
        mutate(del_dose_lm_2m = del_dose_total_lm - del_dose_total_2m) %>%
        
        mutate(del_dose_wast = del_dose_total * 0.1)


    return(supply_received_doses)
}


transform_sup_rec_product <- function(supply_received) {
    print(" >> Grouping doses per product...")
    supply_received_by_product <- supply_received %>%
        group_by(
            iso, product
        ) %>%
        summarise_at(
            c(
                "bimultilat",
                "donations",
                "covax",
                "avat",
                "unknown",
                "total"
            ),
            sum,
            na.rm = TRUE
        )

    print(" >> Renaming products for visualization...")

    supply_received_by_product$product_short <- helper_replace_values_with_map(
        data = supply_received_by_product$product,
        values = c(
            "Gamaleya - Sputnik V",
            "Bharat - Covaxin",
            "CanSino - Ad5-nCOV",
            "Gamaleya - Sputnik Light",
            "Chumakov - Covi-Vac",
            "RIBSP - QazCovid",
            "SRCVB - EpiVacCorona",
            "Medigen - MVC-COV1901",
            "Anhui ZL - Recombinant SARS-CoV-2 vaccine",
            "Sinopharm (Wuhan) - Inactivated",
            "CIGB - CIGB-66",
            "BBIL - Covaxin",
            "CIGB - Abdala",
            "Soberana 2",
            "AstraZeneca - Vaxzevria",
            "SII - Covishield",
            "Pfizer BioNTech - Comirnaty",
            "Sinopharm (Beijing) - BBIBP-CorV",
            "Moderna - Spikevax",
            "Sinovac - CoronaVac",
            "Janssen - Ad26.COV 2.S",
            "SII - Covavax",
            "Unknown"
        ),
        map = c(
            rep("Non-COVAX product", 14),
            "AZ",
            "SII",
            "Pfizer",
            "Sinopharm",
            "Moderna",
            "Sinovac",
            "J&J",
            "Novavax",
            "Unknown"
        )
    )

    colnames(supply_received_by_product) <-
        c(
            "a_iso",
            "product",
            "bimultilat",
            "donations",
            "covax",
            "avat",
            "unknown",
            "total",
            "product_short"
        )
    
    c_delivery_product <- select(c_delivery_doses_product, c("a_iso","product_short","total"))
    c_delivery_product <- filter(c_delivery_product, product_short == "J&J")
    c_delivery_product <- select(c_delivery_product, -c("product_short"))
    colnames(c_delivery_product) <- c("a_iso","del_dose_jj")

    return(supply_received_by_product)
}

treat_country_name_datasource <- function(dataframe) {
    print(" >>> Adding ISO codes...")
    dataframe$iso <- countrycode(
        dataframe$Country.territory,
        origin = "country.name",
        destination = "iso3c",
        warn = TRUE
    )


    print(" >>> Handling special cases...")
    # handling special cases
    # TODO Hong Kong and Macao?
    dataframe <- dataframe %>%
        mutate(iso = replace(iso, Country.territory == "Kosovo", "XKX"))
    dataframe <- dataframe[!(is.na(dataframe$iso)), ]
    dataframe <- select(dataframe, -c("Country.territory"))

    colnames(dataframe) <- c(
        "product",
        "bimultilat",
        "donations",
        "covax",
        "avat",
        "unknown",
        "total",
        "iso"
    )

    return(dataframe)
}


sr_filter_and_rename <- function(df, suffix) {
    columns <- c(
        "bimultilat",
        "donations",
        "covax",
        "avat",
        "unknown",
        "total",
        "total_lm",
        "total_2m"
    )

    df <- select(
        df, c(
            "iso",
            "del_date",
            columns
        )
    )

    new_columns <- helper_tr_add_suffix_to_list(columns, suffix = suffix)
    colnames(df) <- c("iso", "del_date", new_columns)

    repl <- as.list(rep(0, length(new_columns)))
    names(repl) <- new_columns

    df <- df %>% replace_na(repl)

    return(df)
}


eda_sup_rec_courses <- function(supply_received, supply_received_doses) {
    print(" >> Building one dose dataset...")
    one_dose_delivery_courses <- sr_filter_and_rename(
        filter(supply_received, product == "Janssen - Ad26.COV 2.S"),
        suffix = "_1d"
    )

    print(" >> Calculating one dose wastage...")
    one_dose_delivery_courses <- one_dose_delivery_courses %>%
        mutate(wast_1d = total_1d * 0.1)

    print(" >> Building two dose dataset...")
    two_dose_delivery_courses <- sr_filter_and_rename(
        filter(supply_received, product != "Janssen - Ad26.COV 2.S"),
        suffix = "_2d"
    )

    two_dose_delivery_courses <- two_dose_delivery_courses %>%
        group_by(iso) %>%
        summarise_at(
            c(
                "bimultilat_2d",
                "donations_2d",
                "covax_2d",
                "avat_2d",
                "unknown_2d",
                "total_2d",
                "total_lm_2d",
                "total_2m_2d"
            ),
            sum,
            na.rm = TRUE
        )


    print(" >> Calculating two dose wastage...")
    two_dose_delivery_courses <- two_dose_delivery_courses %>%
        mutate(wast_2d = total_2d * 0.1)

    # FIXME make this dry
    two_dose_delivery_courses <- two_dose_delivery_courses %>%
        mutate(bimultilat_2d = bimultilat_2d / 2) %>%
        mutate(donations_2d = donations_2d / 2) %>%
        mutate(covax_2d = covax_2d / 2) %>%
        mutate(avat_2d = avat_2d / 2) %>%
        mutate(unknown_2d = unknown_2d / 2) %>%
        mutate(total_2d = total_2d / 2) %>%
        mutate(wast_2d = wast_2d / 2) %>%
        mutate(total_lm_2d = total_lm_2d / 2) %>%
        mutate(total_2m_2d = total_2m_2d / 2)

    delivery_couses <- left_join(two_dose_delivery_courses,
        one_dose_delivery_courses,
        by = "iso"
    )

    delivery_couses <- delivery_couses %>%
        mutate(across(-c(del_date, iso), ~ replace_na(., 0)))

    print(" >> Adding 1-dose and 2-dose courses...")
    delivery_couses <- delivery_couses %>%
        mutate(del_cour_bilat = bimultilat_1d + bimultilat_2d) %>%
        mutate(del_cour_donat = donations_1d + donations_2d) %>%
        mutate(del_cour_covax = covax_1d + covax_2d) %>%
        mutate(del_cour_avat = avat_1d + avat_2d) %>%
        mutate(del_cour_unkwn = unknown_1d + unknown_2d) %>%
        mutate(del_cour_total = total_1d + total_2d) %>%
        mutate(del_cour_wast = wast_1d + wast_2d) %>%
        mutate(del_cour_total_lm = total_lm_1d + total_lm_2d) %>%
        mutate(del_cour_total_2m = total_2m_1d + total_2m_2d)

    delivery_couses <- delivery_couses %>%
        mutate_if(is.numeric, round)

    delivery_couses <-
        select(
            delivery_couses,
            c(
                "iso",
                "del_date",
                "del_cour_bilat",
                "del_cour_donat",
                "del_cour_covax",
                "del_cour_avat",
                "del_cour_unkwn",
                "del_cour_total",
                "del_cour_wast",
                "del_cour_total_lm",
                "del_cour_total_2m"
            )
        )

    ## Calculate courses delivered since last month and two month
    delivery_couses <- delivery_couses %>%
        mutate(del_cour_since_lm = del_cour_total - del_cour_total_lm)

    delivery_couses <- delivery_couses %>%
        mutate(del_cour_prior_2m = del_cour_total_2m)

    delivery_couses <- delivery_couses %>%
        mutate(del_cour_lm_2m = del_cour_total_lm - del_cour_total_2m)

    delivery_courses_doeses <- left_join(supply_received_doses,
        delivery_couses,
        by = "iso"
    )

    return(delivery_courses_doeses)
}