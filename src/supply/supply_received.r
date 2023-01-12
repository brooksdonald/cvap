
load_sup_rec <- function() {

    # Load current, lm, and 2m datasets
    print(" >> Reading data...")
    b_mdb <-
        data.frame(read_excel("data/input/base_supply_received_current.xlsx",
            sheet = "Delivery_Table"
        ))

    b_mdb_lm <-
        data.frame(read_excel("data/input/base_supply_received_lastmonth.xlsx",
            sheet = "Delivery_Table"
        ))

    b_mdb_2m <-
        data.frame(read_excel("data/input/base_supply_received_twomonth.xlsx",
            sheet = "Delivery_Table"
        ))

    b_mdb_13jan <-
        data.frame(read_excel("data/input/static/base_supply_weekof13jan.xlsx",
            sheet = "Delivery_Table"
        ))

    print(" >> Treating datasets...")

    b_mdb <- treat_country_name_datasource(b_mdb)

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

    b_mdb_13jan <- treat_country_name_datasource(b_mdb_13jan)
    b_mdb_13jan <- b_mdb_13jan %>% select(
      common_columns
    )
    colnames(b_mdb_13jan)[3] <- "total_13jan"


    supply_received <- helper_join_dataframe_list(
        list(b_mdb, b_mdb_lm, b_mdb_2m, b_mdb_13jan),
        join_by = c("iso", "product")
    )
    for (col in c(
        "bimultilat",
        "donations",
        "covax",
        "avat",
        "unknown",
        "total",
        "total_lm",
        "total_2m",
        "total_13jan"
    )
    ) {
        supply_received[, col] <- as.numeric(supply_received[, col])
    }
    return(supply_received)
}

transform_sup_rec_doses <- function(supply_received, del_date) {
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
                "total_2m",
                "total_13jan"
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
        "del_dose_total_2m",
        "del_dose_total_13jan"
    )

    print(" >> Calculate doses delivered since last and previous two months...")
    supply_received_doses <- supply_received_doses %>%
        mutate(del_dose_since_lm = pmax(del_dose_total - del_dose_total_lm, 0))

    supply_received_doses <- supply_received_doses %>%
        mutate(del_dose_prior_2m = del_dose_total_2m)

    supply_received_doses <- supply_received_doses %>%
        mutate(del_dose_lm_2m = pmax(del_dose_total_lm - del_dose_total_2m, 0)) %>%
        mutate(del_dose_lm_13jan = pmax(del_dose_total_lm - del_dose_total_13jan, 0)) %>%
        mutate(del_dose_wast = del_dose_total * 0.1)

    # Introducing del_date to supply_received_doses
    supply_received_doses$del_date <- del_date

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
            "CanSino - CONVIDECIA",
            "CovIran Barekat",
            "SK Bio - SKYCovione",
            "Anhui Zhifei - Zifivax",
            "BBIL - Covaxin",
            "Soberana 2",
            "Turkovac",
            "Medigen - MVC-COV1901",
            "Gamaleya - Sputnik Light",
            "SII - Covavax",
            "Novavax - Nuvaxovid",
            "AstraZeneca - Vaxzevria",
            "SII - Covishield",
            "Sinovac - CoronaVac",
            "Janssen - Ad26.COV 2.S",
            "Moderna - Spikevax",
            "Pfizer BioNTech - Comirnaty",
            "Sinopharm (Beijing) - BBIBP-CorV",
            "Unknown"
        ),
        map = c(
            rep("Non-COVAX product", 9),
            rep("Novavax", 2),
            rep("AZ", 2),
            "Sinovac",
            "J&J",
            "Moderna",
            "Pfizer",
            "Sinopharm",
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
    c_delivery_product <- select(
        supply_received_by_product, c("a_iso", "product_short", "total")
    )
    c_delivery_product <- filter(c_delivery_product, product_short == "J&J")
    c_delivery_product <- select(c_delivery_product, -c("product_short"))
    colnames(c_delivery_product) <- c("a_iso", "del_dose_jj")

    datalist <- list("supply_received_by_product" = supply_received_by_product,
    "c_delivery_product" = c_delivery_product)
    return(datalist)
}

treat_country_name_datasource <- function(dataframe) {
    print(" >>> Adding ISO codes...")
    dataframe$iso <- countrycode(
        dataframe$Country.territory,
        origin = "country.name",
        destination = "iso3c",
        warn = FALSE
    )


    print(" >>> Handling special cases...")
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
        "total_2m",
        "total_13jan"
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
    c_delivery_courses <- supply_received %>%
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
                "total_2m",
                "total_13jan"
            ),
            sum,
            na.rm = TRUE
        )

    colnames(c_delivery_courses) <- c(
        "iso",
        "bimultilat",
        "donations",
        "covax",
        "avat",
        "unknown",
        "total",
        "total_lm",
        "total_2m",
        "total_13jan"
    )

    c_delivery_courses <- c_delivery_courses[!(is.na(c_delivery_courses$iso)), ]


    print(" >> Calculate estimated course wastage")
    c_delivery_courses <- c_delivery_courses %>%
        mutate(wast = total * 0.1)

    print(" >> Divide by 2 to calculate number courses")
    c_delivery_courses <- c_delivery_courses %>%
        mutate(del_cour_bilat = bimultilat / 2) %>%
        mutate(del_cour_donat = donations / 2) %>%
        mutate(del_cour_covax = covax / 2) %>%
        mutate(del_cour_avat = avat / 2) %>%
        mutate(del_cour_unkwn = unknown / 2) %>%
        mutate(del_cour_total = total / 2) %>%
        mutate(del_cour_wast = wast / 2) %>%
        mutate(del_cour_total_lm = total_lm / 2) %>%
        mutate(del_cour_total_2m = total_2m / 2) %>%
        mutate(del_cour_total_13jan = total_13jan)

    print(" >> Replace NAs with 0 and round")
    # c_delivery_courses <- replace_na(c_delivery_courses, everything(), 0)
    c_delivery_courses <- c_delivery_courses %>%
        mutate(across(.cols = c(everything(), -"iso"), ~ replace_na(., 0)))

    c_delivery_courses <- c_delivery_courses %>%
        mutate_if(is.numeric, round)

    c_delivery_courses <- select(
        c_delivery_courses,
        c(
            "iso",
            "del_cour_bilat",
            "del_cour_donat",
            "del_cour_covax",
            "del_cour_avat",
            "del_cour_unkwn",
            "del_cour_total",
            "del_cour_wast",
            "del_cour_total_lm",
            "del_cour_total_2m",
            "del_cour_total_13jan"
        )
    )

    print(" >> Calculate courses delivered since last month and two month")
    c_delivery_courses <- c_delivery_courses %>%
        mutate(del_cour_since_lm = del_cour_total - del_cour_total_lm)

    c_delivery_courses <- c_delivery_courses %>%
        mutate(del_cour_prior_2m = del_cour_total_2m)

    c_delivery_courses <- c_delivery_courses %>%
        mutate(del_cour_lm_2m = del_cour_total_lm - del_cour_total_2m)
    
    c_delivery_courses <- c_delivery_courses %>%
      mutate(del_cour_lm_13jan = del_cour_total_lm - del_cour_total_13jan)

    print(" >> Combine doses and courses delivered tables")
    c_delivery <- left_join(supply_received_doses, c_delivery_courses, by = "iso")

    print(" >> Renaming delivery_courses_doses iso to a_iso")

    colnames(c_delivery)[1] <- c("a_iso")
    return(c_delivery)
}