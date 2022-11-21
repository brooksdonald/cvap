
load_supply_received <- function() {
    print(" >> Loading supply received datasets...")
    base_supply_current <- data.frame(
      read_excel(
        "data/input/base_supply_received_current.xlsx",
        sheet = "Delivery_Table"
        )
      )

    base_supply_lm <- data.frame(
      read_excel(
        "data/input/base_supply_received_lastmonth.xlsx",
        sheet = "Delivery_Table"
        )
      )

    base_supply_2m <- data.frame(
      read_excel(
        "data/input/base_supply_received_twomonth.xlsx",
        sheet = "Delivery_Table"
        )
      )
    
    base_supply_13jan <- data.frame(
      read_excel(
        "data/input/static/base_supply_weekof13jan.xlsx",
        sheet = "Delivery_Table"
        )
      )

    print(" >> Transforming supply received datasets...")
    base_supply_current <- helper_iso_countryname(base_supply_current)

    common_columns <- c("iso", "product", "total")

    base_supply_lm <- helper_iso_countryname(base_supply_lm)
    base_supply_lm <- base_supply_lm %>% select(
      common_columns
      )
    
    colnames(base_supply_lm)[3] <- "total_lm"

    base_supply_2m <- helper_iso_countryname(base_supply_2m)
    base_supply_2m <- base_supply_2m %>% select(
        common_columns
    )
    colnames(base_supply_2m)[3] <- "total_2m"

    base_supply_13jan <- helper_iso_countryname(base_supply_13jan)
    base_supply_13jan <- base_supply_13jan %>% select(
      common_columns
    )
    colnames(base_supply_13jan)[3] <- "total_13jan"


    base_supply_received <- helper_join_dataframe_list(
      list(base_supply_current, base_supply_lm, base_supply_2m, base_supply_13jan),
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
      base_supply_received[, col] <- as.numeric(base_supply_received[, col])
      }
    
    print(" >> Function 'load_supply_received' done")
    return(base_supply_received)
}

transform_supply_doses <- function(base_supply_received, del_date) {
    print(" >> Consolidating by supply source...")
    supply_received_doses <- base_supply_received %>%
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
        sum, na.rm = TRUE
        )

    print(" >> Renaming columns... ")
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

    print(" >> Calculating doses delivered since last and previous two months...")
    supply_received_doses <- supply_received_doses %>% 
      mutate(
        del_dose_since_lm = pmax(del_dose_total - del_dose_total_lm, 0),
        del_dose_prior_2m = del_dose_total_2m,
        del_dose_lm_2m = pmax(del_dose_total_lm - del_dose_total_2m, 0),
        del_dose_lm_13jan = pmax(del_dose_total_lm - del_dose_total_13jan, 0),
        del_dose_wast = del_dose_total * 0.1
        )
    
    print(" >> Adding del_date...")
    supply_received_doses$del_date <- del_date
    
    print(" >> Function 'transform_supply_doses' done")
    return(supply_received_doses)
}

transform_supply_product <- function(base_supply_received) {
    print(" >> Consolidating by product...")
    supply_received_product <- base_supply_received %>%
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
        sum, na.rm = TRUE
        )

    print(" >> Renaming products for visualization...")
    supply_received_product$product_short <- helper_replace_values_with_map(
      data = supply_received_product$product,
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

    print(" >> Renaming columns... ")
    colnames(supply_received_product) <-
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
    
    print(" >> Consolidating J&J doses received...")
    supply_received_jj <- supply_received_product %>%
      select(a_iso,
             product_short,
             total) %>%
      filter(product_short == "J&J") %>%
      select(-product_short)
    
    print(" >> Renaming columns... ")
    colnames(supply_received_jj) <- 
      c(
        "a_iso", 
        "del_dose_jj"
        )

    supply_received_df <- list(
      "supply_received_product" = supply_received_product,
      "supply_received_jj" = supply_received_jj)
    
    print(" >> Function 'transform_supply_product' done")    
    return(supply_received_df)
}

transform_supply_courses <- function(base_supply_received, supply_received_doses) {
    print(" >> Consolidating by country...")
    supply_received_courses <- base_supply_received %>%
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
        sum, na.rm = TRUE
        )
  
    print(" >> Renaming columns... ")
    colnames(supply_received_courses) <- 
        c(
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

    supply_received_courses <- supply_received_courses[!(is.na(supply_received_courses$iso)), ]

    print(" >> Calculating estimated course wastage")
    supply_received_courses <- supply_received_courses %>%
      mutate(wast = total * 0.1,
             del_cour_bilat = bimultilat / 2,
             del_cour_donat = donations / 2,
             del_cour_covax = covax / 2,
             del_cour_avat = avat / 2,
             del_cour_unkwn = unknown / 2,
             del_cour_total = total / 2,
             del_cour_wast = wast / 2,
             del_cour_total_lm = total_lm / 2,
             del_cour_total_2m = total_2m / 2,
             del_cour_total_13jan = total_13jan)

    print(" >> Replacing NAs with 0 and round")
    supply_received_courses <- supply_received_courses %>%
      mutate(across(.cols = c(everything(), -"iso"), ~ replace_na(., 0))) %>%
      mutate_if(is.numeric, round) %>%
      select(
        iso,
        del_cour_bilat,
        del_cour_donat,
        del_cour_covax,
        del_cour_avat,
        del_cour_unkwn,
        del_cour_total,
        del_cour_wast,
        del_cour_total_lm,
        del_cour_total_2m,
        del_cour_total_13jan
        )

    print(" >> Calculating courses delivered since last month and two month")
    supply_received_courses <- supply_received_courses %>%
      mutate(del_cour_since_lm = del_cour_total - del_cour_total_lm,
             del_cour_prior_2m = del_cour_total_2m,
             del_cour_lm_2m = del_cour_total_lm - del_cour_total_2m,
             del_cour_lm_13jan = del_cour_total_lm - del_cour_total_13jan)

    print(" >> Combining doses and courses delivered tables")
    supply_received <- left_join(supply_received_doses, supply_received_courses, by = "iso")

    print(" >> Renaming delivery_courses_doses iso to a_iso")
    colnames(supply_received)[1] <- c("a_iso")
    
    print(" >> Function 'transform_supply_courses' done")
    return(supply_received)
}

helper_iso_countryname <- function(dataframe) {
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
  
  print(" >>> Renaming columns... ")
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
  
  print(" >> Function 'helper_iso_countryname' done")
  return(dataframe)
}
