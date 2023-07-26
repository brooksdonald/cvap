load_sup_rec <- function() {
  print(">> Loading supply received data...")
  mdb <-
    data.frame(
      read_excel("data/input/base_supply_received_current.xlsx",
                 sheet = "Delivery_Table")
    )
  
  mdb_lm <-
    data.frame(
      read_excel("data/input/base_supply_received_lastmonth.xlsx",
                 sheet = "Delivery_Table")
    )
  
  mdb_2m <-
    data.frame(
      read_excel("data/input/base_supply_received_twomonth.xlsx",
                 sheet = "Delivery_Table")
    )
  
  mdb_13jan <-
    data.frame(
      read_excel("data/input/static/base_supply_weekof13jan.xlsx",
                 sheet = "Delivery_Table")
    )
  
  print(">> Adding iso codes and handling special cases...")
  mdb <- sup_rec_iso(mdb)
  common_columns <- c("iso", "product", "total")
  
  mdb_lm <- sup_rec_iso(mdb_lm)
  mdb_lm <- mdb_lm %>% select(all_of(common_columns))
  colnames(mdb_lm)[3] <- "total_lm"
  
  mdb_2m <- sup_rec_iso(mdb_2m)
  mdb_2m <- mdb_2m %>% select(all_of(common_columns))
  colnames(mdb_2m)[3] <- "total_2m"
  
  mdb_13jan <- sup_rec_iso(mdb_13jan)
  mdb_13jan <- mdb_13jan %>% select(all_of(common_columns))
  colnames(mdb_13jan)[3] <- "total_13jan"
  
  print(">> Consolidating supply data into single dataframe...")
  base_sup_rec <- helper_join_dataframe_list(list(mdb, mdb_lm, mdb_2m, mdb_13jan),
                                                join_by = c("iso", "product"))
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
  )) {
    base_sup_rec[, col] <- as.numeric(base_sup_rec[, col])
  }
  
  print(">> Done.")
  return(base_sup_rec)
}

transform_sup_rec_dose <- function(base_sup_rec, date_del) {
    print(">> Summarizing doses received by country & renaming...")
    sup_rec_dose <- base_sup_rec %>%
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
        ) %>%
      rename(
        iso = iso,
        del_dose_bilat = bimultilat,
        del_dose_donat = donations,
        del_dose_covax = covax,
        del_dose_avat = avat,
        del_dose_unkwn = unknown,
        del_dose_total = total,
        del_dose_total_lm = total_lm,
        del_dose_total_2m = total_2m,
        del_dose_total_13jan = total_13jan
      )

    print(">> Calculating doses received since last and previous two months...")
    sup_rec_dose <- sup_rec_dose %>%
        mutate(del_dose_since_lm = pmax(del_dose_total - del_dose_total_lm, 0),
               del_dose_prior_2m = del_dose_total_2m,
               del_dose_lm_2m = pmax(del_dose_total_lm - del_dose_total_2m, 0),
               del_dose_lm_13jan = pmax(del_dose_total_lm - del_dose_total_13jan, 0),
               del_dose_wast = del_dose_total * 0.1)

    print(">> Adding supply received data as of date...")
    sup_rec_dose$date_del <- date_del

    print(">> Done.")
    return(sup_rec_dose)
}

transform_sup_rec_dose_prod <- function(base_sup_rec) {
    print(">> Summarizing doses received by country and by product...")
    sup_rec_dose_prod <- base_sup_rec %>%
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

    print(">> Applying short-form product names...")
    sup_rec_dose_prod$product_short <- helper_replace_values_with_map(
        data = sup_rec_dose_prod$product,
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
            "Gamaleya - Sputnik V",
            "Valneva - VLA2001",
            "Chumakov - Covi-Vac",
            "Sanofi GSK - VidPrevtyn Beta",
            "Inavac",
            "Razi Cov Pars",
            "RIBSP - QazCovid",
            "CovIran Barekat",
            "CIGB - Abdala",
            "SII - Covavax",
            "Novavax - Nuvaxovid",
            "AstraZeneca - Vaxzevria",
            "SII - Covishield",
            "Sinovac - CoronaVac",
            "Janssen - Ad26.COV 2.S",
            "Moderna - Spikevax",
            "Pfizer BioNTech - Comirnaty",
            "Sinopharm (Beijing) - BBIBP-CorV",
            "Pfizer BioNtech - Comirnaty (Bivalent)",
            "Moderna - Spikevax Bivalent",
            "Unknown"
        ),
        map = c(
            rep("Non-COVAX product", 18),
            rep("Novavax", 2),
            rep("AZ", 2),
            "Sinovac",
            "J&J",
            "Moderna",
            "Pfizer",
            "Sinopharm",
            "Pfizer Bivalent",
            "Moderna Bivalent",
            "Unknown"
        )
    )

    colnames(sup_rec_dose_prod) <-
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
    
    print(">> Preparing J&J-specific received dose counts...")   
    sup_rec_jj <- sup_rec_dose_prod %>%
      select(a_iso,
             product_short,
             total) %>%
      filter(product_short == "J&J") %>%
      select(-product_short) %>%
      rename(del_dose_jj = total)

    datalist <- list(
      "sup_rec_dose_prod" = sup_rec_dose_prod,
      "sup_rec_jj" = sup_rec_jj)

    print(">> Done.")
    return(datalist)
}

transform_sup_rec_cour <- function(sup_rec_dose) {
  print(">> Preparing courses received estimates by country & renaming...")
  sup_rec_cour <- sup_rec_dose %>%
    select(-date_del) %>%
    mutate_if(is.numeric, function(x) x / 2) %>%
    mutate_if(is.numeric, round) %>%
    rename_with(.cols = everything(), .fn = ~str_replace(., "dose", "cour"))
  
  print(">> Merging doses and courses received tables & renaming...")
  sup_rec <- left_join(sup_rec_dose, sup_rec_cour, by = "iso") %>%
    rename(a_iso = iso)
  
  print(">> Done.")
  return(sup_rec)
}

sup_rec_iso <- function(dataframe) {
    print(">>> Adding iso codes...")
    dataframe$iso <- countrycode(
        dataframe$Country.territory,
        origin = "country.name",
        destination = "iso3c",
        warn = FALSE
    )


    print(">>> Handling special cases...")
    dataframe <- dataframe %>%
      mutate(iso = replace(iso, Country.territory == "Kosovo", "XKX")) %>%
      filter(is.na(iso) == FALSE) %>%
      select(-Country.territory)
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
    
    print(">>> Done.")
    return(dataframe)
}
