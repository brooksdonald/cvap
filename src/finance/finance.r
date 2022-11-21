
load_finance <- function() {
    print(" >> Loading financing data...")
    base_fin <- data.frame(
      read_excel(
        "data/input/base_financing.xlsx",
        sheet = "Data Structure"
        )
      )
      
    print(" >> Selecting relevant columns...")
    b_fin_funding <- select(
      base_fin, c(
        "ISO.Code",
        "Recipient.Type",
        "Information.Type",
        "Allocation.Type",
        "Funding.Amount",
        "Funding.Source.Type.2",
        "Funding.Source",
        "Double.Counting",
        "Commitments",
        "Disbursements",
        "FA"
        )
      )
      
    print(" >> Renaming columns... ")
    colnames(b_fin_funding) <- c(
      "a_iso",
      "recipient",
      "information_type",
      "allocation_type",
      "fund_total",
      "funding_source",
      "funder",
      "double_count",
      "fund_committed",
      "fund_disbursed",
      "fun_add"
      )
      
    print(" >> Function 'load_finance' done")
    return(b_fin_funding)
}

load_finance_urgent <- function() {
    print(" >> Loading urgent financing data...")
    base_fin_urg_fun <- data.frame(
      read_excel(
        "data/input/base_financing_urgent.xlsx",
        sheet = "Funding tracker"
        )
      )
    
    print(" >> Selecting relevant columns...")
    base_fin_urg_fun <- select(
      base_fin_urg_fun, c(
        "ISO",
        "Amount.to.be.funded",
        "Funds.disbursed..yes...no.",
        "Source.of.funding.identified..Details.",
        "Details.on.the.request",
        "Status",
        "Sources.of.funding.identified..yes...no...Identified.as.not.urgent.",
        "Utilization.rate"
        )
      )
    
    print(" >> Renaming columns...")
    colnames(base_fin_urg_fun) <- c(
      "a_iso",
      "fund_urg_dis_total",
      "fund_urg_status",
      "fund_urg_source",
      "fund_urg_details",
      "fund_urg_source_overall",
      "fund_urg_status_req",
      "fund_urg_util"
    )
    
    print(" >> Function 'load_finance_urgent' done")
    return(base_fin_urg_fun)
}

load_finance_cds <- function(entity_characteristics) {
    print(" >> Loading Gavi CDS financing data...")
    base_fin_cds <- data.frame(
      read_excel(
        "data/input/base_financing_cds.xlsx",
        sheet = "Gavi CDS "
        )
      )
    
    print(" >> Selecting relevant columns...")
    base_fin_cds_red <- select(
      base_fin_cds,
      c(
        "ISO",
        "Comment",
        "Date.of.First.Disbursement",
        "Amount.approved",
        "Amount.disbursed",
        "Funding.type"
      )
    )
    
    print(" >> Renaming columns...")
    colnames(base_fin_cds_red) <- c(
      "a_iso",
      "fund_cds_details",
      "fund_cds_date",
      "fund_cds_amount_approved",
      "fund_cds_amount_disbursed",
      "fund_cds_type"
    )
    
    print(" >> Joining with 'entity_characteristics' data...")
    base_fin_cds_red <- left_join(
      base_fin_cds_red,
      entity_characteristics,
      by = "a_iso"
    )
    
    print(" >> Modifying 'fund_cds_date' variable...")
    base_fin_cds_red$fund_cds_date <- as.character(base_fin_cds_red$fund_cds_date)
    base_fin_cds_red <- base_fin_cds_red %>%
      mutate(fund_cds_date = if_else(
        is.na(fund_cds_date) & is.na(fund_cds_details) == FALSE,
        "Pending",
        fund_cds_date))
    
    print(" >> Function 'load_finance_cds' done")
    return(base_fin_cds_red)
}


transform_finance <- function(b_fin_funding, entity_characteristics) {
    print(" >> Filtering financing data...")
    b_fin_fund_del <- filter(b_fin_funding,
                             recipient == "Country" &
                             information_type == "Funding Information" &
                             double_count == "Keep" &
                             allocation_type == "Vaccine Delivery" &
                             is.na(fund_total) == FALSE
                             & fund_total != 0
                           )

    print(" >> Modifying 'funding_source' variable...")
    b_fin_fund_del <- b_fin_fund_del %>%
      mutate(funding_source = if_else(
        funding_source == "Foundations/Private",
        "Foundations / private",
        funding_source
        )
    )
    
    print(" >> Modifying 'funder' variable...")
    b_fin_fund_del$funder <- helper_replace_values_with_map(
        data = b_fin_fund_del$funder,
        values = c(
            "Japan - Ministry of Foreign Affairs",
            "UNICEF (Thematic/Flexible Funding) HAC)",
            "Inter-American Development Bank",
            "Germany - Federal Foreign Office (AA)",
            "Governm ent of France - Gavi",
            "Government of Ireland",
            "Asian Development Bank",
            "Bill and Melinda Gates Foundation"
        ),
        map = c(
            "Japan MoFA",
            "UNICEF HAC",
            "IADB",
            "Germany FFO",
            "France - Gavi",
            "Ireland",
            "ADB",
            "BMGF"
        ),
        drop_rest = FALSE
    )
    
    print(" >> Generating delivered funding summary data...")
    b_fin_fund_del_sum <- b_fin_fund_del %>%
      group_by(a_iso) %>%
      summarize_at("fund_total", sum, na.rm = TRUE) %>%
      mutate_if(is.numeric, round)

    print(" >> Generating delivered funding by source data...")
    b_fin_fund_del_source <- b_fin_fund_del %>%
      group_by(a_iso, funding_source, funder) %>%
      summarize_at(c("fund_total","fund_disbursed","fund_committed"),
                   sum,
                   na.rm = TRUE) %>%
      mutate_if(is.numeric, round)
    
    print(" >> Generating delivered funding by source data variables...")
    b_fin_fund_del_source <- b_fin_fund_del_source %>%
      mutate(fund_comitted_per = fund_committed / fund_total) %>%
      mutate(fund_disbursed_per = fund_disbursed / fund_total)
    
    print(" >> Generating committed funding data...")
    b_fin_fund_del_committed <- b_fin_fund_del %>%
      group_by(a_iso, funding_source, funder) %>%
      summarize_at("fund_committed", sum, na.rm = TRUE) %>%
      mutate_if(is.numeric, round)
    
    b_fin_fund_del_committed$type <- "Committed"
    
    names(b_fin_fund_del_committed)[names(b_fin_fund_del_committed) == 'fund_committed'] <- 'value'
    
    print(" >> Generating disbursed funding data...")
    b_fin_fund_del_disbursed <- b_fin_fund_del %>%
      group_by(a_iso, funding_source, funder) %>%
      summarize_at("fund_disbursed", sum, na.rm = TRUE) %>%
      mutate_if(is.numeric, round)
    
    b_fin_fund_del_disbursed$type <- "Disbursed"
    
    names(b_fin_fund_del_disbursed)[names(b_fin_fund_del_disbursed) == 'fund_disbursed'] <- 'value'
    
    print(" >> Binding committed and disbursed funding data...")
    b_fin_fund_del_long <- bind_rows(b_fin_fund_del_committed, b_fin_fund_del_disbursed)
  
    print(" >> Joining committed and disbursed funding data with entity characteristics...")
    b_fin_fund_del_long <- left_join(
      b_fin_fund_del_long,
      entity_characteristics,
      by = "a_iso"
    )
    
    print(" >> Joining delivered funding by source data with entity characteristics...")
    b_fin_fund_del_source <- left_join(
        b_fin_fund_del_source,
        entity_characteristics,
        by = "a_iso"
    )
    
    print(" >> Adding data to datalist...")
    datalist <- list("b_fin_fund_del_source" = b_fin_fund_del_source,
                      "b_fin_fund_del_sum" = b_fin_fund_del_sum,
                      "b_fin_fund_del_long" = b_fin_fund_del_long)
    
    print(" >> Function 'transform_finance' done")
    return(datalist)
}



transform_finance_urgent <- function(base_fin_urg_fun, entity_characteristics) {
    print(" >> Filtering urgent financing data...")
    base_fin_urg_fun <- filter(base_fin_urg_fun, fund_urg_status == "yes")
  
    print(" >> Modifying 'fund_urg_dis_total' variable...")
    base_fin_urg_fun$fund_urg_dis_total <- as.numeric(base_fin_urg_fun$fund_urg_dis_total)
  
    print(" >> Joining urgent financing data with entity characteristics...")
    base_fin_urg_fun_long <- left_join(
      base_fin_urg_fun,
      entity_characteristics,
      by = "a_iso"
      )

    print(" >> Generating urgent financing summary data...")
    base_fin_urg_fun_sum <- base_fin_urg_fun %>%
      group_by(a_iso, fund_urg_source, fund_urg_util) %>%
      summarize_at("fund_urg_dis_total", sum, na.rm = TRUE)

    print(" >> Joining urgent financing summary data with entity characteristics...")
    base_fin_urg_fun_sum <- left_join(
      base_fin_urg_fun_sum,
      entity_characteristics,
      by = "a_iso"
    )
    
    print(" >> Adding data to datalist...")
    datalist <- list("base_fin_urg_fun_long" = base_fin_urg_fun_long,
                     "base_fin_urg_fun_sum" = base_fin_urg_fun_sum)
  
  print(" >> Function 'transform_finance_urgent' done")
  return(datalist)
}
