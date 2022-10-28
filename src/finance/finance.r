# Load finance data

load_finance_data <- function() {
    print(" >> Loading financing data...")
    base_fin <- data.frame(
        read_excel("data/input/base_financing.xlsx",
        sheet = "Data Structure"
        )
    )
    print(" >> Select relevant columns from base_fin & renaming...")
    b_fin_funding <- select(
        base_fin,
        c(
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
    return(b_fin_funding)
}

transform_finance_data <- function(b_fin_funding, entity_characteristics) {
    b_fin_funding <- filter(b_fin_funding, recipient == "Country")
    b_fin_funding <- filter(b_fin_funding, information_type == "Funding Information")
    b_fin_funding <- filter(b_fin_funding, double_count == "Keep")

    b_fin_fund_del <- filter(
        b_fin_funding,
        allocation_type == "Vaccine Delivery" &
        is.na(fund_total) == FALSE
        & fund_total != 0
    )

    b_fin_fund_del <- b_fin_fund_del %>%
    mutate(funding_source = if_else(
        funding_source == "Foundations/Private",
        "Foundations / private",
        funding_source
        )
    )
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
    b_fin_fund_del_sum <- b_fin_fund_del %>%
    group_by(a_iso) %>%
    summarize_at("fund_total", sum, na.rm = TRUE) %>%
    mutate_if(is.numeric, round)

    b_fin_fund_del_source <- b_fin_fund_del %>%
    group_by(a_iso, funding_source, funder) %>%
    summarize_at(c("fund_total","fund_disbursed","fund_committed"),
      sum,
      na.rm = TRUE) %>%
    mutate_if(is.numeric, round)
    
    b_fin_fund_del_source <- b_fin_fund_del_source %>%
      mutate(fund_comitted_per = fund_committed / fund_total) %>%
      mutate(fund_disbursed_per = fund_disbursed / fund_total)
    
    b_fin_fund_del_long <- b_fin_fund_del %>%
    group_by(a_iso, funding_source, funder) %>%
    summarize_at("fund_committed", sum, na.rm = TRUE) %>%
    mutate_if(is.numeric, round)
    
    b_fin_fund_del_long$type <- "Committed"
    b_fin_fund_del_long <- b_fin_fund_del_long %>%
      mutate(value = fund_committed)
    b_fin_fund_del_long <- select(b_fin_fund_del_long, -c("fund_committed"))
    
    b_fin_fund_del_long_temp <- b_fin_fund_del %>%
      group_by(a_iso, funding_source, funder) %>%
      summarize_at("fund_disbursed", sum, na.rm = TRUE) %>%
      mutate_if(is.numeric, round)
    
    b_fin_fund_del_long_temp$type <- "Disbursed"
    b_fin_fund_del_long_temp <- b_fin_fund_del_long_temp %>%
      mutate(value = fund_disbursed)
    b_fin_fund_del_long_temp <- select(b_fin_fund_del_long_temp, -c("fund_disbursed"))
    
    b_fin_fund_del_long <- bind_rows(b_fin_fund_del_long, b_fin_fund_del_long_temp)
  
    b_fin_fund_del_long <- left_join(
      b_fin_fund_del_long,
      entity_characteristics,
      by = "a_iso"
    )
    b_fin_fund_del_source <- left_join(
        b_fin_fund_del_source,
        entity_characteristics,
        by = "a_iso"
    )
    datalist <- list("b_fin_fund_del_source" = b_fin_fund_del_source,
      "b_fin_fund_del_sum" = b_fin_fund_del_sum,
      "b_fin_fund_del_long" = b_fin_fund_del_long)
    return(datalist)
}

load_finance_urgent_data <- function() {
  print(" >> Loading CoVDP urgent financing data...")
  base_fin_urg <- data.frame(
    read_excel("data/input/base_financing_urgent.xlsx",
               sheet = "Funding tracker"
    )
  )
  print(" >> Select relevant columns from base_fin_urg & renaming...")
  base_fin_urg_fun <- select(
    base_fin_urg,
    c(
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
  return(base_fin_urg_fun)
}

transform_fund_urgent_data <- function(base_fin_urg_fun, entity_characteristics) {
  base_fin_urg_fun <- filter(base_fin_urg_fun, fund_urg_status == "yes")
  
  base_fin_urg_fun$fund_urg_dis_total <- as.numeric(base_fin_urg_fun$fund_urg_dis_total)
  
  base_fin_urg_fun_long <- base_fin_urg_fun
  
  base_fin_urg_fun_long <- left_join(
    base_fin_urg_fun_long,
    entity_characteristics,
    by = "a_iso"
  )

  base_fin_urg_fun_sum <- base_fin_urg_fun %>%
    group_by(a_iso, fund_urg_source, fund_urg_util) %>%
    summarize_at("fund_urg_dis_total", sum, na.rm = TRUE)

  base_fin_urg_fun_sum <- left_join(
    base_fin_urg_fun_sum,
    entity_characteristics,
    by = "a_iso"
  )
  datalist <- list("base_fin_urg_fun_long" = base_fin_urg_fun_long,
    "base_fin_urg_fun_sum" = base_fin_urg_fun_sum)
  return(datalist)
}


load_finance_cds_data <- function(entity_characteristics) {
  print(" >> Loading Gavi CDS financing data...")
  base_fin_cds <- data.frame(
    read_excel("data/input/base_financing_cds.xlsx",
               sheet = "Status June22"
    )
  )
  print(" >> Select relevant columns from base_fin_cds & renaming...")
  base_fin_cds_red <- select(
    base_fin_cds,
    c(
      "ISO",
      "Comment",
      "Application.approved",
      "Amount.approved",
      "Amount.disbursed",
      "Funding.type"
    )
  )
  colnames(base_fin_cds_red) <- c(
    "a_iso",
    "fund_cds_details",
    "fund_cds_date",
    "fund_cds_amount_approved",
    "fund_cds_amount_disbursed",
    "fund_cds_type"
  )
  base_fin_cds_red$fund_cds_date <- as.numeric(base_fin_cds_red$fund_cds_date)
  base_fin_cds_red$fund_cds_date <- as.Date(base_fin_cds_red$fund_cds_date, origin="1899-12-30")
  base_fin_cds_red$fund_cds_date <- as.character(base_fin_cds_red$fund_cds_date)
  
  base_fin_cds_red <- left_join(
    base_fin_cds_red,
    entity_characteristics,
    by = "a_iso"
  )
  
  base_fin_cds_red <- base_fin_cds_red %>%
    mutate(fund_cds_date = if_else(
      is.na(fund_cds_date) & is.na(fund_cds_details) == FALSE,
      "Pending",
      fund_cds_date))
  return(base_fin_cds_red)
}
