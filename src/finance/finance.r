
load_finance <- function() {
  print(" >> Loading financing data...")
  base_finance <- data.frame(
    read_excel("data/input/base_financing.xlsx",
               sheet = "Data Structure"
    )
  )
  
  print(" >> Selecting relevant columns...")
  base_finance <- select(
    base_finance, c(
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
  
  print(" >> Renaming columns...")
  colnames(base_finance) <- c(
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
    "fund_add"
  )
  
  print(" >> Function 'load_finance' done")
  return(base_finance)
}


load_finance_urgent <- function() {
  print(" >> Loading funding tracker data...")
  base_finance_urgent <- data.frame(
    read_excel("data/input/base_financing_urgent.xlsx",
               sheet = "Funding tracker"
    )
  )
  
  print(" >> Selecting relevant columns...")
  base_finance_urgent <- select(
    base_finance_urgent, c(
      "ISO.Code",
      "Amount.to.be.funded",
      "Funds.disbursed..yes...no.",
      "Source.of.funding.identified..Details.",
      "Details.on.the.request",
      "Status",
      "Sources.of.funding.identified..yes...no...Identified.as.not.urgent.",
      "Estimated.utilization.rate"
    )
  )
  
  print(" >> Renaming columns...")
  colnames(base_finance_urgent) <- c(
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
  return(base_finance_urgent)
}


load_finance_cds <- function(entity_characteristics) {
  print(" >> Loading CDS data...")
  base_finance_cds <- data.frame(
    read_excel("data/input/base_financing_cds.xlsx",
               sheet = "Gavi CDS "
    )
  )
  
  print(" >> Selecting relevant columns...")
  base_finance_cds <- select(
    base_finance_cds, c(
      "ISO",
      "Comment",
      "Date.of.First.Disbursement",
      "Amount.approved",
      "Amount.disbursed",
      "Funding.type"
    )
  )
  
  print(" >> Renaming columns...")
  colnames(base_finance_cds) <- c(
    "a_iso",
    "fund_cds_details",
    "fund_cds_date",
    "fund_cds_amount_approved",
    "fund_cds_amount_disbursed",
    "fund_cds_type"
  )
  
  base_finance_cds$fund_cds_date <- as.numeric(base_finance_cds$fund_cds_date)
  base_finance_cds$fund_cds_date <- as.Date(base_finance_cds$fund_cds_date, origin="1899-12-30")
  base_finance_cds$fund_cds_date <- as.character(base_finance_cds$fund_cds_date)
  
  base_finance_cds <- left_join(
    base_finance_cds,
    entity_characteristics,
    by = "a_iso"
  )
  
  base_finance_cds <- base_finance_cds %>%
    mutate(fund_cds_date = if_else(
      is.na(fund_cds_date) & is.na(fund_cds_details) == FALSE,
      "Pending",
      fund_cds_date))
  
  print(" >> Function 'load_finance_cds' done")
  return(base_finance_cds)
}


transform_finance <- function(base_finance, entity_characteristics) {
  print(" >> Filtering base_finance data...")
  base_finance <- filter(base_finance, recipient == "Country")
  base_finance <- filter(base_finance, information_type == "Funding Information")
  base_finance <- filter(base_finance, double_count == "Keep")
  
  print(" >> Preparing base_finance_delivery data...")
  base_finance_delivery <- filter(base_finance,
                                  allocation_type == "Vaccine Delivery" &
                                    is.na(fund_total) == FALSE &
                                    fund_total != 0
  )
  
  base_finance_delivery <- base_finance_delivery %>%
    mutate(funding_source = if_else(
      funding_source == "Foundations/Private",
      "Foundations / private",
      funding_source
    )
    )
  
  base_finance_delivery$funder <- helper_replace_values_with_map(
    data = base_finance_delivery$funder,
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
  
  print(" >> Preparing base_finance_delivery_summary data...")
  base_finance_delivery_summary <- base_finance_delivery %>%
    group_by(a_iso) %>%
    summarize_at("fund_total", sum, na.rm = TRUE) %>%
    mutate_if(is.numeric, round)
  
  print(" >> Preparing base_finance_delivery_summary_source data...")
  base_finance_delivery_summary_source <- base_finance_delivery %>%
    group_by(a_iso, funding_source, funder) %>%
    summarize_at(c("fund_total","fund_disbursed","fund_committed"), sum, na.rm = TRUE) %>%
    mutate_if(is.numeric, round)
  
  base_finance_delivery_summary_source <- base_finance_delivery_summary_source %>%
    mutate(fund_comitted_per = fund_committed / fund_total) %>%
    mutate(fund_disbursed_per = fund_disbursed / fund_total)
  
  print(" >> Preparing base_finance_delivery_summary_long data...")
  base_finance_delivery_summary_long <- base_finance_delivery %>%
    group_by(a_iso, funding_source, funder) %>%
    summarize_at("fund_committed", sum, na.rm = TRUE) %>%
    mutate_if(is.numeric, round)
  
  base_finance_delivery_summary_long$type <- "Committed"
  base_finance_delivery_summary_long <- base_finance_delivery_summary_long %>%
    mutate(value = fund_committed)
  base_finance_delivery_summary_long <- select(base_finance_delivery_summary_long, -c("fund_committed"))
  
  b_fin_fund_del_long_temp <- base_finance_delivery %>%
    group_by(a_iso, funding_source, funder) %>%
    summarize_at("fund_disbursed", sum, na.rm = TRUE) %>%
    mutate_if(is.numeric, round)
  
  b_fin_fund_del_long_temp$type <- "Disbursed"
  b_fin_fund_del_long_temp <- b_fin_fund_del_long_temp %>%
    mutate(value = fund_disbursed)
  b_fin_fund_del_long_temp <- select(b_fin_fund_del_long_temp, -c("fund_disbursed"))
  
  base_finance_delivery_summary_long <- bind_rows(base_finance_delivery_summary_long, b_fin_fund_del_long_temp)
  
  entity_characteristics <- entity_characteristics %>%
    select(-c("ndvp_mid_deadline",
              "ss_deadline",
              "date"
    ))
  
  print(" >> Joining base_finance_delivery_summary_long data to entity_characteristics data...")
  base_finance_delivery_summary_long <- left_join(
    base_finance_delivery_summary_long, entity_characteristics,
    by = "a_iso"
  )
  
  print(" >> Joining base_finance_delivery_summary_source data to entity_characteristics data...")
  base_finance_delivery_summary_source <- left_join(
    base_finance_delivery_summary_source, entity_characteristics,
    by = "a_iso"
  )
  
  print(" >> Creating base_finance_delivery datalist...")
  datalist_finance <- list("base_finance_delivery_summary_source" = base_finance_delivery_summary_source,
                           "base_finance_delivery_summary" = base_finance_delivery_summary,
                           "base_finance_delivery_summary_long" = base_finance_delivery_summary_long)
  
  print(" >> Function 'transform_finance' done")
  return(datalist_finance)
}


transform_finance_urgent <- function(base_finance_urgent, entity_characteristics) {
  print(" >> Preparing base_finance_urgent data...")
  base_finance_urgent <- filter(base_finance_urgent, fund_urg_status == "yes")
  base_finance_urgent$fund_urg_dis_total <- as.numeric(base_finance_urgent$fund_urg_dis_total)
  
  print(" >> Preparing base_finance_urgent_long data...")
  base_finance_urgent_long <- base_finance_urgent
  base_finance_urgent_long <- left_join(
    base_finance_urgent_long,
    entity_characteristics,
    by = "a_iso"
  )
  
  # print(" >> Selecting relevant columns...")
  # base_finance_urgent_long <- select(
  #   base_finance_urgent_long, c(
  #     "ISO",
  #     "Comment",
  #     "Date.of.First.Disbursement",
  #     "Amount.approved",
  #     "Amount.disbursed",
  #     "Funding.type"
  #   )
  # )
  
  print(" >> Preparing base_finance_urgent_summary data...")
  base_finance_urgent_summary <- base_finance_urgent %>%
    group_by(a_iso, fund_urg_source, fund_urg_util) %>%
    summarize_at("fund_urg_dis_total", sum, na.rm = TRUE)
  
  base_finance_urgent_summary <- left_join(
    base_finance_urgent_summary,
    entity_characteristics,
    by = "a_iso"
  )
  
  print(" >> Creating base_finance_urgent datalist...")
  datalist_finance_urgent <- list("base_finance_urgent_long" = base_finance_urgent_long,
                                  "base_finance_urgent_summary" = base_finance_urgent_summary)
  
  print(" >> Function 'transform_finance_urgent' done")
  return(datalist_finance_urgent)
}
