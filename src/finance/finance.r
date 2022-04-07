# Load finance data

load_finance_data <- function() {
    print(" >> Loading financing data...")
    base_fin <- data.frame(
        read_excel("data/_input/base_financing.xlsx",
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
            "Funding.Source.Type",
            "Funding.Source",
            "Double.Counting"
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
        "double_count"
    )
    return(b_fin_funding)
}

transform_finance_data <- function(b_fin_funding, entity_characteristics) {
    b_fin_funding <- filter(b_fin_funding, recipient == "Country")
    b_fin_funding <- filter(b_fin_funding, information_type == "Funding Information")
    b_fin_funding <- filter(b_fin_funding, is.na(double_count))

    b_fin_fund_del <- filter(
        b_fin_funding,
        allocation_type == "Vaccine Delivery" &
        is.na(fund_total) == FALSE
        & fund_total != 0
    )
    b_fin_fund_del <- b_fin_fund_del %>%
    mutate(funding_source = if_else(funding_source == "Foundations/Private", "Foundations / private", funding_source))
    b_fin_fund_del$funding_source <- helper_replace_values_with_map(
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
        )
    )
    b_fin_fund_del_sum <<- b_fin_fund_del %>%
    group_by(a_iso) %>%
    summarize_at("fund_total", sum, na.rm = TRUE) %>%
    mutate_if(is.numeric, round)

    b_fin_fund_del_source <- b_fin_fund_del %>%
    group_by(a_iso, funding_source, funder) %>%
    summarize_at("fund_total", sum, na.rm = TRUE) %>%
    mutate_if(is.numeric, round)

    b_fin_fund_del_source <- left_join(
        b_fin_fund_del_source,
        entity_characteristics,
        by = "a_iso"
    )
    return(b_fin_fund_del_source)
}