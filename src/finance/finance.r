load_finance_data <- function() {
  print(">> Loading financing data...")
  raw_fin <- data.frame(read_excel("data/input/base_financing.xlsx",
                                   sheet = "Data Structure"))
  
  print(">> Selecting & renaming relevant financing data...")
  base_fin <- raw_fin %>%
    select(
      ISO.Code,
      Recipient.Type,
      Information.Type,
      Allocation.Type,
      Funding.Amount,
      Funding.Source.Type.2,
      Funding.Source,
      Double.Counting,
      Commitments,
      Disbursements,
      FA
    ) %>%
    rename(
      a_iso = ISO.Code,
      recipient = Recipient.Type,
      information_type = Information.Type,
      allocation_type = Allocation.Type,
      fin_tot = Funding.Amount,
      fin_source_type = Funding.Source.Type.2,
      fin_source = Funding.Source,
      double_count = Double.Counting,
      fin_committed = Commitments,
      fin_disbursed = Disbursements,
      fin_add = FA
    )
  
  print(">> Done.")
  return(base_fin)
}

transform_finance_data <-
  function(base_fin, entity_characteristics) {
    print(">> Filtering financing data for country- & delivery-related data...")
    fin_del <- base_fin %>%
      filter(
        recipient == "Country",
        information_type == "Funding Information",
        double_count == "Keep",
        allocation_type == "Vaccine Delivery" &
          is.na(fin_tot) == FALSE
        & fin_tot != 0
      )
    
    print(">> Applying short-form donor names...")
    fin_del$fin_source <- helper_replace_values_with_map(
      data = fin_del$fin_source,
      values = c(
        "Japan - Ministry of Foreign Affairs",
        "UNICEF (Thematic/Flexible Funding) HAC)",
        "Inter-American Development Bank",
        "Germany - Federal Foreign Office (AA)",
        "Government of France - Gavi",
        "Government of Ireland",
        "Asian Development Bank",
        "Bill and Melinda Gates Foundation",
        "Mastercard Foundation"
      ),
      map = c(
        "Japan MoFA",
        "UNICEF HAC",
        "IADB",
        "Germany FFO",
        "France - Gavi",
        "Ireland",
        "ADB",
        "BMGF",
        "MCF"
      ),
      drop_rest = FALSE
    )
    
    print(">> Summarizing financing data by country...")
    fin_del_sum <- fin_del %>%
      group_by(a_iso) %>%
      summarize_at("fin_tot", sum, na.rm = TRUE) %>%
      mutate_if(is.numeric, round) %>%
      ungroup()
    
    print(">> Summarizing financing data by country and by funder...")
    fin_del_sum_source <- fin_del %>%
      group_by(a_iso, fin_source_type, fin_source) %>%
      summarize_at(c("fin_tot", "fin_disbursed", "fin_committed"),
                   sum,
                   na.rm = TRUE) %>%
      mutate_if(is.numeric, round) %>%
      mutate(
        fund_comitted_per = fin_committed / fin_tot,
        fin_disbursed_per = fin_disbursed / fin_tot
      ) %>%
      ungroup()
    
    print(">> Preparing long-form summarized financing data by country and by funder...")
    fin_del_sum_long <- fin_del_sum_source %>%
      select(a_iso,
             fin_source_type,
             fin_source,
             fin_committed,
             fin_disbursed) %>%
      gather(key = "type",
             value = "value",
             -c("a_iso",
                "fin_source_type",
                "fin_source")) %>%
      mutate(type = case_when(
        type == "fin_committed" ~ "Committed",
        type == "fin_disbursed" ~ "Disbursed"
      ))
    
    print(">> Filtering entity details data...")
    entity_characteristics <- entity_characteristics %>%
      select(-c("ss_deadline",
                "date_13jan"))
    
    print(">> Merging financing data frames with entity details data...")
    fin_del_sum_long <- left_join(fin_del_sum_long,
                                  entity_characteristics,
                                  by = "a_iso")
    
    fin_del_sum_source <- left_join(fin_del_sum_source,
                                    entity_characteristics,
                                    by = "a_iso")
    
    print(">> Preparing financing datalist...")
    datalist <-
      list(
        "fin_del_sum" = fin_del_sum,
        "fin_del_sum_source" = fin_del_sum_source,
        "fin_del_sum_long" = fin_del_sum_long
      )
    
    print(">> Done.")
    return(datalist)
  }