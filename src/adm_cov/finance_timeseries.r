path <- "data/input/test/"

library("tidyverse")
library("openxlsx")
library("readxl")
library("writexl")
library("countrycode")
library("lubridate")
library("data.table")

source("helpers/joins.r")
source("helpers/transformations.r")

import_finance_data <- function() {
    folder <- "data/input/test/"
    sheet_name <- "Data Structure"

    # getting a list of files available in specified folder
    files <- unlist(list.files(
        path = folder,
        # add to this pattern if the file name changes in the future
        pattern = "C19VFM"
    ))

    # checking for duplicate months
    print(" >> Checking if data is valid...")
    months <- substr(files, 1, 4)
    if (sum(duplicated(months))) {
        stop(paste0("Error:
        Two files in ",
        folder,
        " with 'C19VFM' in the name have the same year month combination."))
    }

    # checking for reasonable years (between 2021 and 2040)
    # and months (between 01 and 12)
    years <- substr(files, 1, 2)
    months <- substr(files, 3, 4)
    if (
        sum(as.numeric(years) > 40) |
        sum(as.numeric(years) < 21) |
        sum(as.numeric(months) > 12) |
        sum(as.numeric(months) < 1)) {
        stop("One of IMF datasets does not conform to year-month format
            (must be, e.g., 2108 for Aug 2021)")
    }
    print(paste0(" >> Names of all ", length(files), " files are valid"))

    # importing data
    print(paste0(" >> Loading ", length(files), " files from ", folder))
    df_list <- helper_load_list_of_files(
        files = paste0(folder, files),
        sheets = sheet_name,
        date_format = "%y%m%d"
    )

    print(" >> Selecting relevant columns...")
    columns <- c(
        "ISO.Code",
        "Recipient.Type",
        "Information.Type",
        "Allocation.Type",
        "Funding.Amount",
        "Funding.Source.Type.2",
        "Funding.Source.Type",
        "Funding.Source",
        "Double.Counting",
        "Commitments",
        "Disbursements",
        "FA",
        "month_name"
    )
    # Loop through all columns in files to check existence of all column
    for (i in seq_along(df_list)) {
        for (c in columns) {
            if (!(c %in% colnames(df_list[[i]]))) {
                df_list[[i]][c] <- NA
            }
        }

    df_list[[i]] <- df_list[[i]] %>%
        select(
            columns
        )
    # Summing 
    df_list[[i]] <- df_list[[i]] %>%
      drop_na(ISO.Code) %>%
      select(
        ISO.Code,
        Funding.Amount,
        Recipient.Type,
        Information.Type,
        Allocation.Type,
        Double.Counting,
        month_name
      ) %>%
      group_by(ISO.Code, month_name, Recipient.Type, Information.Type,  Allocation.Type, Double.Counting,) %>%
      summarize_at("Funding.Amount", sum, na.rm = TRUE)
    }
    # appending all dataframes together
    print(" >> Combining monthly delivery data...")
    overall_cumul_long <- df_list %>%
        bind_rows() %>%
        arrange(month_name, ISO.Code)
    # Rename columns
    # print(" >> renaming columns...")
    # overall_cumul_long <- rename(
    #     overall_cumul_long,
    #     c(
    #         a_iso = ISO.Code,
    #         recipient = Recipient.Type,
    #         information_type = Information.Type,
    #         allocation_type = Allocation.Type,
    #         fund_total = Funding.Amount,
    #         funding_source_2 = Funding.Source.Type.2,
    #         funding_source = Funding.Source.Type,
    #         funder = Funding.Source,
    #         double_count = Double.Counting,
    #         fund_committed = Commitments,
    #         fund_disbursed = Disbursements,
    #         fun_add = FA,
    #         month_name = month_name
    #     )
    # )
    # print(" >> Done.")
    return(overall_cumul_long)
}
# View(import_finance_data())
overall_cumul_long <- import_finance_data()

transform_fin_data <- function(overall_cumul_long) {
    print(" >> Transforming finacial data...")
    overall_long <- overall_cumul_long %>%
    filter(
        Recipient.Type == "Country" &
      Information.Type == "Funding Information" &
      Double.Counting == "Keep" &
      Allocation.Type == "Vaccine Delivery" &
      is.na(Funding.Amount) == FALSE &
      Funding.Amount != 0
    )
    return(overall_long)
}
# View(transform_fin_data(overall_cumul_long))
overall_long <- transform_fin_data(overall_cumul_long)
write_xlsx(overall_long, "data/output/finance_ts_v1.xlsx")
# View(overall_long)

# }
# transform_fin_data(overall_cumul_long)



# transform_fin_data <- function(overall_cumul_long, entity_characteristics) {
#     print(" >> Transforming finacial data...")
#     b_fin_funding <- overall_cumul_long %>%
#         filter(
#             recipient == "Country" &
#             information_type == "Funding Information" &
#             double_count == "Keep" &
#             allocation_type == "Vaccine Delivery" &
#             is.na(fund_total) == FALSE &
#             fund_total != 0
#         )
#     b_fin_fund_del <- b_fin_funding %>%
#         mutate(funding_source = if_else(
#             funding_source == "Foundations/Private",
#             "Foundations / private",
#             funding_source
#         )
#     )
#     b_fin_fund_del$funder <- helper_replace_values_with_map(
#         data = b_fin_fund_del$funder,
#         values = c(
#             "Japan - Ministry of Foreign Affairs",
#             "UNICEF (Thematic/Flexible Funding) HAC)",
#             "Inter-American Development Bank",
#             "Germany - Federal Foreign Office (AA)",
#             "Governm ent of France - Gavi",
#             "Government of Ireland",
#             "Asian Development Bank",
#             "Bill and Melinda Gates Foundation"
#         ),
#         map = c(
#             "Japan MoFA",
#             "UNICEF HAC",
#             "IADB",
#             "Germany FFO",
#             "France - Gavi",
#             "Ireland",
#             "ADB",
#             "BMGF"
#         ),
#         drop_rest = FALSE
#     )
#     # condense all data to a single line per country
#     b_fin_fund_total <- b_fin_fund_del %>%
#         drop_na(a_iso) %>%
#         select(
#             a_iso,
#             fund_total,
#             month_name
#         ) %>%
#         group_by(a_iso, month_name) %>%
#         summarize_at("fund_total", sum, na.rm = TRUE) %>%
#         mutate_if(is.numeric, round)
#     View(b_fin_fund_total)

#     # Net per month (calculate difference between months)
#     b_fin_fund_diff <- b_fin_fund_del %>%
#         mutate(funds = replace_na(funds, 0)) %>%
#         arrange(month_name) %>%
#         group_by(a_iso) %>%
#         mutate(value = funds - lag(funds))
#     View(b_fin_fund_diff)


# }
# transform_fin_data(overall_cumul_long, entity_characteristics)
