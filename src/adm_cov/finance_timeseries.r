path <- "data/input/test/"

library("tidyverse")
library("openxlsx")
library("readxl")
library("writexl")
library("countrycode")
library("lubridate")
library("data.table")

source("helpers/joins.r")

# load_financial_data <- function(path) {
#     # reading in list of files in directory
#     print(" >> Selecting finance timeseries data files to read in...")
#     files <- unlist(
#         list.files(
#             path = path,
#             pattern = "C19VFM")
#     )
    
#     # finding the right sheet to read in
#     print(" >> Finding the right sheet to read in...")
#     sheet_vector <- c()
#     for (file in files) {
#         sheets <- excel_sheets(path = paste0(path, file))
#         # if sheet names change in the future, simply add to this list
#         possible_sheet_names <- c("Data Structure")
#         for (name in possible_sheet_names) {
#             if (name %in% sheets) {
#                 relevant_sheet <- name
#             }
#         }
        
#         # throw error if there is no sheet with one of the names above
#         if (is.na(relevant_sheet)) {
#             stop(paste0("Error: ", file, " does not have a sheet with a right name.
#             Add new sheet name to the list of 
#             possible sheet names in supply_timeseries.r"))
#         }
#         sheet_vector <- append(sheet_vector, relevant_sheet)
#         remove(relevant_sheet)
#     }

#     # import files from excel
#     print(" >> Read in Data Structure data...")
#     df_list <- helper_load_list_of_files(
#         files = paste0(path, files),
#         sheets = sheet_vector,
#         date_format = "%y%m%d",
#         month_lag = 1
#     )
#     # end of automated read-in

#     # Reduce dataframes to required columns
#     print(" >> Transforming financing data...")
#     # df_list_trans <- list()
#     for (i in seq_along(df_list)) {
#         # df <- df_list[[i]]
#         df_list[[i]] <- df_list[[i]] %>%
#             select(
#                 ISO.Code,
#                 Recipient.Type,
#                 Information.Type,
#                 Allocation.Type,
#                 Funding.Amount,
#                 # Funding.Source.Type.2,
#                 Funding.Source,
#                 Double.Counting
#                 # Commitments,
#                 # Disbursements,
#                 # FA
#             )
#         }
#     print(colnames(df_list[[]]))
# }

jan_april_finance_data <- function() {
    print(">> Loading finance data from 2021 to 2022...")
    jan_data <- data.frame(
        read_excel("data/input/test/220126_C19VFM.xlsx",
            sheet = "Data Structure"
        )
    )
    feb_data <- data.frame(
        read_excel("data/input/test/220228_C19VFM.xlsx",
            sheet = "Data Structure"
        )
    )
    mar_data <- data.frame(
        read_excel("data/input/test/220330_C19VFM.xlsx",
            sheet = "Data Structure"
        )
    )
    apr_data <- data.frame(
        read_excel("data/input/test/220427_C19VFM.xlsx",
            sheet = "Data Structure"
        )
    )
    jan_apr <- helper_join_dataframe_list(
        list(jan_data, feb_data, mar_data, apr_data),
        join_by = c("ISO.Code")
    )
    jan_apr <- select(
        jan_apr,
        c(
            "ISO.Code",
            "Recipient.Type",
            "Information.Type",
            "Allocation.Type",
            "Funding.Amount",
            "Funding.Source.Type.2",
            "Funding.Source",
            "Double.Counting"
        )
    )
    write_xlsx(jan_apr, "data/output")
    print("Done.")
}
jan_april_finance_data()