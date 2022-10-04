path <- "data/input/test/"

library("tidyverse")
library("openxlsx")
library("readxl")
library("writexl")
library("countrycode")
library("lubridate")
library("data.table")

source("helpers/joins.r")

# import_finance_data <- function() {
#     folder <- "data/input/test/"
#     sheet_name <- "Data Structure"

#     # getting a list of files available in specified folder
#     files <- unlist(list.files(
#         path = folder,
#         # add to this pattern if the file name changes in the future
#         pattern = "C19VFM"
#     ))

#     # checking for duplicate months
#     print(" >> Checking if data is valid...")
#     months <- substr(files, 1, 4)
#     if (sum(duplicated(months))) {
#         stop(paste0("Error:
#         Two files in ",
#         folder,
#         " with 'imf' in the name have the same year month combination."))
#     }

#     # checking for reasonable years (between 2021 and 2040) 
#     # and months (between 01 and 12)
#     years <- substr(files, 1, 2)
#     months <- substr(files, 3, 4)
#     if (
#         sum(as.numeric(years) > 40) |
#         sum(as.numeric(years) < 21) |
#         sum(as.numeric(months) > 12) |
#         sum(as.numeric(months) < 1)) {
#         stop("One of IMF datasets does not conform to year-month format
#             (must be, e.g., 2108 for Aug 2021)")
#     }
#     print(paste0(" >> Names of all ", length(files), " files are valid"))

#     # importing data
#     print(paste0(" >> Loading ", length(files), " files from ", folder))
#     df_list <- helper_load_list_of_files(
#         files = paste0(folder, files),
#         sheets = sheet_name,
#         date_format = "%y%m%d"
#     )

#     print(" >> Selecting relevant columns...")
#     for (i in seq_along(df_list)) {
#         df_list[[i]] <- df_list[[i]] %>%
#             select(
#                 c(
#                     "ISO.Code",
#                     "Recipient.Type",
#                     "Information.Type",
#                     "Allocation.Type",
#                     "Funding.Amount",
#                     "Funding.Source.Type",
#                     "Funding.Source",
#                     "Double.Counting",
#                     "month_name"
#                 )
#             )
#     }

#     # appending all dataframes together
#     print(" >> Combining monthly delivery data...")
#     overall_cumul_long <- df_list %>%
#         bind_rows() %>%
#         arrange(month_name, ISO.Code)
#     print(" >> Done.")

#     return(overall_cumul_long)
# }


# View(import_finance_data())

jan_april_finance_data <- function() {
    print(">> Loading finance data from 2021 to 2022...")
    # Jan data
    jan_data <- data.frame(
        read_excel("data/input/test/220126_C19VFM.xlsx",
            sheet = "Data Structure",
            skip = 1
        )
    ) %>% select(
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
    jan_data$month <- "2022-01"
    print(" > Jan data loaded...")
    # Feb data
    feb_data <- data.frame(
        read_excel("data/input/test/220228_C19VFM.xlsx",
            sheet = "Data Structure",
            skip = 1
        )
    ) %>% select(
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
    feb_data$month <- "2022-02"
    print(" > Feb data loaded...")
    # March data
    mar_data <- data.frame(
        read_excel("data/input/test/220330_C19VFM.xlsx",
            sheet = "Data Structure",
            skip = 1
        )
    ) %>% select(
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
    mar_data$month <- "2022-03"
    print(" > March data loaded...")
    # April data
    apr_data <- data.frame(
        read_excel("data/input/test/220427_C19VFM.xlsx",
            sheet = "Data Structure",
            skip = 1
        )
    ) %>% select(
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
    apr_data$month <- "2022-04"
    print(" > April data loaded...")

    # Combine Jan to April 2022 data
    print(" > Merging data from Jan to April 2022...")
    combined_data <- rbind(
        jan_data,
        feb_data,
        mar_data,
        apr_data
    )
    print(" > merge done")
    print(head(combined_data))
}
jan_april_finance_data()