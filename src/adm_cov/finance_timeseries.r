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
        stop("One of C19VFM datasets does not conform to year-month format
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
    # Summing up the fund total
    df_list[[i]] <- df_list[[i]] %>%
      drop_na(ISO.Code) %>%
        filter(
            Recipient.Type == "Country" &
            Information.Type == "Funding Information" &
            Double.Counting == "Keep" &
            Allocation.Type == "Vaccine Delivery" &
            is.na(Funding.Amount) == FALSE &
            Funding.Amount != 0
        ) %>%
    group_by(ISO.Code, month_name) %>%
    summarize_at("Funding.Amount", sum, na.rm = TRUE)
    }
    # appending all dataframes together
    print(" >> Combining monthly delivery data...")
    overall_cumul_long <- df_list %>%
        bind_rows() %>%
        arrange(month_name, ISO.Code)
    return(overall_cumul_long)
}
# View(import_finance_data())
overall_cumul_long <- import_finance_data()

transform_fin_data <- function(overall_cumul_long) {
    print(" >> Transforming finacial data...")
    overall_long <- overall_cumul_long %>%
        arrange(month_name) %>%
        mutate(Net_per_month = Funding.Amount - lag(Funding.Amount))
    return(overall_long)
}
overall_long <- transform_fin_data(overall_cumul_long)
View(transform_fin_data(overall_cumul_long))

# write_xlsx(overall_long, "data/output/finance_ts_v1.xlsx")
# View(overall_long)
