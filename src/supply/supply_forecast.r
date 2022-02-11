

extract_supply_forecast <- function() {
    print(" >> Reading supply forecast data...")
    supply_forecast_input <-
        data.frame(read_excel("data/_input/base_supply_secured_forecasts.xlsx",
            sheet = "data"
        ))

    supply_forecast <-
        select(
            supply_forecast_input,
            c(
                "iso3",
                "month",
                "total",
                "bilateral",
                "donation",
                "covax",
                "avat",
                "unknown"
            )
        )

    return(supply_forecast)
}




transform_supply_forecast <- function(supply_forecast) {

    # TODO automate the month difference - logic please?
    # TODO get rid of wide datasets

    # Supply forecast
    # FIXME Here I am redoing the whole logic of hardcoding months as it is highly redundant
    # FIXME going to create an issue once you have more than 12 months
    print(" >> Getting unique dates...")
    # get unique dates
    dataset_dates <- unique(supply_forecast$month)
    # sort to make sure we are going from the latest to the newest
    order(as.Date(dataset_dates, format = "%Y-%m-%d"))
    forecasts_monthly <- list()
    totals_column_names <- c()
    # for each date filter the
    # you cannot forloop over datelist as it treats each of them as integer
    print(" >> Filtering monthly datasets...")
    # TODO why selecting so many columns if we use only totals?
    # TODO maybe using pivot table and procedurelly changing the column names is better?
    for (index in seq_len(length(dataset_dates))) {
        date <- dataset_dates[index]
        month_string <- tolower(format(date, "%b"))
        monthly_set <- filter(supply_forecast, month == date)
        total_column <- paste0("sec_cum_total_", month_string)
        colnames(monthly_set) <- c(
            "iso",
            "date",
            total_column,
            paste0("sec_bilat_", month_string),
            paste0("sec_donat_", month_string),
            paste0("sec_covax_", month_string),
            paste0("sec_avat_", month_string),
            paste0("sec_unknown_", month_string)
        )
        forecasts_monthly <- append(forecasts_monthly, list(monthly_set))
        totals_column_names <- c(
            totals_column_names,
            total_column
        )
    }
    print(" >> Joining forecasted data...")
    supply_forecast_join <- Reduce(
        function(x, y) merge(x, y, by = "iso", all.x = TRUE),
        forecasts_monthly
    )

    supply_forecast_total <- select(
        supply_forecast_join,
        all_of(c("iso", totals_column_names))
    )

    print(" >> Calculating running month difference...")
    # calculating difference for all moths, starting with the second month (first element of columns is iso, so index 3)
    for (i in 2:(length(totals_column_names))) {
        month <- unlist(strsplit(totals_column_names[i], "_"))
        month <- month[length(month)]

        supply_forecast_total <- supply_forecast_total %>%
            mutate(
                "add_total_{month}" := UQ(rlang::sym(totals_column_names[i]))
                    -
                    UQ(rlang::sym(totals_column_names[i - 1]))
            )
    }

    return(supply_forecast_total)
}