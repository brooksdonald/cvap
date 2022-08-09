

helper_join_dataframe_list <- function(l, join_by, allx = TRUE, ally = FALSE) {
    join <- Reduce(
        function(x, y) merge(x, y, by = join_by, all.x = allx, all.y = ally),
        l
    )

    if (typeof(join) == "list") {
       join <- data.frame(join)
    }

    return(join)
}

helper_load_list_of_files <- function(files, sheet_name, date_format) {
    df_list <- list()
    for (i in seq_along(files)) {
        length_of_date_string <- nchar(
            format(as.Date("2000-01-01"), date_format))
        date <- as.Date(
            substr(sub(".*/", "", files[i]), 1, length_of_date_string),
            date_format)
        month <- substr(date, 1, 7)
        df <- data.frame(
            read_excel(
                files[i],
                sheet = sheet_name)) %>%
            mutate(month_name = month)
        df_list[[i]] <- df
    }
    return(df_list)
}