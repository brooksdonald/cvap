run_api <- function() {
    load_dot_env(".env")

    my_tokens <- Sys.getenv(
        list(
            "authn_resource",
            "authn_tenant",
            "authn_app",
            "authn_auth_type",
            "authn_password",
            "authn_use_cache"
            )
        )

    print(" > Getting Azure tokens...")
    tok0 <- get_azure_token(
        resource = my_tokens[[1]],
        tenant = my_tokens[[2]],
        app = my_tokens[[3]],
        auth_type = my_tokens[[4]],
        password = my_tokens[[5]],
        use_cache = my_tokens[[6]]
    )
    access_token <- tok0$credentials$access_token
    bearer <- paste("Bearer", access_token)
    headers <- add_headers(Authorization = bearer)
    print(" > Tokens obtained successfully...")

    return(environment())
}

helper_wiise_api <- function(link, headers, refresh_api) {
    folder <- "data/input/interim"
    storage_name <- paste0(folder, "/", sub(".*/", "", link), ".csv")
    if (refresh_api | !file.exists(storage_name)) {
        print(" > Downloading data from who.int API...")

        if (is.logical(headers) && !headers) {
            response <- GET(link)
        } else {
            response <- GET(link, headers)
        }
        json <- content(response, "text", encoding = "UTF-8")
        data <- fromJSON(json)
        data <- data$value
        print(" > Done.")
        print(" > Data is stored for future API calls...")
        if (!file.exists(folder)) {
            dir.create(folder)
        }
        write.csv(data, file = storage_name, row.names = FALSE)
    } else {
        print(paste0(" > Old API data is used from ", folder, "..."))
        data <- read.csv(storage_name)
    }
    print(" > Done.")
    return(data)
}