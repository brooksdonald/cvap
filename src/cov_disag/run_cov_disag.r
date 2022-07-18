# rows 600 - 978

run_population <- function(headers) {
    source("src/cov_disag/cov_disag.r")

    print(" > Starting local environment for cov_disag")

    print(" > Population uptake...")
    datalist <- load_population_uptake(headers)
    uptake_gender_data <- transform_population_uptake(
        as.data.frame(datalist$uptake_gender),
        as.data.frame(datalist$uptake_groups)
    )
    print(" > Done.")

    print(" > Returning to global environment. ")
    return(environment())
}