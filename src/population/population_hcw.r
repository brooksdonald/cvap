
load_population_hcw <- function() {
    print(" >> Loading base hcw data...")
    population_hcw <- data.frame(
        read_excel("data/input/static/base_hcw population.xlsx",
            sheet = "base_hcw"
        )
    )

    print(" >> Selecting base hcw data...")
    population_hcw <- 
        select(
            population_hcw, 
            c(
                "ISO3",
                "Stock"
            )
        )

    print(" >> Renaming columns...")
    colnames(population_hcw) <- c(
        "a_iso",
        "a_pop_hcw"
    )

    return(population_hcw)

}

transform_population_hcw <- function(population_hcw) {
    print(" >> Replacing all 0s with NA_real_ ...")
    population_hcw <- population_hcw %>%
    mutate(a_pop_hcw = if_else(a_pop_hcw == 0, NA_real_, a_pop_hcw))

    return(population_hcw)
}