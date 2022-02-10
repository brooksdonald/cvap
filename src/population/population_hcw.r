
load_base_hcw <- function() {
    print(" >> Loading base hcw data...")
    b_pop_hcw <- data.frame(
        read_excel("data/input/static/base_hcw population.xlsx",
            sheet = "base_hcw"
        )
    )

    print(" >> Selecting base hcw data...")
    z_pop_hcw <- 
        select(
            b_pop_hcw, 
            c(
                "ISO3",
                "Stock"
            )
        )

    print(" >> Renaming columns...")
    colnames(z_pop_hcw) <- c(
        "a_iso",
        "a_pop_hcw"
    )

    return(z_pop_hcw)

}

transform_base_hcw <- function() {
    z_pop_hcw <- z_pop_hcw %>%
    mutate(a_pop_hcw = if_else(a_pop_hcw == 0, NA_real_, a_pop_hcw))

    #FIXME What am I returning here
    return()
}