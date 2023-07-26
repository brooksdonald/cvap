run_rank_bin <- function(a_data) {
    source("eda/rank_bin/rank_bin.r")

    print("> Starting local environment for ranking & binning")
    print("> Ranking & binning for one variable instances...")
    a_data <- grouping_by_one(a_data)
    print("> Done.")

    print("> Ranking & binning for two variable instances...")
    a_data <- grouping_by_two(a_data)
    print("> Done.")

    return(environment())
}