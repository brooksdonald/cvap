
run_rank_bin <- function(a_data) {
    source("eda/rank_bin/rank_bin.r")

    print(" > Starting local environment for ranking and binning")
    print(" > Grouping by one column...")
    a_data <- grouping_by_one(a_data)
    print(" > Done.")

    print(" > Grouping by two columns...")
    a_data <- grouping_by_two(a_data)
    print(" > Done.")
    
    print(" > Grouping by three columns...")
    a_data <- grouping_by_three(a_data)
    print(" > Done.")

    return(environment())
}