# rows 2575 - 2691

run_binning <- function(a_data, env = .GlobalEnv) {
    source("eda/rank-bin/rank_bin.r")

    print(" > Starting local environment for ranking and binning")
    
    print(" > Grouping by one column...")
    a_data <- grouping_by_one(a_data)
    print(" > Done.")

    print(" > Grouping by two columns...")
    a_data <- grouping_by_two(a_data)
    print(" > Done.")

    print(" > Loading ranking and binning data back to global environment...") 
    env$a_data <- a_data

    return(environment())

}
