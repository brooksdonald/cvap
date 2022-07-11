run_financing <- function(env = .GlobalEnv) {
    source("src/finance/finance.r")
    
    print(" > Starting local environment for fiancing module")
    print(" > Loading financing data...")
    b_fin_funding <- load_finance_data()
    base_fin_urg_fun <- load_finance_urgent_data()
    base_fin_cds_red <- load_finance_cds_data(entity_characteristics)
    print(" > Done.")

    print(" > Transforming financing data...")
    b_fin_fund_del_source <- transform_finance_data(
        b_fin_funding, entity_characteristics
    )
    base_fin_urg_fun_sum <- transform_fund_urgent_data(
      base_fin_urg_fun, entity_characteristics
    )
    print(" > Done.")

    print(" > Returning to global environment. ")

    print(" > Loading financing data back to global environment...")
    env$b_fin_funding <- b_fin_funding
    env$b_fin_fund_del_source <- b_fin_fund_del_source
    env$base_fin_urg_fun <- base_fin_urg_fun
    env$base_fin_urg_fun_sum <- base_fin_urg_fun_sum
    env$base_fin_urg_fun_long <- base_fin_urg_fun_long
    env$base_fin_cds_red <- base_fin_cds_red

    return(environment())
}