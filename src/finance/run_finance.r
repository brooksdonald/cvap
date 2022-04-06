run_financing <- function(env = .GlobalEnv) {
    source("src/finance/finance.r")
    
    print(" > Starting local environment for fiancing module")
    print(" > Loading financing data...")
    b_fin_funding <- load_finance_data()
    print(" > Done.")

    print(" > Transforming financing data...")
    b_fin_fund_del_source <- transform_finance_data(
        b_fin_funding, entity_characteristics
    )
    print(" > Done.")

    print(" > Returning to global environment. ")

    print(" > Loading financing data back to global environment...")
    env$b_fin_funding <- b_fin_funding
    env$b_fin_fund_del_source <- b_fin_fund_del_source

    return(environment())
}
