run_financing <- function(entity_characteristics, env = .GlobalEnv) {
    source("src/finance/finance.r")
    
    print(" > Starting local environment for fiancing module")
    print(" > Loading financing data...")
    b_fin_funding <- load_finance_data()
    print(" > Done.")

    print(" > Transforming financing data...")
    datalist <- transform_finance_data(
        b_fin_funding, entity_characteristics
    )
    b_fin_fund_del_source <- datalist$b_fin_fund_del_source
    b_fin_fund_del_sum <- datalist$b_fin_fund_del_sum
    print(" > Done.")

    print(" > Returning to global environment. ")

    return(environment())
}