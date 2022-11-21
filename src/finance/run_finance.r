
run_finance <- function(entity_characteristics) {
    print(" > Starting local environment for financing module...")
    source("src/finance/finance.r")
    
    print(" > Loading financing data...")
    b_fin_funding <- load_finance()
    print(" > Done.")
    
    print(" > Loading urgent financing data...")
    base_fin_urg_fun <- load_finance_urgent()
    print(" > Done.")
    
    print(" > Loading Gavi CDS financing data...")
    base_fin_cds_red <- load_finance_cds(entity_characteristics)
    print(" > Done.")

    print(" > Transforming financing data...")
    datalist <- transform_finance(
        b_fin_funding, entity_characteristics
    )
    b_fin_fund_del_source <- datalist$b_fin_fund_del_source
    b_fin_fund_del_sum <- datalist$b_fin_fund_del_sum
    b_fin_fund_del_long <- datalist$b_fin_fund_del_long
    print(" > Done.")

    print(" > Transforming urgent funding data...")
    datalist2 <- transform_finance_urgent(
        base_fin_urg_fun, entity_characteristics)
    base_fin_urg_fun_long <- datalist2$base_fin_urg_fun_long
    base_fin_urg_fun_sum <- datalist2$base_fin_urg_fun_sum
    print(" > Done.")

    print(" > Returning to global environment.")
    return(environment())
}