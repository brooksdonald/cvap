
load_demand_plan <- function() {
    print(" >> Loading demand planning data...")
    base_dp <- data.frame(
      read_excel(
        "data/input/static/base_demandplanning.xlsx",
        sheet = "Data"
        )
      )
    
    print(" >> Selecting relevant columns... ")
    b_dp_red <- select(
      base_dp, c(
        "ISOCountry",
        "Coverage.target.in..",
        "Expected.date.to.reach.coverage.target"
        )
      )
    
    print(" >> Renaming columns... ")
    colnames(b_dp_red) <- c(
      "a_iso",
      "dp_target",
      "dp_deadline"
      )
    
    print(" >> Function 'load_demand_plan' done")
    return(b_dp_red)
  }
  
transform_demand_plan <- function(b_dp_red) {
    print(" >> Transforming demand planning data...")
    b_dp <- subset(b_dp_red[!duplicated(b_dp_red), ], dp_target != "" & a_iso != 0)
    b_dp$dp_deadline <- format(
      as.Date(b_dp$dp_deadline, format = "%d/%m/%Y"), "%Y-%m-%d"
      )
    
    print(" >> Function 'transform_demand_plan' done")
    return(b_dp)
}