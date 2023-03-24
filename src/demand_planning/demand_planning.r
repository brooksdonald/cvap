
load_demandplanning <- function() {
  print(" >> Loading demand planning data")
  base_demandplanning <- data.frame(
    read_excel("data/input/static/base_demandplanning.xlsx",
               sheet = "Data"
               )
    )
  
  print(" >> Selecting relevant columns...")
  base_demandplanning <- select(
    base_demandplanning, c(
      "ISOCountry",
      "Coverage.target.in..",
      "Expected.date.to.reach.coverage.target"
      )
    )
  
  colnames(base_demandplanning) <- c(
    "a_iso",
    "dp_target",
    "dp_deadline"
    )
    
  print(" >> Function 'load_demandplanning' done")
  return(base_demandplanning)
}

transform_demandplanning <- function(base_demandplanning) {
  base_demandplanning <- subset(base_demandplanning, dp_target != "")
  
  base_demandplanning <- base_demandplanning %>%
    group_by(a_iso) %>%
    summarise(
      dp_target = head(dp_target, 1),
      dp_deadline = head(dp_deadline, 1)
      )
  
  base_demandplanning <- subset(base_demandplanning, a_iso != 0)
  base_demandplanning$dp_deadline <- format(
    as.Date(base_demandplanning$dp_deadline, format = "%d/%m/%Y"),
    "%Y-%m-%d"
  )
  
  print(" >> Function 'transform_demandplanning' done")
  return(base_demandplanning)
}
