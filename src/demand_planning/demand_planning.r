# Load demand planning data

load_demand_plan_data <- function() {
  print(" >> Loading demand planning data")
  base_dp <- data.frame(
    read_excel("data/input/static/base_demandplanning.xlsx",
    sheet = "Data"
    )
  )
  print(" >> Select and rename columns from base_dp...")
  b_dp_red <- select(
    base_dp,
    c(
      "ISOCountry",
      "Coverage.target.in..",
      "Expected.date.to.reach.coverage.target"
      )
    )
    colnames(b_dp_red) <- c(
      "a_iso",
      "dp_target",
      "dp_deadline"
    )
    return(b_dp_red)
}

transform_demandplan_data <- function(b_dp_red) {
  b_dp_red <- subset(b_dp_red, dp_target != "")
  b_dp <- b_dp_red %>%
    group_by(a_iso) %>%
    summarise(
      dp_target = head(dp_target, 1),
      dp_deadline = head(dp_deadline, 1)
    )
  b_dp <- subset(b_dp, a_iso != 0)
  b_dp$dp_deadline <- format(
    as.Date(b_dp$dp_deadline, format = "%d/%m/%Y"),
    "%Y-%m-%d"
  )
  return(b_dp)
}