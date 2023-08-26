run_supply <- function(date_del,
                       date_refresh,
                       refresh_timeseries) {
  print("> Loading src/supply module scripts...")
  source("src/supply/supply_received.r")
  source("src/supply/supply_timeseries.r")
  
  print("> Loading supply received data...")
  base_sup_rec <- load_sup_rec()
  print("> Done.")
  
  print("> Transforming supply received (dose) data by country...")
  sup_rec_dose <- transform_sup_rec_dose(base_sup_rec, date_del)
  print("> Done.")
  
  print("> Transforming supply received (dose) data by country and product...")
  datalist2 <- transform_sup_rec_dose_prod(base_sup_rec)
  sup_rec_dose_prod <- datalist2$sup_rec_dose_prod
  sup_rec_jj <- datalist2$sup_rec_jj
  print("> Done.")
  
  print("> Preparing supply received (course) data by country...")
  sup_rec <- transform_sup_rec_cour(sup_rec_dose)
  print("> Done.")
  
  print("> Setting path for timeseries base files...")
  path_ts <- "data/input/test/"
  print("> Done.")
  
  if (refresh_timeseries) {
    print("> Reconstructing supply timeseries data...")
    sup_sec_ts <- load_ts_sup_sec(path_ts)
    sup_rec_ts <- load_ts_sup_rec(path_ts)
    sup_sec_ts_add <- transform_sup_sec(sup_sec_ts)
    sup_rec_ts_add <- transform_sup_rec(sup_rec_ts)
    merge_export_sup_ts(sup_sec_ts,
                        sup_sec_ts_add,
                        sup_rec_ts_add,
                        sup_rec_ts)
    
    print("> Done - file exported to data/input/interim/supply.xlsx.")
    
  } else {
    print("> Importing supply timeseries from data/input/interim/supply.xlsx")
    sup_ts_long <- load_sup_ts_long_xlsx()
    sup_ts_wide <- load_sup_ts_wide_xlsx()
    
    print("> Done.")
  }
  
  return(environment())
}