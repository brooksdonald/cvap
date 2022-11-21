
load_one_budget_tracker <- function() {
  print(" >> Loading One Budget Tracker data...")
  base_one_budget_tracker <- data.frame(
    read_excel(
      "data/input/base_funding_tracker.xlsx",
      sheet = "One Budget tracker"
      )
    )
  
  print(" >> Selecting relevant columns...")
  base_one_budget_tracker <- select(
    base_one_budget_tracker,
    c(
      "Country",
      "One.Budget.Status",
      "Progress.Tracker...current.step.being.worked.on...achieved"
      )
    )

  print(" >> Renaming Columns...")
  colnames(base_one_budget_tracker) <- c(
    "a_name_long",
    "one_budget_status",
    "progress_tracker"
  )
    
  print(" >> Finished 'load_one_budget_tracker' function...")
  return(base_one_budget_tracker)
}

load_one_budget_cds <- function() {
  print(" >> Loading One Budget CDS data...")
  base_one_budget_cds <- data.frame(
    read_excel(
      "data/input/base_funding_tracker.xlsx",
      sheet = "One Budget & CDS ")
  )
  
  print(" >> Selecting relevant columns...")
  base_one_budget_cds <- select(
    base_one_budget_cds,
    c(
      "Country",
      "One.Budget.Status",
      "Open.One.Plan...One.Budget.TA.request.status",
      "Application.Status..CDS.3.0.",
      "TA.request.for.CDS"
    )
  )
  
  print(" >> Renaming Columns...")
  colnames(base_one_budget_cds) <- c(
    "a_name_long",
    "one_budget_status",
    "request_status",
    "cds3_application_status",
    "cds3_technical_assistance"
  )
  
  print(" >> Finished 'load_one_budget_cds' function...")
  return(base_one_budget_cds)
}

load_requests <- function() {
  print(" >> Loading requests data...")
  base_requests <- data.frame(
    read_excel(
      "data/input/base_financing_urgent.xlsx",
      sheet = "Funding tracker"
      )
    )
  
  print(" >> Selecting relevant columns...")
  base_requests <- select(
    base_requests, c(
      "ISO",
      "Country.supported",
      "Type.of.request",
      "Total.amount.of.funding.request.received.initially..or.amount.of.standard.request.discussed.",
      "Amount.to.be.funded",
      "Status"
      )
    )
  
  print(" >> Renaming Columns...")
  colnames(base_requests) <- c(
    "a_iso",
    "a_name_long",
    "request_type",
    "amount_requested",
    "amount_approved",
    "request_status"
  )
  
  print(" >> Finished 'load_requests' function...")
  return(base_requests)
}

transform_base_one_budget_cds <- function(base_one_budget_cds) {
    print(" >> Transforming one budget CDS data...")
    
    base_one_budget_cds$cds3_application_status[grepl('Delay expected', base_one_budget_cds$cds3_application_status)] <- 'Delay expected'
    base_one_budget_cds$cds3_application_status[grepl('Submitted', base_one_budget_cds$cds3_application_status)] <- 'Submitted'
    base_one_budget_cds$cds3_application_status[grepl('Requested and being provided', base_one_budget_cds$cds3_application_status)] <- 'Requested and being provided'
    base_one_budget_cds$cds3_application_status[grepl('New deadline', base_one_budget_cds$cds3_application_status)] <- 'New deadline'
    base_one_budget_cds$cds3_application_status[grepl('TBC', base_one_budget_cds$cds3_application_status)] <- 'TBC'
    base_one_budget_cds$cds3_application_status[grepl('TBD', base_one_budget_cds$cds3_application_status)] <- 'TBD'
    
    print(" >> Finished 'transform_base_one_budget_cds' function...")
    return(base_one_budget_cds)
  }
  