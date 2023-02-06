
load_one_budget_tracker <- function() {
  print(" >> Loading One Budget Tracker data...")
  base_one_budget_tracker <- data.frame(
    read_excel(
      "data/input/base_one_budget_tracker.xlsx",
      sheet = "One Budget tracker")
    )
  
  print(" >> Selecting relevant columns...")
  base_one_budget_tracker <- select(
    base_one_budget_tracker,
    c(
      "ISO",
      "One.Budget.Status",
      "Progress.Tracker...current.step.being.worked.on...achieved"
      )
    )

  print(" >> Renaming Columns...")
  colnames(base_one_budget_tracker) <- c(
    "a_iso",
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
      "data/input/base_one_budget_tracker.xlsx",
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
    read_excel("data/input/base_financing_urgent.xlsx",
               sheet = "Funding tracker"
    )
  )
  
  print(" >> Selecting relevant columns...")
  base_requests <- select(
    base_requests,
    c(
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
    base_one_budget_cds$cds3_application_status[base_one_budget_cds$cds3_application_status == "Submitted before Sep 30 (CDS2 )
Planning future submission of CDS3"] <- "Submitted"
    base_one_budget_cds$cds3_application_status[base_one_budget_cds$cds3_application_status == "Submitted before Sep 30"] <- "Submitted"
    base_one_budget_cds$cds3_application_status[base_one_budget_cds$cds3_application_status == "New deadline offcially communicated 15.11.2022"] <- "New deadline"
    
    base_one_budget_cds$cds3_application_status[base_one_budget_cds$cds3_application_status == "Delay expected- NB request submitted end of August"] <- "Delay expected"
    base_one_budget_cds$cds3_application_status[base_one_budget_cds$cds3_application_status == "Delay expected (1-2 weeks - tracking towards 15th Oct)"] <- "Delay expected"
    base_one_budget_cds$cds3_application_status[base_one_budget_cds$cds3_application_status == "Delay expected - just received NBW"] <- "Delay expected"
    base_one_budget_cds$cds3_application_status[base_one_budget_cds$cds3_application_status == "Delay expected  - tracking towards mid-October"] <- "Delay expected"
    
    base_one_budget_cds$cds3_technical_assistance[base_one_budget_cds$cds3_technical_assistance == "Requested and provided by DO and SL"] <- "Requested and provided"
    base_one_budget_cds$cds3_technical_assistance[base_one_budget_cds$cds3_technical_assistance == "Requested and being provided by DO"] <- "Requested and provided"
    base_one_budget_cds$cds3_technical_assistance[base_one_budget_cds$cds3_technical_assistance == "provided by partners"] <- "Requested and provided"
    base_one_budget_cds$cds3_technical_assistance[base_one_budget_cds$cds3_technical_assistance == "Provided"] <- "Requested and provided"
    base_one_budget_cds$cds3_technical_assistance[base_one_budget_cds$cds3_technical_assistance == "Person TBC"] <- "TBC"
    base_one_budget_cds$cds3_technical_assistance[base_one_budget_cds$cds3_technical_assistance == "TBD - partner to be tapped"] <- "TBC"
    
    return(base_one_budget_cds)
  }
  
  merge_one_budget_tracker <- function(a_data, base_one_budget_tracker) {
    a_data<-merge(x=a_data,y=base_one_budget_tracker, by="a_iso",all.x=TRUE)
    
    print(" >> Finished 'merge_one_budget_tracker' function...")
    return(a_data)
  }
    
  
  