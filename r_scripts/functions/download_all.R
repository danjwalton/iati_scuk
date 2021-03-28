###
#Function which combines downloading, prettifying, adding sectors, and portfolio
###

new_download <- function(name = "fcdo_all", reporting_ref = "GB-GOV-1", date_from = NULL, date_to = NULL, policy_markers = T, extra_info = T, sector_fill = T, fcdo_portfolio = T, fiscal_year = 1){
  
  #Install required packages if not already present
  packages <- c("data.table", "jsonlite")
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(packages)
  suppressPackageStartupMessages(lapply(packages, require, character.only=T))
  
  #Pull in functions from scripts
  lapply(c("functions/dportal_pull.R", "functions/prettify.R", "functions/sector_fill.R", "functions/analysis.R", "functions/fcdo_portfolio.R"), function(x) source(x, echo = F))
  
  download <- readRDS(dportal_pull(reporting_ref = reporting_ref, date_from = date_from, date_to = date_to, policy_markers = policy_markers, extra_info = extra_info))
  
  download <- prettify(download, policy_markers = policy_markers, extra_info = extra_info, fiscal_year = fiscal_year)
  
  if(sector_fill) download <- sector_fill(download)
  
  if(fcdo_portfolio) download <- portfolio(download)
  
  output <- paste0("outputs/", name, "_[", Sys.Date(), "].csv")
  output2 <- paste0("rdatas/", name, "_[", Sys.Date(), "].RData")
  
  fwrite(download, output)
  saveRDS(download, output2)
}