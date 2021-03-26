#Install required packages if not already present
packages <- c("data.table", "jsonlite")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(packages)
suppressPackageStartupMessages(lapply(packages, require, character.only=T))

#Pull in functions from scripts
lapply(c("functions/download_all.R", "functions/analysis.R"), function(x) source(x, echo = F))

###
#Run sections below for complete download, parse, and analysis
###

###
#FCDO
###

#Download - only run if you want a new version of data
new_download(name = "fcdo_all", reporting_ref = "GB-GOV-1", date_from = NULL, date_to = NULL, policy_markers = T, extra_info = T, sector_fill = T, fcdo_portfolio = T)

#Analysis
analysis_tables("fcdo_all")

###

###
#World Bank
###

#Download - only run if you want a new version of data
new_download(name = "world_bank_all", reporting_ref = "44000", date_from = NULL, date_to = NULL, policy_markers = T, extra_info = T, sector_fill = T, fcdo_portfolio = F)

#Analysis
analysis_tables("world_bank_all")

###

###
#NORAD
###

#Download - only run if you want a new version of data
new_download(name = "norad_all", reporting_ref = "NO-BRC-971277882", date_from = NULL, date_to = NULL, policy_markers = T, extra_info = T, sector_fill = T, fcdo_portfolio = F)

#Analysis
analysis_tables("norad_all")

###
