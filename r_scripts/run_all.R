#Install required packages if not already present
packages <- c("data.table", "jsonlite","rstudioapi")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(packages)
suppressPackageStartupMessages(lapply(packages, require, character.only=T))

#Set working directory
setwd(dirname(getActiveDocumentContext()$path))

#Pull in functions from scripts
lapply(c("functions/download_all.R", "functions/analysis.R"), function(x) source(x, echo = F))

###
#Run sections below for complete download, parse, and analysis
###

###
#FCDO
###

#Download - only run if you want a new version of data. Enter data parameters you want to use.
new_download(
  name = "fcdo_all", #Choose a name for the dataset you want.
  reporting_ref = "GB-GOV-1", #The IATI publisher reference you want data for (optional, multiple can be separated by a comma, character or NULL)
  date_from = "2015-01-01", #Lower bound date for IATI activity to be active (optional, in the form "YYYY-MM-DD" or NULL)
  date_to = "2025-12-31", #Upper bound date for IATI activity to be active (optional, in the form "YYYY-MM-DD" or NULL)
  policy_markers = T, #Option to download policy markers with the data (T/F)
  extra_info = T, #Option to download extra info with the data (aid type, flow type, regional transaction codes etc.) (T/F)
  sector_fill = T, #Option to fill top-level sector codes based on a keyword search of activities (T/F)
  fcdo_portfolio = T, #Option to specify previous portfolio - applicable to FCDO only (T/F)
  fiscal_year = 2 #Specify the quarter when a fiscal year should start, or F (F,2,3,4)
)

#Analysis - run once a download is complete. This will automatically choose the most recent available download to analyse.
analysis_tables("fcdo_all", fiscal_year = T) #Enter the name of the dataset you want to analyse, and whether to use fiscal year (T/F)

###

###
#World Bank
###

#Download - only run if you want a new version of data
new_download(
  name = "world_bank_all",
  reporting_ref = "44000",
  date_from = NULL,
  date_to = NULL,
  policy_markers = T,
  extra_info = T,
  sector_fill = T
)

#Analysis
analysis_tables("world_bank_all", fiscal_year = F)

###

###
#NORAD
###

#Download - only run if you want a new version of data
new_download(
  name = "norad_all",
  reporting_ref = "NO-BRC-971277882",
  date_from = NULL,
  date_to = NULL,
  policy_markers = T,
  extra_info = T,
  sector_fill = T
)

#Analysis
analysis_tables("norad_all", fiscal_year = F)

###
