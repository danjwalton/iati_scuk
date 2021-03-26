#Install required packages if not already present
packages <- c("data.table", "jsonlite")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(packages)
suppressPackageStartupMessages(lapply(packages, require, character.only=T))

#Pull in functions from scripts
lapply(c("functions/dportal_pull.R", "functions/prettify.R", "functions/sector_fill.R", "functions/analysis.R", "functions/fcdo_portfolio.R"), function(x) source(x, echo = F))

###
#Run sections below for complete download, parse, and analysis
###

#FCDO
fcdo <- readRDS(dportal_pull(reporting_ref = "GB-GOV-1", date_from = NULL, date_to = NULL))
fcdo <- prettify(fcdo, policy_markers = T, extra_info = T)
fcdo <- sector_fill(fcdo)
fcdo <- portfolio(fcdo)
fwrite(fcdo, "outputs/fcdo_all.csv")
analysis_tables(fcdo)

#World Bank
world_bank <- readRDS(dportal_pull(reporting_ref = "44000", date_from = NULL, date_to = NULL))
world_bank <- prettify(world_bank, policy_markers = T, extra_info = T)
world_bank <- sector_fill(world_bank)
fwrite(world_bank, "outputs/world_bank_all.csv")
analysis_tables(world_bank)

#NORAD
norad <- readRDS(dportal_pull(reporting_ref = "NO-BRC-971277882", date_from = NULL, date_to = NULL))
norad <- prettify(norad, policy_markers = T, extra_info = T)
norad <- sector_fill(norad)
fwrite(norad, "outputs/norad_all.csv")
analysis_tables(norad)
