###
#Function to 'prettify' the data outputs from d-portal and cast to a nice data table
###

prettify <- function(dportal_out, activity_info = T, date_info = F, trans_info = F, budget_info = F, policy_markers = T, extra_info = T, fiscal_year = 1){
  
  #Ensure required packages are installed and attached
  suppressPackageStartupMessages(lapply(c("data.table"), require, character.only=T))
  
  #Get DAC sector names
  subsector_codes <- fread("https://iatistandard.org/reference_downloads/203/codelists/downloads/clv3/csv/en/Sector.csv", showProgress = F)[, c("code", "name")]
  subsector_codes <- subsector_codes[, lapply(.SD, as.character)]
  setnames(subsector_codes, c("Subsector code", "Subsector name"))
  
  #Top level sector names
  sector_codes <- fread("sources/DAC_sector_codes.csv")
  sector_codes <- sector_codes[, lapply(.SD, as.character)]
  
  #Country codes
  country_codes <- fread("https://iatistandard.org/reference_downloads/203/codelists/downloads/clv3/csv/en/Country.csv", showProgress = F, encoding = "UTF-8", na.strings = "")[, c("code", "name")]
  region_codes <- fread("https://iatistandard.org/reference_downloads/203/codelists/downloads/clv3/csv/en/Region.csv", showProgress = F, encoding = "UTF-8")[, c("code", "name")]
  country_codes <- rbind(country_codes, region_codes)
  
  country_codes <- country_codes[, lapply(.SD, as.character)]
  setnames(country_codes, c("Recipient code", "Recipient name"))
  
  #Data table of original column names and readable ones
  activity_info_cols <- data.table(t(data.table(
    #Activity info
    c("aid", "IATI identifier"),
    c("reporting", "Reporting organisation"),
    c("reporting_ref", "Reporting organisation reference"),
    c("funder_ref", "Funder reference"),
    c("title", "Project title"),
    c("slug", "Reference dataset"),
    c("description", "Project description")
  )))
  
  date_info_cols <- data.table(t(data.table(
    #Exact date info
    c("day_start", "Activity start date"),
    c("day_end", "Activity end date"),
    c("trans_day", "Transaction date"),
    c("budget_day_start", "Budget start date"),
    c("budget_day_end", "Budget end date")
  )))
  
  trans_info_cols <- data.table(t(data.table(
    #Transaction info
    c("trans_country", "Transaction recipient country ISO"),
    c("trans_sector", "Transaction sector"),
    c("trans_code", "Transaction type")
  )))
  
  budget_info_cols <- data.table(t(data.table(
    #Budget info
    c("budget_country", "Budget recipient country code"),
    c("budget_sector", "Budget sector"),
    c("budget_code", "Budget type")
  )))
  
  value_cols <- data.table(t(data.table(
    #Transaction value data
    c("trans_usd", "Transaction value (USD)"),
    c("trans_gbp", "Transaction value (GBP)"),
    #Budget value data
    c("budget_usd", "Budget value (USD)"),
    c("budget_gbp", "Budget value (GBP)")
  )))
  
  policy_cols <- data.table(t(data.table(
    #Policy markers
    c("gender", "Gender marker"),
    c("environment", "Environment marker"),
    c("good_governance", "Good governance marker"),
    c("trade_development", "Trade development marker"),
    c("biological_diversity", "Biological diversity marker"),
    c("climate_mitigation", "Climate change mitigation marker"),
    c("climate_adaptation", "Climate change adaptation marker"),
    c("desertification", "Desertification marker"),
    c("rmnch", "RMNCH marker"),
    c("drr", "DRR marker"),
    c("disability", "Disability marker"),
    c("nutrition", "Nutrition marker")
  )))
  
  extra_info_cols <- data.table(t(data.table(
    #Other
    c("participating-org", "Participating organisation"),
    c("recipient-region", "Recipient region code"),
    c("humanitarian", "Humanitarian flag"),
    c("default-tied-status", "Aid tied status"),
    c("default-finance-type", "Finance type"),
    c("default-aid-type", "Aid type"),
    c("capital-spend", "Capital spend")
  )))
  
  #Join all column names together
  col_key <- rbind(activity_info_cols, date_info_cols, trans_info_cols, budget_info_cols, value_cols, policy_cols, extra_info_cols)
  
  #Change old column names to readable ones
  setnames(dportal_out, old = col_key$V1, new = col_key$V2, skip_absent = T)
  
  #Create a basic unique index
  dportal_out[, key := seq(nrow(dportal_out))]
  
  top_sector <- function(subsector_char){
    if(!is.na(subsector_char)){
      sector_char <- paste0(substr(subsector_char,1,2),"0")
    } else {
      sector_char <- NA
    }
    return(sector_char)
  }
  
  get_fy <- function(date, fiscal_year){
    cy <- as.numeric(year(date))
    cq <- as.numeric(quarter(date))
    if(is.numeric(fiscal_year)){
      cy <- ifelse(cq < fiscal_year, paste0(cy - 1, "/", cy), paste0(cy, "/", cy + 1))
    } else{
      fy <- cy
    }
  }
  
  #Merge transaction and budget columns which contain the same type of info
  suppressWarnings(dportal_out[, `:=` (
    `Value USD` = max(`Transaction value (USD)`, `Budget value (USD)`, na.rm = T),
    `Value GBP` = max(`Transaction value (GBP)`, `Budget value (GBP)`, na.rm = T),
    `Sector code` = max(top_sector(`Transaction sector`), top_sector(`Budget sector`), na.rm = T),
    `Subsector code` = max(`Transaction sector`, `Budget sector`, na.rm = T),
    `Recipient code` = ifelse(extra_info, 
                              max(`Transaction recipient country ISO`, `Budget recipient country code`, `Recipient region code`, na.rm = T),
                              max(`Transaction recipient country ISO`, `Budget recipient country code`, na.rm = T)),
    `Year` = max(year(`Transaction date`), year(`Budget start date`), na.rm = T),
    `Quarter` = max(quarter(`Transaction date`), quarter(`Budget start date`), na.rm = T),
    `Fiscal year` = ifelse(fiscal_year,
                           max(get_fy(`Transaction date`, fiscal_year), get_fy(`Budget start date`, fiscal_year), na.rm = T),
                           max(year(`Transaction date`), year(`Budget start date`), na.rm = T))
  ), by = key])
  
  dportal_out <- merge(dportal_out, sector_codes, all.x = T, by = "Sector code")
  dportal_out <- merge(dportal_out, subsector_codes, all.x = T, by = "Subsector code")
  dportal_out <- merge(dportal_out, country_codes, all.x = T, by = "Recipient code")
  
  aggregated_cols  <- c("Sector code", "Sector name", "Subsector code", "Subsector name", "Recipient code", "Recipient name", "Year", "Quarter", "Fiscal year")
  
  select_cols <- function(col_names, data = dportal_out){
    selected <- grep(paste0(col_names, collapse = "|"), names(data), value = T)
    return(selected)
  }
  
  #Create a list of our selected columns present in the data
  cast_cols <- c(if(activity_info) select_cols(activity_info_cols$V2),
                 if(date_info) select_cols(date_info_cols$V2),
                 select_cols(aggregated_cols),
                 if(trans_info) select_cols(trans_info_cols$V2),
                 if(budget_info) select_cols(budget_info_cols$V2),
                 if(policy_markers) select_cols(policy_cols$V2),
                 if(extra_info) select_cols(extra_info_cols$V2)
  )
  
  #Cast a data table based on selected columns  
  cast <- dcast(dportal_out, paste0(paste0("`", cast_cols , "`", collapse = " + "), " ~ `Transaction type`"), value.var = "Value GBP", fun.aggregate = function(x) sum(x, na.rm = T))
  
  #Rename transaction type column headings to English
  setnames(cast, old = c("0", "B", "IF", "C", "D", "E", "IR", "LR", "R", "QP", "Q3", "CG")
           , c("Unknown finance type", "Budget", "Incoming funds", "Commitments", "Disbursements", "Expenditures", "Interest payments", "Loan repayments", "Reimbursements", "Equity purchases", "Sales of equity", "Credit guarantees"),
           skip_absent = T)
  
  total_spend_cols <- grep("Disbursements|Expenditures|Equity purchases", names(cast), value = T)
  
  cast[, `Total spend` := rowSums(sapply(.SD, as.numeric), na.rm = T), .SDcols = (total_spend_cols)]
  
  return(cast)
  
}

###
