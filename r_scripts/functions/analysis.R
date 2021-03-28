###
#Function to produce required analysis outputs from prettified data
###

analysis_tables <- function(dataset_name, fiscal_year = F){
  
  #Install required packages if not already present
  packages <- c("data.table", "jsonlite")
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(packages)
  suppressPackageStartupMessages(lapply(packages, require, character.only=T))
  
  most_recent <- function(dataset_name, path = "rdatas/"){
    files <- list.files(path, pattern = dataset_name)
    downloads <- grep(paste0(dataset_name, "_[[]....-..-..[]]"), files, value = T)
    downloads <- downloads[!grepl("raw", downloads)]
    dates <- as.Date(substr(downloads, nchar(dataset_name)+3, nchar(downloads)-5))
    recent <- files[which.max(dates)]
    if(length(recent) == 1){
      message(paste0("Reading most recent file: ", recent))
      data <- readRDS(paste0(path, recent))
      return(data)
    } else{
      message(paste0("No data found matching the name '", dataset_name, "'. Do you need to download it first?"))
      return(NULL)
    }
  }
  
  data <- most_recent(dataset_name)
  
  ffwrite <- function(x, path="outputs/"){
    if(!is.data.table(x))stop("Data is not a data.table object")
    dataname <- deparse(substitute(x))
    fwrite(x, paste0(path, dataset_name, "_", dataname, ".csv"))
  }
  
  if(fiscal_year){
    data[, `:=` (Year = `Fiscal year`)]
  }
  
  if(!is.null(data)){
    
    tryCatch({
      sector_trends_budget <- dcast(data, `Sector name` ~ Year , value.var = "Budget", fun.aggregate = function(x) sum(x, na.rm = T))
      ffwrite(sector_trends_budget)
    }, error = function(e) warning("Analysis table could not be produced due to missing variable(s).", call. = F, immediate. = T))
    
    tryCatch({
      sector_trends_spend <- dcast(data, `Sector name` ~ Year , value.var = "Total spend", fun.aggregate = function(x) sum(x, na.rm = T))
      ffwrite(sector_trends_spend)
    }, error = function(e) warning("Analysis table could not be produced due to missing variable(s).", call. = F, immediate. = T))
    
    tryCatch({
      recipient_trends_budget <- dcast(data, `Recipient name` + `Recipient code` ~ Year , value.var = "Budget", fun.aggregate = function(x) sum(x, na.rm = T))
      ffwrite(recipient_trends_budget)
    }, error = function(e) warning("Analysis table could not be produced due to missing variable(s).", call. = F, immediate. = T))
    
    tryCatch({
      recipient_trends_spend <- dcast(data, `Recipient name` + `Recipient code` ~ Year , value.var = "Total spend", fun.aggregate = function(x) sum(x, na.rm = T))
      ffwrite(recipient_trends_spend)
    }, error = function(e) warning("Analysis table could not be produced due to missing variable(s).", call. = F, immediate. = T))
    
    tryCatch({
      health_sectors_trends_budget <- dcast(data[`Sector name` == "Health"], `Subsector name` ~ Year , value.var = "Budget", fun.aggregate = function(x) sum(x, na.rm = T))
      ffwrite(health_sectors_trends_budget)
    }, error = function(e) warning("Analysis table could not be produced due to missing variable(s).", call. = F, immediate. = T))
    
    tryCatch({
      health_sectors_trends_spend <- dcast(data[`Sector name` == "Health"], `Subsector name` ~ Year , value.var = "Total spend", fun.aggregate = function(x) sum(x, na.rm = T))
      ffwrite(health_sectors_trends_spend)
    }, error = function(e) warning("Analysis table could not be produced due to missing variable(s).", call. = F, immediate. = T))
    
    tryCatch({
      education_sectors_trends_budget <- dcast(data[`Sector name` == "Education"], `Subsector name` ~ Year , value.var = "Budget", fun.aggregate = function(x) sum(x, na.rm = T))
      ffwrite(education_sectors_trends_budget)
    }, error = function(e) warning("Analysis table could not be produced due to missing variable(s).", call. = F, immediate. = T))
    
    tryCatch({
      education_sectors_trends_spend <- dcast(data[`Sector name` == "Education"], `Subsector name` ~ Year , value.var = "Total spend", fun.aggregate = function(x) sum(x, na.rm = T))
      ffwrite(education_sectors_trends_spend)
    }, error = function(e) warning("Analysis table could not be produced due to missing variable(s).", call. = F, immediate. = T))
    
    tryCatch({
      education_gender_trends_budget <- dcast(data[`Sector name` == "Education"], `Gender marker` ~ Year , value.var = "Budget", fun.aggregate = function(x) sum(x, na.rm = T))
      ffwrite(education_gender_trends_budget)
    }, error = function(e) warning("Analysis table could not be produced due to missing variable(s).", call. = F, immediate. = T))
    
    tryCatch({
      education_gender_trends_spend <- dcast(data[`Sector name` == "Education"], `Gender marker` ~ Year , value.var = "Total spend", fun.aggregate = function(x) sum(x, na.rm = T))
      ffwrite(education_gender_trends_spend)
    }, error = function(e) warning("Analysis table could not be produced due to missing variable(s).", call. = F, immediate. = T))
    
    tryCatch({
      nutrition_recipients_trends_budget <- dcast(data[`Subsector name` == "Basic nutrition"], `Recipient name` + `Recipient code` ~ Year , value.var = "Budget", fun.aggregate = function(x) sum(x, na.rm = T))
      ffwrite(nutrition_recipients_trends_budget)
    }, error = function(e) warning("Analysis table could not be produced due to missing variable(s).", call. = F, immediate. = T))
    
    tryCatch({
      nutrition_recipients_trends_spend <- dcast(data[`Subsector name` == "Basic nutrition"], `Recipient name` + `Recipient code` ~ Year , value.var = "Total spend", fun.aggregate = function(x) sum(x, na.rm = T))
      ffwrite(nutrition_recipients_trends_spend)
    }, error = function(e) warning("Analysis table could not be produced due to missing variable(s).", call. = F, immediate. = T))
    
  }
  
}
