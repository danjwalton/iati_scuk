###
#Function to produce required analysis outputs from prettified data
###

analysis_tables <- function(data){
  
  dataset_name <- deparse(substitute(data))
  #Ensure required packages are installed and attached
  suppressPackageStartupMessages(lapply(c("data.table"), require, character.only=T))
  
  ffwrite <- function(x, path="outputs/"){
    if(!is.data.table(x))stop("Data is not a data.table object")
    dataname <- deparse(substitute(x))
    fwrite(x, paste0(path, dataset_name, "_", dataname, ".csv"))
  }
  
  sector_trends_budget <- dcast(data, `Sector name` ~ Year , value.var = "Budget", fun.aggregate = function(x) sum(x, na.rm = T))
  ffwrite(sector_trends_budget)
  
  sector_trends_spend <- dcast(data, `Sector name` ~ Year , value.var = "Total spend", fun.aggregate = function(x) sum(x, na.rm = T))
  ffwrite(sector_trends_spend)
  
  recipient_trends_budget <- dcast(data, `Recipient name` + `Recipient code` ~ Year , value.var = "Budget", fun.aggregate = function(x) sum(x, na.rm = T))
  ffwrite(recipient_trends_budget)
  
  recipient_trends_spend <- dcast(data, `Recipient name` + `Recipient code` ~ Year , value.var = "Total spend", fun.aggregate = function(x) sum(x, na.rm = T))
  ffwrite(recipient_trends_spend)
  
  health_sectors_trends_budget <- dcast(data[`Sector name` == "Health"], `Subsector name` ~ Year , value.var = "Budget", fun.aggregate = function(x) sum(x, na.rm = T))
  ffwrite(health_sectors_trends_budget)
  
  health_sectors_trends_spend <- dcast(data[`Sector name` == "Health"], `Subsector name` ~ Year , value.var = "Total spend", fun.aggregate = function(x) sum(x, na.rm = T))
  ffwrite(health_sectors_trends_spend)
 
  education_sectors_trends_budget <- dcast(data[`Sector name` == "Education"], `Subsector name` ~ Year , value.var = "Budget", fun.aggregate = function(x) sum(x, na.rm = T))
  ffwrite(education_sectors_trends_budget)
  
  education_sectors_trends_spend <- dcast(data[`Sector name` == "Education"], `Subsector name` ~ Year , value.var = "Total spend", fun.aggregate = function(x) sum(x, na.rm = T))
  ffwrite(education_sectors_trends_spend)
   
  education_gender_trends_budget <- dcast(data[`Sector name` == "Education"], `Gender marker` ~ Year , value.var = "Budget", fun.aggregate = function(x) sum(x, na.rm = T))
  ffwrite(education_gender_trends_budget)
  
  education_gender_trends_spend <- dcast(data[`Sector name` == "Education"], `Gender marker` ~ Year , value.var = "Total spend", fun.aggregate = function(x) sum(x, na.rm = T))
  ffwrite(education_gender_trends_spend)
  
  nutrition_recipients_trends_budget <- dcast(data[`Subsector name` == "Basic nutrition"], `Recipient name` + `Recipient code` ~ Year , value.var = "Budget", fun.aggregate = function(x) sum(x, na.rm = T))
  ffwrite(nutrition_recipients_trends_budget)
  
  nutrition_recipients_trends_spend <- dcast(data[`Subsector name` == "Basic nutrition"], `Recipient name` + `Recipient code` ~ Year , value.var = "Total spend", fun.aggregate = function(x) sum(x, na.rm = T))
  ffwrite(nutrition_recipients_trends_spend)
  
}
