###
#This function fills missing top-level sector codes based on a keyword search of project titles and descriptions
###

sector_fill <- function(data, narrative_cols= c("Project title", "Project description"), sector_code_col = "Sector code", sector_name_col = "Sector name"){
  
  #Ensure required packages are installed and attached
  suppressPackageStartupMessages(lapply(c("data.table"), require, character.only=T))
  
  #Function to fill missing sector info
  keyword_sector_fill <- function(data, keywords, sector_code_fill, sector_name_fill){
    
    data_na <-  data[is.na(get(sector_name_col))]
    data_n_na <- data[!is.na(get(sector_name_col))]
    
    data_na[grepl(paste0(keywords, collapse = "|"), apply(data_na[,..narrative_cols],1,  paste, collapse = ""))]$`Sector name` <- sector_name_fill
    data_na[grepl(paste0(keywords, collapse = "|"), apply(data_na[,..narrative_cols],1,  paste, collapse = ""))]$`Sector code` <- sector_code_fill
    
    data <- rbind(data_n_na, data_na)
    
    return(data)
  }
  
  #Education 110
  education <- c(
    "(?i)Global Partnership for Education"
    ,"GPE")
  
  data <- keyword_sector_fill(data, education, "110", "Education")
  
  #Health 120
  health <- c(
    "(?i)Gavi"
    ,"(?i)World Health Organisation"
    ,"WHO"
    ,"AMC"
    ,"IFFIm"
    ,"(?i)Unitaid"
    ,"(?i)Malaria"
    ,"Global Fund"
  )
  
  data <- keyword_sector_fill(data, health, "120", "Health")
  
  #Water Supply & Sanitation 140
  wash <- c(
    "WASH"
  )
  
  data <- keyword_sector_fill(data, wash, "140", "Water Supply & Sanitation")
  
  #Emergency Response 720
  emergency <- c(
    "(?i)Red Cross"
    ,"CERF"
  )
  
  data <- keyword_sector_fill(data, emergency, "720", "Emergency Response")
  
  return(data)
}
