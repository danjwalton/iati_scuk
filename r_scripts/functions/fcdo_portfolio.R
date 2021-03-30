###
#Function to get former portfolio for FCDO data
###

portfolio <- function(fcdo_data){
  
  #Ensure required packages are installed and attached
  suppressPackageStartupMessages(lapply(c("data.table"), require, character.only=T))
  
  slugs <- data.table(slug_code = c("alb", "bbcws", "british-council", "cssf", "frontlinediplomaticactivity", "programme-strategic", "propertityfund", "z_historic-programme"),
                      `Former portfolio` = c("Arms Length Bodies", "BBC World Service", "British Council", "CSSF", "Frontline Diplomatic Activity", "International subscriptions", "Prosperity fund", "Programme departmental spend")
  )
  
  fcdo_data[, `:=` (slug_code = gsub("fcdo-", "", `Reference dataset`))]
  fcdo_data <- merge(fcdo_data, slugs, all.x = T)
  fcdo_data[is.na(`Former portfolio`), `Former portfolio` := "DFID"]
  
  fcdo_data[, slug_code := NULL]

  return(fcdo_data)
}