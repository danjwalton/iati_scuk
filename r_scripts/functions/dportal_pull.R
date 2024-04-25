###
#This function will pull IATI data from the d-portal dquery API given a range of parameters
###

dportal_pull <- function( reporting_ref="GB-GOV-1"  ,  date_from=NULL  ,  date_to=NULL  ,  policy_markers=T  ,  extra_info=T ){
  
  #Ensure required packages are installed and attached
  suppressPackageStartupMessages(lapply(c("data.table", "jsonlite"), require, character.only=T))
  
  #Establish dquery date parameters if provided in function
  if(!is.null(date_from)){
    day_end_gteq <- paste0(as.character(as.Date(date_from)), "|day_end=null")
    rm(date_from)
  }
  
  if(!is.null(date_to)){
    day_start_lteq <- paste0(as.character(as.Date(date_to)), "|day_start=null")
    rm(date_to)
  }
  
  #Build our parameters based on function inputs
  params <- data.table(param = names(as.list(environment())), value = as.character(as.list(environment())))
  params <- params[value != "NULL"]
  params.flat <- paste0(apply(params, 1, paste, collapse = "="), collapse = "&")
  
  #Base url and activity columns
  dquery_url <- "https://d-portal.org/q?"
  base_select <- "aid,reporting,reporting_ref,funder_ref,title,slug,day_start,day_end,description"
  
  #Activity query url and transaction columns
  acts <- "from=act&orderby=1-&"
  act_select <- "trans_day,trans_usd,trans_gbp,trans_code,trans_country,trans_sector,trans_id"
  act_url <- paste0(dquery_url, acts, "select=", base_select, ",", act_select, "&")
  
  #Transactions query url and transaction columns
  trans <- "from=act,trans&orderby=1-&"
  trans_select <- "trans_day,trans_usd,trans_gbp,trans_code,trans_country,trans_sector,trans_id"
  trans_url <- paste0(dquery_url, trans, "select=", base_select, ",", trans_select, "&")
  
  #Budget query url and budget columns
  budget <- "from=act,budget&orderby=1-&"
  budget_select <- "budget_day_start,budget_day_end,budget_usd,budget_gbp,budget_country,budget_sector,budget_id"
  budget_url <- paste0(dquery_url, budget, "select=", base_select, ",", budget_select, "&")
  
  #Policy marker query url and policy column
  policy <- "from=act,policy&orderby=1-&"
  policy_select <- "aid,policy_code"
  policy_url <- paste0(dquery_url, policy, "select=", policy_select, "&")
  
  #Humanitarian flag query url
  hum <- "from=act&orderby=1-&"
  hum_select <- "aid&*@humanitarian=1"
  hum_url <- paste0(dquery_url, hum, "select=", hum_select, "&") 
  
  #Extra data from JML url
  jml <- "from=act,jml&"
  jml_select <- "jml"
  jml_url <- paste0(dquery_url, jml, "select=", jml_select, "&") 

  #Generic function which downloads and parses dquery results
  data_pull <- function(base_url, params.flat, progress, block = F){
    
    pre_query_url <- gsub(",trans|,budget", "", gsub("select=.*?&", "select=aid&limit=-1&", paste0(base_url, params.flat)))
    pre_response <- fromJSON(pre_query_url)
    
    acts <- unique(pre_response$rows$aid)
    
    if(length(acts) > 0){
      
      per.query <- 1
      
      acts <- suppressWarnings(split(acts, ceiling(seq(length(acts)/per.query)))) #Pull 'per.query' records each time. Higher numbers (10+) tend to lose records
    
      if(progress) pb <- txtProgressBar(0, length(acts), style = 3)
    
      temp_list <- list()
      
      #Iterate through all pages of data
      if(!block){
        
        for(i in 1:length(acts)){
        
          acts_cat <- paste0(acts[[i]], collapse = ",")
        
          query_url <- gsub(" ", "%20", paste0(base_url, params.flat, "&aid=", acts_cat, "&limit=-1"))
        
          response <- fromJSON(query_url)
        
          temp_data <- response$rows
          temp_list[[i]] <- temp_data
        
          if(progress) setTxtProgressBar(pb, i)
          
        }
        
        if(progress) close(pb)  
        
        out <- rbindlist(temp_list)
      
      } else {
        
        query_url <- paste0(base_url, params.flat, "&limit=-1")
        response <- fromJSON(query_url)
        out <- data.table(response$rows)
        
      }
      
    } else { out <- NULL}
    #out <- unique(out) #d-portal will sometimes offer up duplicate results
    return(out)
    
  }
  
  steps <- 2 + ifelse(extra_info, 2, 0)
  
  message(paste0("Step 1 of ", steps, " - downloading transactions:"))
  transactions <- data_pull(trans_url, params.flat, progress = T)
  
  message(paste0("Step 2 of ", steps, " - downloading budgets:"))
  budgets <- data_pull(budget_url, params.flat, progress = T)
  
  budgets$trans_code <- "B"
  
  setnames(transactions, "trans_id", "record_id")
  setnames(budgets, "budget_id", "record_id")
  
  out <- rbind(transactions, budgets, fill = T)
  
  if(policy_markers){
    
    #Download policy marker info
    policies <- data_pull(policy_url, params.flat, progress = F, block = T)
    
    #Function to split the policy marker info and cast to wide data
    split_policies <- function(policies){
      
      if(!is.null(policies)){
        
        policies <- unique(policies)
        
        policy_key <- data.table(policy_code = seq(1,12), policy_name = c("gender","environment","good_governance","trade_development","biological_diversity","climate_mitigation","climate_adaptation","desertification","rmnch","drr","disability","nutrition"))
        
        policies <- policies[, (tstrsplit(policy_code, "_")), by = aid]
        setnames(policies, c("base_aid", "targeting", "policy_code"))
        
        policies$policy_code <- policy_key$policy_name[match(unlist(policies$policy_code), policy_key$policy_code)]
        
        policies <- dcast(policies, base_aid ~ policy_code, value.var = "targeting")
        
        return(policies)
        
      }
    }
    
    policies <- split_policies(policies)
    
    #Join policy markers to activity data based on activity root
    if(!is.null(policies)){
      
      out[, base_aid := ifelse(grepl("-1..$", aid), substr(aid, 0, nchar(aid)-4), aid)]
      out <- merge(out, policies, by = "base_aid", all.x = T)
      out[, base_aid := NULL]
      
    }
  }

  #Download humanitarian flag info
  humanitarian <- data_pull(hum_url, params.flat, progress = F, block = T)
  
  #Join humanitarian flag to activity data
  if(!is.null(humanitarian)){
    humanitarian$humanitarian <- 1
    out <- merge(out, humanitarian, by = "aid", all.x = T)
  }
  
  if(extra_info){
    #Get extra info from JML
    message(paste0("Step 3 of ", steps, " - downloading extra info:"))
    extra <- data_pull(jml_url, params.flat, progress = T)
    
    #Function to grab extra data from nested JML
    get_extra_data <- function(extra){
      
      message(paste0("Step 4 of ", steps, " - parsing extra info (there will be a pause):"))
      
      pb <- txtProgressBar(0, nrow(extra), style = 3)
      extra_list <- list()
      extra_cols <- c("iati-identifier", "recipient-region", "default-finance-type", "default-aid-type", "default-tied-status", "capital-spend", "participating-org")
      
      extra <- apply(extra, 1, fromJSON)
      
      for(i in 1:length(extra)){
        if(any(grepl(paste0(extra_cols, collapse = "|"), extra[[i]]))){
          #extra_temp <- fromJSON(as.character(extra$jml[i]))$`1`
          extra_temp <- extra[[i]]$`1`
          keep <- c("0", "1", "ref", "code")
          extra_temp <- extra_temp[, names(extra_temp) %in% keep]
          extra_temp <- data.table(extra_temp)
          extra_temp <- extra_temp[, lapply(.SD, function(x) paste0(unique(x[!is.na(x)]), collapse = ", ")), by = `0`]
          extra_temp <- setnames(data.table(t(extra_temp)), extra_temp$`0`)[]
          extra_temp$`iati-identifier` <- extra_temp$`iati-identifier`[2]
          extra_temp <- tail(extra_temp, -2)
          
          extra_temp <- extra_temp[, names(extra_temp) %in% extra_cols, with = F]
          extra_temp[extra_temp == "NULL"] <- NA_character_
          extra_temp <- extra_temp[, lapply(.SD, function(x) max(as.character(x), na.rm = T))]
          extra_temp[, setdiff(extra_cols, names(extra_temp)) := ""]
          
          extra_list[[i]] <- extra_temp
          rm(extra_temp)
          setTxtProgressBar(pb, i)
        }
      }
      
      close(pb)
      extra_data <- rbindlist(extra_list, fill = T)
      names(extra_data) <- make.unique(names(extra_data))
      return(extra_data)
      
    }
    
    extra_data <- get_extra_data(extra)
    
    out <- merge(out, extra_data, by.x = "aid", by.y = "iati-identifier", all.x = T)
    
  }
  
  #Function to convert IATI day codes to readable dates
  days_to_date <- function(days, origin = as.Date("1970-01-01")){
    return(origin + as.numeric(days))
  }
  
  date_cols <- c("day_start", "day_end", "trans_day", "budget_day_start", "budget_day_end")
  out[, (date_cols) := data.frame(lapply(.SD, function(x) days_to_date(x))), .SDcols = (date_cols)]
  
  save_params <- paste0("raw_", paste0(params$value, collapse = "_"), "_[", Sys.Date(), "]")
  save_params <- gsub("[|]day_end=null|[|]day_start=null", "", save_params)
  save_loc <- paste0("rdatas/", save_params, ".RData" )
  saveRDS(out, save_loc)
  return(save_loc)
  
}

####

