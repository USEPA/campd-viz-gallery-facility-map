# API info
apiUrlBase <- Sys.getenv("API_url_base")
apiKEY <- Sys.getenv("API_KEY")

# allowance compliance stream url
complianceUrl <- paste0(apiUrlBase,"/streaming-services/allowance-compliance?API_KEY=",apiKEY)
# allowance compliance page url
compliancePageUrl <- paste0(apiUrlBase,"/account-mgmt/allowance-compliance?API_KEY=",apiKEY)
# allowance compliance url
complianceApplicableUrl <- paste0(apiUrlBase,"/account-mgmt/allowance-compliance/attributes/applicable?api_key=",apiKEY)
# facilities stream url
facilitiesUrl <- paste0(apiUrlBase,"/streaming-services/facilities/attributes?API_KEY=",apiKEY)
# facilities (applicable) url
facilitiesApplicableUrl <- paste0(apiUrlBase,"/facilities-mgmt/facilities/attributes/applicable?API_KEY=",apiKEY)
# account info url
accountInfoUrl <- paste0(apiUrlBase,"/streaming-services/accounts/attributes?API_KEY=",apiKEY)
# program mdm url
programMdmUrl <- paste0(apiUrlBase,"/master-data-mgmt/programs?API_KEY=",apiKEY)
# program mdm url
statesMdmUrl <- paste0(apiUrlBase,"/master-data-mgmt/states?API_KEY=",apiKEY)


## global API functions

get_facility_data <- function(years){
  
  query <- list(year=(paste0(years, collapse = '|')))
  
  res = GET(facilitiesUrl, query = query)
  
  if (length(res$status_code) != 0){
    if (res$status_code != 200){
      stop(paste("API status code:",res$status_code,annualEmissionsUrl,"..",res$message))
    }
  }
  
  yearFacilityData <- fromJSON(rawToChar(res$content))
  
  yearFacilityData
}

# API calls to get compliance data
# format queryList - list(stateCode = paste0(c("AL"), collapse = '|'),programCodeInfo = paste0(c("ARP"), collapse = '|'))
# where states is a c() vector of elements
get_allow_comp_data <- function(complianceYears, programs=NULL, 
                                states=NULL, facilities=NULL){
  
  query <- list(year=(paste0(complianceYears, collapse = '|')))
  
  if (!is.null(programs)){query <- append(query, list(programCodeInfo = (paste0(programs, collapse = '|'))))}
  if (!is.null(states)){query <- append(query, list(stateCode = (paste0(states, collapse = '|'))))}
  if (!is.null(facilities)){query <- append(query, list(facilityId = (paste0(facilities, collapse = '|'))))}
  
  res = GET(complianceUrl, query = query)
  
  if (length(res$status_code) != 0){
    if (res$status_code != 200){
      stop(paste("API status code:",res$status_code,annualEmissionsUrl,"..",res$message))
    }
  }
  
  yearComplianceData <- fromJSON(rawToChar(res$content))
  
  yearComplianceData
}

# API call to get allowance holdings info for a facility
get_account_info_data <- function(facilities){
  
  query <- list(facilityId = (paste0(facilities, collapse = '|')))
  
  res = GET(accountInfoUrl, query = query)
  
  if (length(res$status_code) != 0){
    if (res$status_code != 200){
      stop(paste("API status code:",res$status_code,annualEmissionsUrl,"..",res$message))
    }
  }
  
  returnData <- fromJSON(rawToChar(res$content))
  
  returnData
}

