
# used to get rid of columns that would cause duplication of lat/long data
# programFacilityData has a program column and facilities can be in multiple programs
filter_facility_latlong_data <- function(facilityData){
  unique(facilityData[,c("facilityId","stateCode","stateName","county","facilityName","longitude","latitude")])
}

load_compliance_download_file <- function(){
  #read.csv(file = paste0(dataGitRawBase,"/data/complianceDataTableForDownload.csv"))
  facilitiesForLatestYear <- unique(unitData[,c("facilityId","facilityName")])
  
  ### Collect compliance data for all years ###
  complianceYears <- unique(na.omit(unlist(programInfo$allCompliancePrograms$complianceYears)))
  
  # getting all compliance data for all applicable compliance years
  allYearComplianceFacilityData <- get_allow_comp_data(complianceYears)
  
  complianceFacilityDataLatestYear <- allYearComplianceFacilityData[allYearComplianceFacilityData$year == latestComplianceYear,]
  
  accountInfo <- get_account_info_data(facilities=facilitiesForLatestYear$facilityId)
  accountInfoFac <- unique(accountInfo[accountInfo$accountNumber %like% 'FACLTY',c("accountNumber","facilityId")])
  
  accountInfoFac <- merge(facilitiesForLatestYear,accountInfoFac,by=c("facilityId"),all.x = TRUE)
  
  leftOutFacAccts <- anti_join(accountInfoFac,unique(complianceFacilityDataLatestYear[,c("facilityId","facilityName","accountNumber")]))
  leftOutFacAccts$year <- latestComplianceYear
  
  complianceFacilityDataLatestYearFull <- bind_rows(complianceFacilityDataLatestYear,leftOutFacAccts)
  
  complianceFacilityDataLatestFormat <- bind_rows(lapply(1:nrow(complianceFacilityDataLatestYearFull), function(row){
    if (is.na(complianceFacilityDataLatestYearFull$programCodeInfo[row])){
      compStr <- NA
    }
    else if (is.na(complianceFacilityDataLatestYearFull$excessEmissions[row])){
      compStr <- "Yes"
    }
    else{compStr <- "No"}
    c("Facility Name"=complianceFacilityDataLatestYearFull$facilityName[row], 
      "Facility Id"=complianceFacilityDataLatestYearFull$facilityId[row], 
      "Account Number"=complianceFacilityDataLatestYearFull$accountNumber[row], 
      "Program"=complianceFacilityDataLatestYearFull$programCodeInfo[row], 
      "Year"=complianceFacilityDataLatestYearFull$year[row],
      "In compliance?"=compStr)
  }))
  
  
  allYearComplianceFacilityDataFormat <- bind_rows(lapply(1:nrow(allYearComplianceFacilityData), function(row){
    if (!is.na(allYearComplianceFacilityData[row,"excessEmissions"])){
      c("Facility Name"=allYearComplianceFacilityData$facilityName[row], 
        "Facility Id"=allYearComplianceFacilityData$facilityId[row], 
        "Account Number"=allYearComplianceFacilityData$accountNumber[row], 
        "Program"=allYearComplianceFacilityData$programCodeInfo[row], 
        "Year"=allYearComplianceFacilityData$year[row],
        "In compliance?"="No")
    }
  }))
  
  complianceDataTableForDownload <- rbind(complianceFacilityDataLatestFormat,allYearComplianceFacilityDataFormat)
  complianceDataTableForDownload
}

load_facility_download_file <- function(){
  read.csv(file = paste0(dataGitRawBase,"/data/facilityDataTableForDownload.csv"))
}