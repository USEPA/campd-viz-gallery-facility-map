# GitHub raw base 
dataGitRawBase <- "https://raw.githubusercontent.com/USEPA/campd-viz-gallery-data-files/main"

### Program year data ###
# Get all programs and storing appropriate emission and compliance years
res = GET(programMdmUrl)
allPrograms <- fromJSON(rawToChar(res$content))
allPrograms$programDescription <- paste0(
  allPrograms$programDescription, " (",
  allPrograms$programCode, ")")

# Get all current programs
currentPrograms <- allPrograms[allPrograms$retiredIndicator==FALSE,]

# Get all allowance programs 
compliancePrograms <- read.csv(paste0(dataGitRawBase,"/data/allowanceProgramTable.csv"))
allCompliancePrograms <- compliancePrograms

for (i in 1:nrow(allCompliancePrograms)){
  if (!is.na(allCompliancePrograms$complianceYears[i])){
    allCompliancePrograms$complianceYears[i] <- list(c(as.integer(unlist(strsplit(compliancePrograms$complianceYears[i], ",")))))
  }
  if (!is.na(allCompliancePrograms$emissionYears[i])){
    allCompliancePrograms$emissionYears[i] <- list(c(as.integer(unlist(strsplit(compliancePrograms$emissionYears[i], ",")))))
  }
}

currentCompliancePrograms <- allCompliancePrograms[allCompliancePrograms$retiredIndicator == FALSE,]

latestComplianceYear <- min(na.omit(unlist(lapply(currentCompliancePrograms$programCode, function(program){
  max(unlist(currentCompliancePrograms$complianceYears[currentCompliancePrograms$programCode == program]))
}))))

latestEmissionsYear <- min(na.omit(unlist(lapply(currentCompliancePrograms$programCode, function(program){
  max(unlist(currentCompliancePrograms$emissionYears[currentCompliancePrograms$programCode == program]))
}))))

# table to convert column name to appropriate lables for UI
facilityMapLabelConversion <- data.frame(columnName=c("programCode", 
                                                      "programDescription",
                                                      "facilityName", 
                                                      "stateName", 
                                                      "countyName"), 
                                         label=c("Select a Regulatory Program",
                                                 "Select a Regulatory Program",
                                                 "Select a Facility",
                                                 "Select a State",
                                                 "Select a County"),
                                         placeholder=c("--select program--",
                                                       "--select program--",
                                                       "--select facility--",
                                                       "--select state--",
                                                       "--select county--"))

# drop these states in shapefiles
dropStates <- c("American Samoa", "American Samoa", 
                "Commonwealth of the Northern Mariana Islands",
                "Guam", "United States Virgin Islands")

####### County Search Data #######
get_county_search_data <- function(shapefilepath, FIPSfilepath){
  
  USA <- st_read(dsn = shapefilepath)
  
  countyDF <- USA[,c("STATEFP", "COUNTYNS","NAME","geometry"),]
  
  names(countyDF) <- tolower(names(countyDF))
  names(countyDF)[names(countyDF)=="name"] <- c("countyName")
  
  county_sf <- st_as_sf(countyDF)
  
  FIPS <- read.csv(FIPSfilepath)
  
  FIPS$FIPS <- formatC(FIPS$FIPS, width = 2, format = "d", flag = "0", big.mark = "-")
  
  countyState <- merge(county_sf, FIPS, by.x="statefp", by.y="FIPS",all.x=TRUE)
  
  countyState <- countyState[!(countyState$stateName %in% dropStates),]
  countyState
  
}

####### State Search Data #######
get_state_search_data <- function(shapefilepath){
  USA <- st_read(dsn = shapefilepath)
  
  stateDF <- USA[,c("NAME","geometry"),]
  
  names(stateDF) <- tolower(names(stateDF))
  names(stateDF)[names(stateDF)=="name"] <- c("stateName")
  
  state_sf <- st_as_sf(stateDF)
  
  state_sf <- state_sf[order(state_sf$stateName),]
  
  state_sf <- state_sf[!(state_sf$stateName %in% dropStates),]
  state_sf
}

countyStateSf <- get_county_search_data("./shapefiles/cb_2018_us_county_5m.shp","./shapefiles/stateFIPS.csv")
stateSf <- get_state_search_data("./shapefiles/cb_2018_us_state_5m.shp")


## global functions

## store functions

# to get all facility data for downloading off of app
store_facility_data <- function(unitDataBase){
  unitData <- unitDataBase %>%
    mutate("SO2 Controls Installed" = case_when(
      length(na.omit(so2ControlInfo)) != 0 ~ "Yes",
      length(na.omit(so2ControlInfo)) == 0 ~ "No"
    ))
  
  unitData <- unitData %>%
    mutate("NOx Controls Installed" = case_when(
      length(na.omit(noxControlInfo)) != 0 ~ "Yes",
      length(na.omit(noxControlInfo)) == 0 ~ "No"
    ))
  
  unitData <- unitData %>%
    mutate("Particulate Matter Controls Installed" = case_when(
      length(na.omit(pmControlInfo)) != 0 ~ "Yes",
      length(na.omit(pmControlInfo)) == 0 ~ "No"
    ))
  
  unitData <- unitData %>%
    mutate("Mercury Controls Installed" = case_when(
      length(na.omit(hgControlInfo)) != 0 ~ "Yes",
      length(na.omit(hgControlInfo)) == 0 ~ "No"
    ))
  
  facilityTableForDownload <- unitData[,c("facilityName","facilityId","stateCode",
                                          "stateName","county",
                                          "latitude","longitude","unitId","operatingStatus",
                                          "primaryFuelInfo","SO2 Controls Installed",
                                          "NOx Controls Installed", 
                                          "Particulate Matter Controls Installed",
                                          "Mercury Controls Installed",
                                          "year")]
  names(facilityTableForDownload) <- c("Facility Name","Facility Id","State Code",
                                       "State Name","County",
                                       "Latitude","Longitude","Unit Id","Operating Status",
                                       "Primary Fuels","SO2 Controls Installed",
                                       "NOx Controls Installed", 
                                       "Particulate Matter Controls Installed",
                                       "Mercury Controls Installed",
                                       "Year of Operation")
  
  facilityTableForDownload
}
