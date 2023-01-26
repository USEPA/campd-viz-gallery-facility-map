
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
countyStateSf <- countyStateSf[with(countyStateSf, order(stateName, countyName)), ]
stateSf <- get_state_search_data("./shapefiles/cb_2018_us_state_5m.shp")

