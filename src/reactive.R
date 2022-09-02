## global reactives

programInfo <- reactiveValues(allPrograms = NULL,
                              allCompliancePrograms = NULL,
                              currentCompliancePrograms = NULL,
                              descriptionTable = NULL)


statesMdm <- reactiveVal(NULL)

# unit data
unitData <- reactiveVal(NULL)

# facility map data 
programFacilityData <- reactiveVal(NULL)


load_global_vars <- function(){
  
  descriptionTable <- currentCompliancePrograms[,c("programCode",
                                                   "programDescription",
                                                   "compParameterCode",
                                                   "programGroupCode",
                                                   "programGroupDescription")]
  
  
  names(descriptionTable) <- c("Program Code",
                               "Full Program Name",
                               "Pollutant",
                               "Program Group",
                               "Program Group Description")
  
  
  # Storing states 
  url <- paste0(apiUrlBase,"/master-data-mgmt/states?API_KEY=",apiKEY)
  res = GET(url)
  states <- fromJSON(rawToChar(res$content))
  
  # Unit data
  unitData <<- read.csv(file = paste0(dataGitRawBase,"/data/unitData.csv"))
  
  ####### assigning global values #######
  programInfo$allPrograms <<- allPrograms
  programInfo$allCompliancePrograms <<- allCompliancePrograms
  programInfo$currentCompliancePrograms <<- currentCompliancePrograms
  programInfo$descriptionTable <<- descriptionTable
  
  statesMdm <<- states
  
  programFacilityData <<- read.csv(file = paste0(dataGitRawBase,"/data/facilityLocationData.csv"))
  
}
