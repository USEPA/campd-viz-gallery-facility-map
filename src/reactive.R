## global reactives

programInfo <- reactiveValues(allPrograms = NULL,
                              currentPrograms = NULL,
                              allCompliancePrograms = NULL,
                              currentCompliancePrograms = NULL,
                              currentComplianceProgramsDescriptionTable = NULL,
                              retiredComplianceProgramsDescriptionTable = NULL)


statesMdm <- reactiveVal(NULL)

# unit data
unitData <- reactiveVal(NULL)

# facility map data 
programFacilityData <- reactiveVal(NULL)

latestComplianceYear <- reactiveVal(NULL)


load_global_vars <- function(){
  
  # Get all current programs
  allPrograms <- read.csv(paste0(dataGitRawBase,"programTable.csv"))
  currentPrograms <- allPrograms[(allPrograms$retiredIndicator==FALSE),]
  
  # get retired compliace programs
  retiredCompliancePrograms <- allPrograms[(allPrograms$retiredIndicator==TRUE &
                                              allPrograms$allowanceUIFilter == TRUE),]
  
  # Get all allowance programs 
  #compliancePrograms <- read.csv(paste0(dataGitRawBase,"allowanceProgramTable.csv"))
  allCompliancePrograms <- allPrograms[allPrograms$allowanceUIFilter == TRUE,]
  compliancePrograms <- allCompliancePrograms
  
  for (i in 1:nrow(allCompliancePrograms)){
    if (!is.na(allCompliancePrograms$complianceYears[i])){
      allCompliancePrograms$complianceYears[i] <- list(c(as.integer(unlist(strsplit(compliancePrograms$complianceYears[i], ",")))))
    }
    if (!is.na(allCompliancePrograms$emissionYears[i])){
      allCompliancePrograms$emissionYears[i] <- list(c(as.integer(unlist(strsplit(compliancePrograms$emissionYears[i], ",")))))
    }
  }
  
  currentCompliancePrograms <- allCompliancePrograms[allCompliancePrograms$retiredIndicator == FALSE,]
  
  latestComplianceYear <<- min(na.omit(unlist(lapply(currentCompliancePrograms$programCode, function(program){
    max(unlist(currentCompliancePrograms$complianceYears[currentCompliancePrograms$programCode == program]))
  }))))
  
  currentComplianceProgramsDescriptionTable <- currentCompliancePrograms[,c("programCode",
                                                   "programShorthandDescription",
                                                   "compParameterCode")]
  
  retiredComplianceProgramsDescriptionTable <- retiredCompliancePrograms[,c("programCode",
                                                                            "programShorthandDescription",
                                                                            "compParameterCode")]
  
  
  names(currentComplianceProgramsDescriptionTable) <- c("Program Code",
                                                        "Full Program Name",
                                                        "Pollutant")
  names(retiredComplianceProgramsDescriptionTable) <- c("Program Code",
                                                        "Full Program Name",
                                                        "Pollutant")
  
  
  # Storing states 
  url <- paste0(apiUrlBase,"/master-data-mgmt/states?API_KEY=",apiKEY)
  res = GET(url)
  states <- fromJSON(rawToChar(res$content))
  
  ####### assigning global values #######
  programInfo$allPrograms <<- allPrograms
  programInfo$currentPrograms <<- currentPrograms
  programInfo$allCompliancePrograms <<- allCompliancePrograms
  programInfo$currentCompliancePrograms <<- currentCompliancePrograms
  programInfo$currentComplianceProgramsDescriptionTable <<- currentComplianceProgramsDescriptionTable
  programInfo$retiredComplianceProgramsDescriptionTable <<- retiredComplianceProgramsDescriptionTable
  
  statesMdm <<- states
  
}

load_map_data <- function(){
  # Unit data
  unitData <<- read.csv(file = paste0(dataGitRawBase,"unitData.csv"))
  programFacilityData <<- read.csv(file = paste0(dataGitRawBase,"facilityLocationData.csv"))
}
