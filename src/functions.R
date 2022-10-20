
# debugging/verbose use in Rscript
debugging <- function(...) cat(sprintf(...), sep='', file=stderr())

# used to get rid of columns that would cause duplication of lat/long data
# programFacilityData has a program column and facilities can be in multiple programs
filter_facility_latlong_data <- function(facilityData){
  unique(facilityData[,c("facilityId","stateCode","stateName","county","facilityName","longitude","latitude")])
}
