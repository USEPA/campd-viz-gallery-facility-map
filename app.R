# App

# clear quotes below while deploying

'library_path <- paste("Library Path: ", Sys.getenv(c("LD_LIBRARY_PATH")))
print(paste("LD_LIBRARY_PATH: ", library_path))

lib_dir <- "/home/vcap/deps/0/r/lib"
local_lib_dir <- "r-lib"
local_bin_dir <- "r-bin"

if (dir.exists(lib_dir)) {
  if (dir.exists(local_lib_dir)) {
    # Get the list of libs
    lib_tars <- list.files(local_lib_dir)
    lib_tars <- paste(local_lib_dir, lib_tars, sep = "/")
    
    print(paste("Local libs: ", lib_tars))
    print(paste("Working directory: ", list.files(getwd())))
    
    # Copy the files to the lib_dir
    for (i in 1:length(lib_tars)) {
      untar(lib_tars[i], exdir = lib_dir)
    }
    
    Sys.setenv(PROJ_LIB = lib_dir)
    
  }
  if (dir.exists(local_bin_dir)) {
    Sys.setenv(RMARKDOWN_PANDOC = local_bin_dir)
    Sys.setenv(PATH = paste("/home/vcap/app/",
                            local_bin_dir, ":${PATH}", sep = ""))
  }
  print(list.files(lib_dir))
}'

library(shiny)
library(shinydashboard)
library(shinybusy)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(shinydisconnect)
library(sf)
library(leaflet)
library(leaflet.extras)
library(httr)
library(htmltools)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(data.table)
library(curl)
library(epaRShinyTemplate)

#install.packages(paste0(getwd(),'/tmp/epaRShinyTemplate_0.0.4.tar.gz'), repos=NULL)

library(epaRShinyTemplate)

# get application environment variables
#APPLICATION_ENV_VARIABLES <- fromJSON(Sys.getenv(c("VCAP_APPLICATION")))

# src scripts
source("./src/static.R")
source("./src/functions.R")

# modules
source("./modules/search.R")
source("./modules/display-table.R")
source("./modules/display-list.R")

ui <- tags$main(
  
  # load in shiny dashboard - use this first so epaSlimHeader can override some styling
  useShinydashboard(),
  
  # easey design template
  #epaSlimHeader("epaNav",APPLICATION_ENV_VARIABLES$space_name),
  epaSlimHeader("epaNav","local"),
  
  # dark blue banner
  div(class="banner",
      h1(class="banner-conents","Facility Map"),
  ),
  
  # main page
  div(
    
    # adding html js and style essentials
    tags$head(
      useShinyjs(),
      includeScript('www/script.js'),
      HTML("<title>Facility Map</title>")
    ),
    
    tags$html(class="CAMPDRShiny",lang="en"),
    class="main-page",
    includeCSS("www/style.css"),
    
    # adding load spinner
    add_busy_spinner(
      spin = "fading-circle",
      color = "#112446",
      timeout = 100,
      position = c("full-page"),
      height = "50px",
      width = "50px"
    ),
    # disconnect
    disconnectMessage(
      text = "Your session timed out! Please reload the application.",
      refresh = "Reload now",
      background = "white",
      colour = "#000000",
      overlayColour = "#f0f0f0",
      overlayOpacity = 0.3,
      refreshColour = "#1a4480"
    ),
    
    ############ Main UI ############
    
    tags$div(class="padding-y-2 mobile-lg:padding-x-2 tablet:padding-x-4 widescreen:padding-x-10 font-sans-xs text-base-darkest text-ls-1 line-height-sans-5",
        
        h2(class="font-sans-xl text-bold","Getting Started"),
        
        uiOutput("gettingStartedText"),
        
        div(id = "how-to-use-map-box", box(
          uiOutput('howToUseBox'), 
          title = "How to use the map",  
          collapsible = TRUE, 
          collapsed = TRUE, 
          status = "primary",
          width = 12)
        ),
        div(id="source-data-box", box(
          div(
            p("Use the download buttons below to obtain ",tags$strong("all data available in the map.")),
            p("NOTE: The CSV tables are not produced based on your Location Search or Facility Filters selections."),
            div(class="grid-container download-buttons",
                div(class="grid-row",
                    div(class="grid-col-auto",
                        downloadButton(class="usa-button","download_facility_data", "Download Facility Data CSV"),
                        downloadButton(class="usa-button","download_compliance_data", "Download Compliance Data CSV")
                    )
                )
                
            )),
          title = "Source Data",  
          collapsible = TRUE, 
          collapsed = TRUE, 
          status = "primary",
          width = 12)
        ),
        
        fluidRow(id="filtersearch-container",
                 column(8, 
                        div(class='column-decorator', div(h3(class="font-sans-lg text-bold", 
                                                             bsButton(class="usa-button","search-tooltip", label = "Search Tooltip", 
                                                                      icon = icon("question"), style = "info", 
                                                                      size = "extra-small"),'Location Search')),
                            bsPopover(id = "search-tooltip", title = "Using the searches",
                                      content = paste0("Use the search dropdown to highlight specific locations and zoom into the ", 
                                                       "location to find nearby facilities."),
                                      placement = "right", 
                                      trigger = "focus", 
                                      options = list(container = "body")
                            ),
                            fluidRow(
                              column(5,
                                     searchUI('stateSearchInput', 
                                              placeholder='--search by state-- ', 
                                              label=div('State Name'),
                                              df=stateSf, 
                                              group="", 
                                              items='stateName')),
                              column(5,
                                     searchUI('countySearchInput', 
                                              placeholder='--search by county-- ', 
                                              label='County Name',
                                              df=countyStateSf, 
                                              group='stateName', 
                                              items='countyName'))
                            ),
                            
                            div(class="select-clear", actionButton(class="usa-button","clearSearch","Clear Search")),
                            
                        )
                 ),
                 column(4, 
                        div(class="column-decorator", div(h3(class="font-sans-lg text-bold", 
                                                             bsButton(class="usa-button", "filter-tooltip", label = "Filter Tooltip", 
                                                                      icon = icon("question"), style = "info", 
                                                                      size = "extra-small"),
                                                             "Facility Filters")),
                            bsPopover(id = "filter-tooltip", title = "Using the filter",
                                      content = paste0("Use the program filter to refine your search of facilities within a regulatory ", 
                                                       "program across the United States."),
                                      placement = "right", 
                                      trigger = "focus", 
                                      options = list(container = "body")
                            ),
                            fluidRow(column(10,selectizeInput("programSelection", 
                                                              label="Select a Regulatory Program",
                                                              choices=c("Select All"),
                                                              selected=c("Select All"),
                                                              multiple = FALSE))),
                            div(class="select-clear", actionButton(class="usa-button","clearFilters","Clear Filters"))
                        )
                        
                 )
            ),
        ),
    div(class="maparea",
      absolutePanel(id = "facility-map-panel", 
                      class = "panel panel-default",
                      draggable = FALSE,
                      div(class="font-sans-xs line-height-sans-5",
                          id = "facility-summary-box", 
                          box(uiOutput("fac_summary_text"),
                              title = "Facility Summary",  
                              collapsible = TRUE, 
                              status = "primary",
                              width = 12)
                      ),
                      div(class="font-sans-xs line-height-sans-5",
                          id = "compliance-summary-box", 
                          box(uiOutput("acct_summary_text"), 
                              title = "Compliance Summary",  
                              collapsible = TRUE, 
                              status = "primary",
                              width = 12)
                      )
    ),
    leafletOutput("map",width="100%", height = "600px")
        
    ),
    
    div(class="position-relative",
        div(class="position-fixed .pin-bottom.pin-x z-top",
            #epaSlimFooter("epaFoot", appVersion = Sys.getenv(c("APP_VERSION")), appPublished = format(Sys.Date(),"%a %b %d %Y"))
            epaSlimFooter("epaFoot", appVersion = "v1.0.0", appPublished = format(Sys.Date(),"%a %b %d %Y"))
        )
    )
    
    #################################
    
  )
)

server <- function(input, output, session) {
  
  # GitHub raw base 
  dataGitRawBase <- "https://raw.githubusercontent.com/USEPA/campdRShinyDataSource/main/data/facility-map/"
  getDataGitRaw <- "https://github.com/USEPA/campdRShinyDataSource/raw/main/data/facility-map/"
  if (Sys.getenv(c("USE_BACKUP_GIT")) == 'true'){
    dataGitRawBase <- "https://raw.githubusercontent.com/USEPA/campdRShinyDataSource/backup/data/facility-map/"
    getDataGitRaw <- "https://github.com/USEPA/campdRShinyDataSource/raw/backup/data/facility-map/"
  }
  
  # Get all current programs
  allPrograms <- read.csv(paste0(dataGitRawBase,"programTable.csv"))
  currentPrograms <- allPrograms[(allPrograms$retiredIndicator==FALSE),]
  filterByPrograms <- currentPrograms %>% filter(!(programCode %in% c('NHNOX','NSPS4T','SIPNOX')))
  
  # Get all allowance programs 
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
  
  latestComplianceYear <- min(na.omit(unlist(lapply(currentCompliancePrograms$programCode, function(program){
    max(unlist(currentCompliancePrograms$complianceYears[currentCompliancePrograms$programCode == program]))
  }))))
  
  # MAIN DATA SETS #
  unitData <- read.csv(file = paste0(dataGitRawBase,"facilityDataTableForDownload.csv"))
  complianceData <- read.csv(file = paste0(dataGitRawBase,"complianceDataTableForDownload.csv"))
  
  updateSelectizeInput(session, "programSelection",
                       choices=c("Select All",unique(na.omit(filterByPrograms$programShorthandDescription))))
  
  output$gettingStartedText <- renderUI({ 
    div(
      p("This map currently uses ",tags$strong(as.character(latestComplianceYear)),
          " data to show the location of facilities along with their basic attribute information 
          and compliance performance as part of EPA’s emissions trading programs (NOTE: Compliance 
          data is not available for non-allowance programs such as Acid Rain Program NOx or state 
          programs such as Regional Greenhouse Gas Initiative). More resources on these programs 
          can be found at ",
          tags$a(class="usa-link",href="https://www.epa.gov/airmarkets/programs", 
                 "EPA's Clean Air Markets Programs website",
                 target="_blank", .noWS = "outside"),
          ".", .noWS = c("after-begin", "before-end")),
      
      p("Interested in exploring the data further? Check out ",
        tags$a(class="usa-link",href="https://campd.epa.gov/data", 
               "CAMPD's data section",
               target="_blank", .noWS = "outside"),
        "!", .noWS = c("after-begin", "before-end")),
      p("Expand the boxes below to see more."),
    )
  })
  
  output$howToUseBox <- renderUI({
    div(
      tags$ul(class="intro-list",
              tags$li("The ",
                      tags$img(src='https://raw.githubusercontent.com/USEPA/campdRShinyDataSource/main/images/cluster.png', 
                               width='27px',height='27px',alt="facility cluster marker",
                               style="min-width:27px;"),
                      " icons on the map represent an area where two or more facilities are located. 
                      The number in the middle represents the number of facilities in that general area."),
              tags$li("You can zoom in on the map to reveal the individual facilities, marked by ",
                      tags$img(src='https://unpkg.com/leaflet@1.3.1/dist/images/marker-icon-2x.png', 
                               width='20px',height='35px',alt = "facility marker"),
                      "."),
              tags$li("Hover your mouse pointer over the ",
                      tags$img(src='https://unpkg.com/leaflet@1.3.1/dist/images/marker-icon-2x.png', 
                               width='20px',height='35px',alt = "facility marker"),
                      " to see the facility name."),
              tags$li("Click on a ",
                      tags$img(src='https://unpkg.com/leaflet@1.3.1/dist/images/marker-icon-2x.png', 
                               width='20px',height='35px',alt = "facility marker"),
                      " on the map to view a facility's attribute and 
      program compliance information using the map's side panel ",
                      '"Facility Summary" and "Compliance Summary" expandable boxes.'),
              tags$li('Use the state and county search to find facilities near you.'),
              tags$li(class="indent",'Selecting a state first will refine the county 
              search to only counties within the selected state.'),
              tags$li(class="indent",'Note that the county search box is grouped by state.'),
              tags$li('Use the facility filter to narrow down facilities by regulatory program.'),
              tags$li('Use the clear buttons to clear filters or searches.'),
              tags$li(paste0('Facility Summary displays basic facility/unit attribute information 
              for the ',as.character(latestComplianceYear),' operating year.')),
              tags$li(paste0("Compliance Summary displays compliance information for 
              CAMD's emissions trading programs applicable to the chosen facility for ",
                             as.character(latestComplianceYear),' and 
              any previous years the facility was non-compliant.')),
      )
    )
  })
  
  output$download_facility_data <- downloadHandler(
    filename =  function() { paste0("facility-data-",as.character(latestComplianceYear),".csv") },
    content = function(file) {
      GET(paste0(getDataGitRaw,"facilityDataTableForDownload.csv"), write_disk(file))
    }
  )
  
  output$download_compliance_data <- downloadHandler(
    filename =  function() { paste0("compliance-data.csv") },
    content = function(file) {
      GET(paste0(getDataGitRaw,"complianceDataTableForDownload.csv"), write_disk(file))
    }
  )
  
  # State and county searches
  stateFilterVal <- reactiveVal({NULL})
  countyFilterVal <- reactiveVal({NULL})
  clearSearchTrigger <- reactiveVal({1})
  markerData <- reactiveVal({NULL})
  
  stateSearch <- callModule(searchServer,"stateSearchInput",
                            stateSf,reactive({input$clearSearch}),
                            filterBy='stateName',
                            filterVal=reactive(c()))
  coutSearch <- callModule(searchServer,"countySearchInput",
                           countyStateSf,reactive({input$clearSearch}),
                           filterBy='stateName',
                           filterVal=stateFilterVal)
  
  observeEvent(stateSearch(),{
    if (nrow(stateSearch()) != 0){
      # State search selected is filled to pass to county search in order to clear the
      # county search
      stateFilterVal(stateSearch()$stateName)
      # display county outline and zoom to location
      update_map_search(stateSf,stateSearch(),'stateName')
    }
  }, ignoreInit = TRUE)
  observeEvent(coutSearch(),{
    if (nrow(coutSearch()) != 0){
      # County search selected is filled to pass to state search in order to clear the
      # state search
      countyFilterVal(coutSearch()$stateName)
      # display county outline and zoom to location
      update_map_search(countyStateSf,coutSearch(),'countyns')
    }
  },ignoreNULL = TRUE)
  # If something is selected in one of the searches, the map is cleared and
  # re-rendered, otherwise nothing happens
  observeEvent(input$clearSearch,{
    if(!(is.null(countyFilterVal())) | !(is.null(stateFilterVal()))){
      stateFilterVal(NULL)
      countyFilterVal(NULL)
      if (length(input$programSelection) != 0){
        if (input$programSelection == "Select All"){
          update_full_map(markerData())
        }
        else{
          shapeFileData <- stateSf[stateSf$stateName %in% unique(markerData()$stateName),]
          update_map_filter_selections(markerData(),shapeFileData)
        }
      }
    }
  },ignoreInit = TRUE)
  
  # If program filter changes, and has a value, searches are cleared 
  # map is rendered to the facilities in the filter selections
  observeEvent(input$programSelection,{
    if (length(input$programSelection) != 0){
      if (input$programSelection != "Select All"){
        programSelected <- currentPrograms$programCode[currentPrograms$programShorthandDescription == input$programSelection]
        
        markerData(filter_facility_latlong_data(unitData[grepl(paste0('\\',programSelected,'\\b'), unitData$programCodeInfo),]))
        
        if (!is.null(stateFilterVal()) | !is.null(countyFilterVal())){
          update_map_with_shape_maintained(markerData())
        }
        else{
          shapeFileData <- stateSf[stateSf$stateName %in% unique(markerData()$stateName),]
          update_map_filter_selections(markerData(),shapeFileData)
        }
      }
      else{
        markerData(filter_facility_latlong_data(unitData))
        if (!is.null(stateFilterVal()) | !is.null(countyFilterVal())){
          update_map_with_shape_maintained(markerData())
        }
        else{
          update_full_map(markerData())
        }
        
      }
    }
  })
  # Clear filters and reinitialize the map
  observeEvent(input$clearFilters,{
    if (input$programSelection != "Select All"){
      updateSelectizeInput(
        session = session,
        inputId = "programSelection",
        selected = c("Select All"))
      
    }
  }, ignoreInit = TRUE)
  
  # When map is clicked, or selection is made, clear popups and side panels
  observeEvent(c(input$map_click,input$programSelection,stateSearch(),coutSearch()),{
    event <- input$map_map_click
    output$fac_summary_text <- renderUI({HTML("")})
    output$acct_summary_text <- renderUI({HTML("")})
    leafletProxy("map") %>% clearPopups()
  },ignoreInit = TRUE)
  
  # Initialize leaflet
  output$map <- renderLeaflet({
    arpShapeFileData <- stateSf[stateSf$stateName %in% unique(unitData$stateName[grepl(paste0('\\','ARP','\\b'), unitData$programCodeInfo)]),]
    matsShapeFileData <- stateSf[stateSf$stateName %in% unique(unitData$stateName[grepl(paste0('\\','MATS','\\b'), unitData$programCodeInfo)]),]
    
    legendText <- tagList(div(class="display-table font-sans-xs line-height-sans-2 maxw-mobile padding-x-1",
                              div(class="display-table-row padding-bottom-1",
                                  div(class="display-table-cell padding-right-1 text-middle text-center",
                                      tags$img(src='https://raw.githubusercontent.com/USEPA/campdRShinyDataSource/main/images/cluster.png', 
                                               width='27px',height='27px',alt="facility cluster marker",
                                               style="min-width:27px;")),
                                  div(class="display-table-cell padding-y-1",
                                      "indicates the number of facilities in that general area")
                                  ),
                              div(class="display-table-row",
                                  div(class="display-table-cell padding-right-1 text-middle text-center",
                                      tags$img(src='https://unpkg.com/leaflet@1.3.1/dist/images/marker-icon-2x.png',
                                               width='20px',height='35px',alt="facility marker",
                                               style="min-width:20px;")),
                                  div(class="display-table-cell padding-y-1",
                                      "indicates one facility")))
                                  )
    leaflet() %>%
      addTiles() %>% 
      addPolygons(data = arpShapeFileData,
                  layerId = ~paste0('ARP-',arpShapeFileData[['stateName']]), 
                  group = 'ARP',
                  color = 'green',
                  fillColor = 'green',
                  opacity = 1.0) %>%
      hideGroup('ARP') %>%
      addPolygons(data = matsShapeFileData,
                  layerId = ~paste0('MATS-',matsShapeFileData[['stateName']]), 
                  group = 'MATS',
                  color = 'green',
                  fillColor = 'green',
                  opacity = 1.0) %>%
      hideGroup('MATS') %>%
      fitBounds(lng1 = -158.1283, 
                lat1 = 17.9477,
                lng2 = -66.1037,
                lat2 = 63.8542) %>%
      addControl(legendText, position = "bottomleft" )
  })
  
  # min Longitude , min Latitude , max Longitude , max Latitude 
  # "boundingbox":[-158.1283,17.9477,-66.1037,63.8542]
  # Update map and include shape file outline that was selected in the search
  update_map_search <- function(shapeFileData,searchRow,layer){
    bbox <- st_bbox(shapeFileData[searchRow,]) %>% 
      as.vector()
    leafletProxy("map",data=searchRow) %>% 
      hideGroup('ARP') %>%
      hideGroup('MATS') %>%
      removeShape(unique(stateSf$stateName)) %>% 
      removeShape(unique(countyStateSf$countyns)) %>% 
      addPolygons(layerId = ~searchRow[[layer]], 
                  color = 'green',
                  fillColor = 'green',
                  opacity = 1.0) %>% 
      fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
  }
  
  # Update map with filter selections - states outlined
  update_map_filter_selections <- function(markerData,shapeFileData){
    map <- leafletProxy("map",data=shapeFileData) %>% 
      clearMarkerClusters() %>% clearPopups() %>% clearMarkers() %>% 
      removeShape(unique(stateSf$stateName)) %>% 
      removeShape(unique(countyStateSf$countyns)) %>% 
      addMarkers(data = markerData,
                 lng = ~longitude, lat = ~latitude, 
                 options = markerOptions(alt=~htmlEscape(facilityName), 
                                         `aria-label`="facility marker", 
                                         lat=~latitude,
                                         lng=~longitude), 
                 layerId = ~facilityId,
                 label = ~facilityName,
                 clusterOptions = markerClusterOptions(
                   iconCreateFunction = JS("function (cluster) {    
        var markers = cluster.getAllChildMarkers();
        var latList = []; 
        var lngList = []; 
        for (i = 0; i < markers.length; i++) {
          var markerlats = [markers[i].options.lat];
          var markerlngs = [markers[i].options.lng];
          var latList = latList.concat(markerlats);
          var lngList = lngList.concat(markerlngs);
        }
        var minLat = Math.min(...latList);
        var maxLat = Math.max(...latList);
        var minLng = Math.min(...lngList);
        var maxLng = Math.max(...lngList);
        return new L.DivIcon({ 
          html: `<div role='img' aria-label='facility cluster marker'>` + cluster.getChildCount() + `<span>&nbsp facilites with bounding box: [`+minLng+`,`+minLat+`,`+maxLng+`,`+maxLat+`]</span></div>`,
         className: 'marker-cluster'
        });
      }"),
                   showCoverageOnHover=FALSE,
                   removeOutsideVisibleBounds=TRUE))
    if(currentPrograms$programCode[currentPrograms$programShorthandDescription == input$programSelection] == 'ARP'){
      map %>%
        hideGroup('MATS') %>%
        showGroup('ARP')
    }
    else if(currentPrograms$programCode[currentPrograms$programShorthandDescription == input$programSelection] == 'MATS'){
      map %>%
        hideGroup('ARP') %>%
        showGroup('MATS')
    }
    else {
      map %>% 
        hideGroup('ARP') %>%
        hideGroup('MATS') %>%
        addPolygons(data = shapeFileData,
                    layerId = ~shapeFileData[['stateName']], 
                    color = 'green',
                    fillColor = 'green',
                    opacity = 1.0) 
    }
    map %>% 
      #setView(lng = -93.85, lat = 37, zoom = 4)
      fitBounds(lng1 = min(na.omit(markerData$longitude))-1.5, 
                lat1 = min(na.omit(markerData$latitude))-1.5,
                lng2 = max(na.omit(markerData$longitude))+1.5,
                lat2 = max(na.omit(markerData$latitude))+1.5)
  }
  
  # Update map with cleared filter selections
  update_full_map <- function(markerData){
    leafletProxy("map",data=markerData) %>% 
      clearMarkerClusters() %>% clearPopups() %>% clearMarkers() %>% 
      removeShape(unique(stateSf$stateName)) %>% 
      removeShape(unique(countyStateSf$countyns)) %>% 
      hideGroup('ARP') %>%
      hideGroup('MATS') %>%
      addMarkers(data = markerData,
                 lng = ~longitude, lat = ~latitude, 
                 options = markerOptions(alt=~htmlEscape(facilityName), 
                                         `aria-label`="facility marker", 
                                         lat=~latitude,
                                         lng=~longitude), 
                 layerId = ~facilityId,
                 label = ~facilityName,
                 clusterOptions = markerClusterOptions(
                   iconCreateFunction = JS("function (cluster) {    
        var markers = cluster.getAllChildMarkers();
        var latList = []; 
        var lngList = []; 
        for (i = 0; i < markers.length; i++) {
          var markerlats = [markers[i].options.lat];
          var markerlngs = [markers[i].options.lng];
          var latList = latList.concat(markerlats);
          var lngList = lngList.concat(markerlngs);
        }
        var minLat = Math.min(...latList);
        var maxLat = Math.max(...latList);
        var minLng = Math.min(...lngList);
        var maxLng = Math.max(...lngList);
        return new L.DivIcon({ 
          html: `<div role='img' aria-label='facility cluster marker'>` + cluster.getChildCount() + `<span>&nbsp facilites with bounding box: [`+minLng+`,`+minLat+`,`+maxLng+`,`+maxLat+`]</span></div>`,
         className: 'marker-cluster'
        });
      }"),
                   showCoverageOnHover=FALSE,
                   removeOutsideVisibleBounds=TRUE))%>%
      #setView(lng = -93.85, lat = 37, zoom = 4)
      fitBounds(lng1 = min(na.omit(markerData$longitude))-1.5, 
                lat1 = min(na.omit(markerData$latitude))-1.5,
                lng2 = max(na.omit(markerData$longitude))+1.5,
                lat2 = max(na.omit(markerData$latitude))+1.5)
  }
  
  update_map_with_shape_maintained <- function(markerData){
    leafletProxy("map",data=markerData) %>% 
      clearMarkerClusters() %>% clearPopups() %>% clearMarkers() %>% 
      addMarkers(data = markerData,
                                lng = ~longitude, lat = ~latitude, 
                                options = markerOptions(alt=~htmlEscape(facilityName), 
                                                        `aria-label`="facility marker", 
                                                        lat=~latitude,
                                                        lng=~longitude), 
                                layerId = ~facilityId,
                                label = ~facilityName,
                                clusterOptions = markerClusterOptions(
                                  iconCreateFunction = JS("function (cluster) {    
        var markers = cluster.getAllChildMarkers();
        var latList = []; 
        var lngList = []; 
        for (i = 0; i < markers.length; i++) {
          var markerlats = [markers[i].options.lat];
          var markerlngs = [markers[i].options.lng];
          var latList = latList.concat(markerlats);
          var lngList = lngList.concat(markerlngs);
        }
        var minLat = Math.min(...latList);
        var maxLat = Math.max(...latList);
        var minLng = Math.min(...lngList);
        var maxLng = Math.max(...lngList);
        return new L.DivIcon({ 
          html: `<div role='img' aria-label='facility cluster marker'>` + cluster.getChildCount() + `<span>&nbsp facilites with bounding box: [`+minLng+`,`+minLat+`,`+maxLng+`,`+maxLat+`]</span></div>`,
         className: 'marker-cluster'
        });
      }"),
                                  showCoverageOnHover=FALSE,
                                  removeOutsideVisibleBounds=TRUE))
  }
  
  # When map is clicked, show a popup with facility info
  observeEvent(input$map_marker_click,{
    event <- input$map_marker_click
    if (is.null(event)){
      return()
    }
    isolate({
      showFacInfoPopup(event$id, event$lat, event$lng)
    })
    output$fac_summary_text <- renderUI({
      tryCatch({
        get_facility_info_for_side_panel(event$id)
      },
      error = function(e) {
        debugging('get_facility_info_for_side_panel - %s', e)
        return(
          tagList(
            p(class="shiny-output-error","Something went wrong... Try again or reach out to ",
              tags$a(class="usa-link" ,
                     href="mailto:campd-support@camdsupport.com?subject=CAMPD Visualization Gallery - Facility Map" ,
                     target="_blank", rel="noopener noreferrer",
                     `aria-label`="Contact Support for Facility Map",
                     "campd-support@camdsupport.com"),
              " if you continue to encounter this error.")
          )
        )
      })
    })
    output$acct_summary_text <- renderUI({
      tryCatch({
        get_compliance_info_for_side_panel(event$id)
      },
      error = function(e) {
        debugging('get_compliance_info_for_side_panel - %s', e)
        return(
          tagList(
            p(class="shiny-output-error","Something went wrong... Try again or reach out to ",
              tags$a(class="usa-link" ,
                     href="mailto:campd-support@camdsupport.com?subject=CAMPD Visualization Gallery - Facility Map" ,
                     target="_blank", rel="noopener noreferrer",
                     `aria-label`="Contact Support for Facility Map",
                     "campd-support@camdsupport.com"),
              " if you continue to encounter this error.")
          )
        )
      })
    })
  })
  
  # Show a popup at the given location
  showFacInfoPopup <- function(fac_id, lat, lng) {
    # Pick first row of unit data
    selectedFac <- unique(unitData[unitData$facilityId == fac_id,c("facilityName","county","stateCode","stateName")])
    content <- as.character(
      tagList(
        div(class="font-sans-xs line-height-sans-5",
        tags$h4(class="font-sans-md text-bold",selectedFac$facilityName),
        HTML(sprintf("%s, %s",
                                 selectedFac$county, 
                                 as.character(selectedFac$stateCode)
        )), tags$br(),
        sprintf("Latitude: %s", as.character(lat)), tags$br(),
        sprintf("Longitude: %s", as.character(lng))
      )))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = fac_id)
  }
  
  # collect and output facility information for display
  get_facility_info_for_side_panel <- function(facilityId){
    
    selectedUnitFac <- unitData[unitData["facilityId"] == facilityId,]
    selectedFac <- unique(selectedUnitFac[,c("facilityName","county","stateCode","stateName")])
    fuelTypesStg <- paste0(unique(selectedUnitFac$primaryFuelInfo),collapse = ", ")
    operatingStatuses <- paste0(lapply(sort(selectedUnitFac$unitId),function(unit){
      paste0("<strong>",unit,"</strong>",": ",selectedUnitFac$operatingStatus[selectedUnitFac$unitId == unit])
    }),collapse = "<br/>")
    
    
    if (length(setdiff(selectedUnitFac$primaryFuelInfo,c("Not reported"))) > 0){
      fuelTypesStg <- paste(unlist(unique(selectedUnitFac$primaryFuelInfo)),collapse = ", ")
    }
    else{fuelTypesStg <- "Not reported"}
    
    if (length(intersect(selectedUnitFac$so2ControlsInstalled, c("Yes","No")))){
      if ("Yes" %in% selectedUnitFac$so2ControlsInstalled){so2Controls <- "Yes"}
      else{so2Controls <- "No"}
    }
    else{so2Controls <- selectedUnitFac$so2ControlsInstalled[1]}
    if (length(intersect(selectedUnitFac$noxControlsInstalled, c("Yes","No")))){
      if ("Yes" %in% selectedUnitFac$noxControlsInstalled){noxControls <- "Yes"}
      else{noxControls <- "No"}
    }
    else{noxControls <- selectedUnitFac$noxControlsInstalled[1]}
    if (length(intersect(selectedUnitFac$particulateMatterControlsInstalled, c("Yes","No")))){
      if ("Yes" %in% selectedUnitFac$particulateMatterControlsInstalled){pmControls <- "Yes"}
      else{pmControls <- "No"}
    }
    else{pmControls <- selectedUnitFac$particulateMatterControlsInstalled[1]}
    if (length(intersect(selectedUnitFac$mercuryControlsInstalled, c("Yes","No")))){
      if ("Yes" %in% selectedUnitFac$mercuryControlsInstalled){hgControls <- "Yes"}
      else{hgControls <- "No"}
    }
    else{hgControls <- selectedUnitFac$mercuryControlsInstalled[1]}
    
    content <- 
      tagList(
        tags$h4(class="font-sans-lg text-bold", selectedFac$facilityName),
        tags$strong("Facility ID:"),sprintf(" %s", as.character(facilityId)), tags$br(),
        tags$strong("State:"),sprintf(" %s", selectedFac$stateName), tags$br(),
        tags$h5(class="font-sans-md text-bold",tags$u("Primary Fuel Type")),
        sprintf("%s", fuelTypesStg), tags$br(),
        tags$h5(class="font-sans-md text-bold",tags$u("Pollutant Controls")),
        tags$strong("SO2 Controls Installed:"),sprintf("%s", so2Controls), tags$br(),
        tags$strong("NOx Controls Installed:"),sprintf("%s", noxControls), tags$br(),
        tags$strong("Particulate Matter Controls Installed:"),sprintf("%s", pmControls), tags$br(),
        tags$strong("Mercury Controls Installed:"),sprintf("%s", hgControls), tags$br(), 
        div(style="margin-top:5px;","For more information on pollutants, please visit:"),
        tags$a(class="usa-link",href="https://www.epa.gov/criteria-air-pollutants", 
               "https://www.epa.gov/criteria-air-pollutants",
               target="_blank"), tags$br(),
        tags$a(class="usa-link",href="https://www.epa.gov/mercury", 
               "https://www.epa.gov/mercury",
               target="_blank"),
        tags$h5(class="font-sans-md text-bold",tags$u("Unit Operating Statuses:")),
        HTML(operatingStatuses)
      )
  }
  
  get_compliance_info_for_side_panel <- function(facilityId){
    
    selectedFac <- unique(complianceData[complianceData$facilityId == facilityId,])
    selectedFac <- merge(selectedFac, allPrograms[,c("programCode","programShorthandDescription")],all.x=TRUE)
    
    accountNumber <- as.character(selectedFac$accountNumber[1])
    if (is.na(selectedFac$accountNumber[1])){
      accountNumber <- "No CAMD compliance account associated with this facility."
    }
    
    if((nrow(selectedFac)==1) & is.na(selectedFac$accountNumber[1]) & is.na(selectedFac$programCode[1])){
      subjectedPrograms <- tagList(sprintf(" %s", "This facility has no historical compliance record for CAMD's emissions trading programs."),
                                   tags$br())
      complianceDisplayTable <- ""
      outOfComplianceTable <- ""
    }
    
    else{
      latestComplianceFacilityData <- selectedFac[selectedFac$year == latestComplianceYear,]
      subjectedProgramsCodes <- unique(latestComplianceFacilityData$programCode)
      subjectedPrograms <- allPrograms$programShorthandDescription[allPrograms$programCode %in% subjectedProgramsCodes]
      
      if (length(subjectedPrograms) == 0){
        subjectedPrograms <-tagList(sprintf(" %s",  "CAMD's emissions trading programs are not currently applicable to this facility."),
                                    tags$br())
        complianceDisplayTable <- ""
      }
      else{
        subjectedPrograms <- tagList(HTML(getListHTML(subjectedPrograms)))
        
        compTableForLatestYear <- latestComplianceFacilityData[,c("programShorthandDescription","inCompliance.")]
        names(compTableForLatestYear) <- c("Program","In Compliance?")
        
        complianceDisplayTable <- tagList(
          tags$h5(class="font-sans-md text-bold",tags$u(paste0("Compliance for ",latestComplianceYear,":"))),
          HTML(getTableHTML(compTableForLatestYear))
        )
      }
      
      previousComplianceFacilityData <- na.omit(selectedFac[selectedFac$year < latestComplianceYear,])
      
      if(nrow(previousComplianceFacilityData) == 0){
        outOfComplianceTable <- tagList(
          tags$h5(class="font-sans-md text-bold",tags$u("Non-Compliant Years:")),
          HTML("This facility has no record of non-compliance for applicable CAMD emissions trading programs.")
        )
      }
      else{
        
        compYearsOutOfComp <- previousComplianceFacilityData[,c("programShorthandDescription","year","inCompliance.")]
        names(compYearsOutOfComp) <- c("Program","Year","In Compliance?")
        
        outOfComplianceTable <- tagList(
          tags$h5(class="font-sans-md text-bold",tags$u("Non-Compliant Years:")),
          HTML(getTableHTML(compYearsOutOfComp)),
          tags$p("This facility has no other record of non-compliance for applicable CAMD emissions trading programs.")
        )
      }
    }
    
    content <- 
      tagList(
        tags$h4(class="font-sans-lg text-bold",selectedFac$facilityName[1]),
        tags$strong("Account Number:"),sprintf(" %s", accountNumber),
        tags$h5(class="font-sans-md text-bold",tags$u("Applicable Programs:")),
        subjectedPrograms,
        complianceDisplayTable,
        outOfComplianceTable
      )
    
    content
    
  }
  
}
if (!interactive()) sink(stderr(), type = "output")

shiny::runApp(shinyApp(ui,server))
# switch to below while deploying
#shiny::runApp(shinyApp(ui,server), host="0.0.0.0", port=strtoi(Sys.getenv("PORT")))

