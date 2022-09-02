# App

# clear quotes below while deploying

'library_path <- paste("Library Path: ", Sys.getenv(c("LD_LIBRARY_PATH")))
print(paste("LD_LIBRARY_PATH: ", library_path))

lib_dir <- "/home/vcap/deps/0/r/lib"
local_lib_dir <- "lib"

if(dir.exists(lib_dir))
{
  # Get the list of libs
  lib_tars <- list.files(local_lib_dir)
  lib_tars <- paste(local_lib_dir, lib_tars, sep="/")
  
  print(paste("Local libs: ", lib_tars))
  print(paste("Working directory: ", list.files(getwd())))
  
  # Copy the files to the lib_dir
  for(i in 1:length(lib_tars)) {untar(lib_tars[i], exdir = lib_dir)}
  
  Sys.setenv(PROJ_LIB=lib_dir)
}

print(list.files(lib_dir))'

library(dotenv)
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

install.packages(paste0(getwd(),'/tmp/EPAheaderwidget_0.0.2.tar.gz'), repos=NULL)

library(EPAheaderwidget)

load_dot_env(".env")

# src scripts
source("./src/API-calls.R")
source("./src/static.R")
source("./src/reactive.R")
source("./src/functions.R")

# modules
source("./modules/search.R")
source("./modules/display-table.R")

ui <- div(
  
  useShinydashboard(),
          
  epaSlimNavInput("epaNav",'dev'),
  div(class="banner",
      #p("content"),
      div(class="banner-conents",
          "Facility Map"),
  ),
  
    div(
    tags$head(
      useShinyjs(),
      #tags$link(rel="shortcut icon", href=paste0(gitRawBase,"/www/favicon.ico")),
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
    
    #########################
    
    div(id="facility-map-content",
        
        
        
        #h1("Welcome to the Facility Map!"),
        h2("Getting Started"),
        p(class="intro-text","This facility map uses data collected as part of 
      EPA's emissions trading programs. Interested in exploring the data 
        further, check out ",
          tags$a(href="https://campd.epa.gov/data", 
                 "CAMPD's data section",
                 target="_blank"),
          "!"),
        p(class="intro-text","Click on a ",
          tags$img(src='https://unpkg.com/leaflet@1.3.1/dist/images/marker-icon-2x.png', 
                   width='20px',height='35px',alt = "facility marker"),
          " on the map to view a facility's attribute and 
        program compliance information using the map's side panel ",
          '"Facility Summary" and "Compliance Summary" expandable boxes.'),
        div(class="bullet-title","Using the Map"),
        tags$ul(class="intro-list",
                tags$li("Search and filter options dynamically update the map view 
                according to inputted selections."),
                tags$li('Use the state and county search to find facilities near you.'),
                tags$li(class="indent",'Selecting a state first will refine the county 
                search to only counties within the selected state.'),
                tags$li(class="indent",'Note that the county search box is grouped by state.'),
                tags$li('Use the facility filter to narrow down facilities by trading program.'),
                tags$li('Use the clear buttons to clear filters or searches.'),
                tags$li(paste0('Facility Summary displays basic facility/unit attribute information 
                for the ',as.character(latestComplianceYear),' operating year.')),
                tags$li(paste0('Compliance Summary displays compliance information for 
                programs applicable to the chosen facility for ',
                               as.character(latestComplianceYear),' and 
                any previous years the facility was non-compliant.')),
        ),
        p(class="intro-text","Use the download buttons below to save the data available in the map."),
        div(class="btn-group",
            div(downloadButton("download_facility_data","Download Facility Data CSV")),
            div(downloadButton("download_compliance_data","Download Compliance Data CSV"))),
        p(class="intro-text","For more information on these programs visit",
          tags$a(href="https://www.epa.gov/airmarkets/programs", 
                 "EPA's Clean Air Markets Programs web area",
                 target="_blank"),
          "."),
        tags$hr(),
        
        fluidRow(id="filtersearch-container",
                 column(8, 
                        div(class='column-decorator', div(h3(bsButton("search-tooltip", label = "Search Tooltip", 
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
                                              label='State Name',
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
                            
                            div(class="select-clear", actionButton("clearSearch", "Clear Search"))
                            
                        )
                 ),
                 column(4, 
                        div(class="column-decorator", div(h3(bsButton("filter-tooltip", label = "Filter Tooltip", 
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
                                                              label=facilityMapLabelConversion$label[facilityMapLabelConversion$columnName == "programCode"],
                                                              choices=c("Select All",unique(na.omit(currentPrograms$programCode))),
                                                              selected=c("Select All"),
                                                              multiple = FALSE))),
                            div(class="select-clear", actionButton("clearFilters", "Clear Filters"))
                        )
                        
                 )
        ),
    ),
    div(class="maparea",
      absolutePanel(id = "facility-map-panel", 
                      class = "panel panel-default",
                      draggable = TRUE,
                      div(id = "facility-summary-box", 
                          box(uiOutput("fac_summary_text"),
                              title = "Facility Summary",  
                              collapsible = TRUE, 
                              status = "primary",
                              width = 12)
                      ),
                      div(id = "compliance-summary-box", 
                          box(uiOutput("acct_summary_text"), 
                              title = "Compliance Summary",  
                              collapsible = TRUE, 
                              status = "primary",
                              width = 12)
                      )
    ),
    leafletOutput("map",width="100%", height = "500px")
        
    ),
    epaSlimFootInput("epaFoot", appVersion = "v0.0.0", appPublished = "local")
    
    #facilityMapAppUI("page"),
    
    #########################
    
  )
)

server <- function(input, output, session) {
  
  load_global_vars()
  
  
  
  output$download_facility_data <- downloadHandler(
    filename =  function() { paste0("facility-data-",as.character(latestComplianceYear),".csv") },
    content = function(file) {
      write.csv(load_facility_download_file(), file, row.names = FALSE)
    }
  )
  
  output$download_compliance_data <- downloadHandler(
    filename =  function() { paste0("compliance-data.csv") },
    content = function(file) {
      write.csv(load_compliance_download_file(), file, row.names = FALSE)
    }
  )
  
  # State and county searches
  stateFilterVal <- reactiveVal({NULL})
  countyFilterVal <- reactiveVal({NULL})
  markerData <- reactiveVal({programFacilityData})
  
  stateSearch <- callModule(searchServer,"stateSearchInput",
                            stateSf,reactive(c(input$clearSearch)),
                            filterBy='stateName',
                            filterVal=reactive(c()))
  coutSearch <- callModule(searchServer,"countySearchInput",
                           countyStateSf,reactive(c(input$clearSearch)),
                           filterBy='stateName',
                           filterVal=stateFilterVal)
  
  observeEvent(stateSearch(),{
    if (nrow(stateSearch()) != 0){
      # State search selected is filled to pass to county search in order to clear the
      # county search
      stateFilterVal(NULL)
      stateFilterVal(stateSearch()$stateName)
      # display county outline and zoom to location
      update_map_search(markerData(),stateSf,stateSearch(),'stateName','stateOutline')
    }
  })
  observeEvent(coutSearch(),{
    if (nrow(coutSearch()) != 0){
      # County search selected is filled to pass to state search in order to clear the
      # state search
      countyFilterVal(NULL)
      countyFilterVal(coutSearch()$stateName)
      # display county outline and zoom to location
      update_map_search(markerData(),countyStateSf,coutSearch(),'countyns','countyOutline')
    }
  },ignoreInit = TRUE)
  # If something is selected in one of the searches, the map is cleared and
  # re-rendered, otherwise nothing happens
  observeEvent(input$clearSearch,{
    if (length(input$programSelection) != 0){
      if (input$programSelection == "Select All"){
        update_full_map(markerData())
      }
      else{
        shapeFileData <- stateSf[stateSf$stateName %in% unique(markerData()$stateName),]
        
        update_map_filter_selections(markerData(),shapeFileData)
      }
    }
  },ignoreInit = TRUE)
  
  # If program filter changes, and has a value, searches are cleared 
  # map is rendered to the facilities in the filter selections
  observeEvent(input$programSelection,{
    if (length(input$programSelection) != 0){
      if (input$programSelection != "Select All"){
        markerData(filter_facility_latlong_data(programFacilityData[programFacilityData$programCode == input$programSelection,]))
        
        if ((nrow(stateSearch()) != 0) | (nrow(coutSearch()) != 0)){
          update_map_with_shape_maintained(markerData())
        }
        else{
          shapeFileData <- stateSf[stateSf$stateName %in% unique(markerData()$stateName),]
          
          update_map_filter_selections(markerData(),shapeFileData)
        }
      }
      else{
        markerData(filter_facility_latlong_data(programFacilityData))
        if ((nrow(stateSearch()) != 0) | (nrow(coutSearch()) != 0)){
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
    if (length(input$programSelection) != 0){
      if (input$programSelection != "Select All"){
        markerData(filter_facility_latlong_data(programFacilityData))
        if ((nrow(stateSearch()) != 0) | (nrow(coutSearch()) != 0)){
          update_map_with_shape_maintained(markerData())
        }
        else{
          update_full_map(markerData())
        }
        updateSelectizeInput(
          session = session,
          inputId = "programSelection",
          selected = c("Select All"))
        
      }
    }
  },ignoreInit = TRUE)
  
  # When map is clicked, or selection is made, clear popups and side panels
  observeEvent(c(input$map_click,input$programSelection,stateSearch(),coutSearch()),{
    event <- input$map_map_click
    output$fac_summary_text <- renderUI({HTML("")})
    output$acct_summary_text <- renderUI({HTML("")})
    leafletProxy("map") %>% clearPopups()
  },ignoreInit = TRUE)
  
  # Initialize leaflet
  output$map <- renderLeaflet({
    mapData <- filter_facility_latlong_data(programFacilityData)
    legendText <- tagList(div(class="grid-container",
                              div(class="grid-item",
                                  tags$img(src='https://raw.githubusercontent.com/USEPA/campd-maps-graphs/testing/www/cluster.png', 
                                           width='27px',height='27px',alt = "facility cluster marker")),
                              div(class="grid-item",style="text-align: left;",div("indicates the amount of"),
                                  div("facilities in that general area"))),
                          div(class="grid-container",
                              div(class="grid-item",
                                  tags$img(src='https://unpkg.com/leaflet@1.3.1/dist/images/marker-icon-2x.png',
                                           width='20px',height='35px',alt = "facility marker",style="margin-left:3px;margin-right:3px;")),
                              div(class="grid-item",style="text-align: left;margin: 0;",div("indicates one facility"))))
    leaflet() %>%
      addTiles() %>% 
      fitBounds(lng1 = min(na.omit(mapData$longitude))-1.5, 
                lat1 = min(na.omit(mapData$latitude))-1.5,
                lng2 = max(na.omit(mapData$longitude))+1.5,
                lat2 = max(na.omit(mapData$latitude))+1.5) %>%
      addControl(legendText, position = "bottomleft" )
  })
  
  # min Longitude , min Latitude , max Longitude , max Latitude 
  # "boundingbox":["51.2867602","51.6918741","-0.5103751","0.3340155"]
  # Update map and include shape file outline that was selected in the search
  update_map_search <- function(markerData,shapeFileData,searchRow,layer,group){
    bbox <- st_bbox(shapeFileData[searchRow,]) %>% 
      as.vector()
    leafletProxy("map",data=shapeFileData) %>% 
      clearMarkerClusters() %>%
      clearMarkers() %>%
      clearShapes() %>% 
      addTiles() %>% 
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
      
      addPolygons(data = searchRow,
                  layerId = ~searchRow[[layer]], 
                  group = group,
                  color = 'green',
                  fillColor = 'green') %>% 
      fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
  }
  
  # Update map with filter selections - states outlined
  update_map_filter_selections <- function(markerData,shapeFileData){
    leafletProxy("map",data=shapeFileData) %>% 
      clearMarkerClusters() %>% clearPopups() %>%
      clearMarkers() %>% 
      clearShapes() %>% 
      addTiles() %>% addMarkers(data = markerData,
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
      addPolygons(data = shapeFileData,
                  layerId = ~shapeFileData[['stateName']], 
                  group = 'stateOutline',
                  color = 'green',
                  fillColor = 'green') %>% 
      #setView(lng = -93.85, lat = 37, zoom = 4)
      fitBounds(lng1 = min(na.omit(markerData$longitude))-1.5, 
                lat1 = min(na.omit(markerData$latitude))-1.5,
                lng2 = max(na.omit(markerData$longitude))+1.5,
                lat2 = max(na.omit(markerData$latitude))+1.5)
  }
  
  # Update map with cleared filter selections
  update_full_map <- function(markerData){
    leafletProxy("map",data=markerData) %>% 
      clearMarkerClusters() %>% clearPopups() %>%
      clearMarkers() %>% 
      clearShapes() %>% 
      addTiles() %>% addMarkers(data = markerData,
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
      clearMarkerClusters() %>% clearPopups() %>%
      clearMarkers() %>% 
      addTiles() %>% addMarkers(data = markerData,
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
      get_facility_info_for_side_panel(event$id)
    })
    output$acct_summary_text <- renderUI({
      get_compliance_info_for_side_panel(event$id)
    })
  })
  
  # Show a popup at the given location
  showFacInfoPopup <- function(fac_id, lat, lng) {
    # Pick first row of unit data
    selectedFac <- unique(programFacilityData[programFacilityData$facilityId == fac_id,c("facilityName","county","stateCode","stateName")])
    content <- as.character(
      tagList(
        tags$h4(selectedFac$facilityName),
        tags$strong(HTML(sprintf("%s, %s",
                                 selectedFac$county, 
                                 as.character(selectedFac$stateCode)
        ))), tags$br(),
        sprintf("Latitude: %s", as.character(lat)), tags$br(),
        sprintf("Longitude: %s", as.character(lng))
      ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = fac_id)
  }
  
  # collect and output facility information for display
  get_facility_info_for_side_panel <- function(facilityId){
    
    selectedUnitFac <- unitData[unitData$facilityId == facilityId,]
    selectedFac <- unique(programFacilityData[programFacilityData$facilityId == facilityId,c("facilityName","county","stateCode","stateName")])
    fuelTypesStg <- paste0(unique(unitData$primaryFuelInfo[unitData$facilityId == facilityId]),collapse = ", ")
    operatingStatuses <- paste0(lapply(sort(selectedUnitFac$unitId),function(unit){
      paste0("<strong>",unit,"</strong>",": ",selectedUnitFac$operatingStatus[selectedUnitFac$unitId == unit])
    }),collapse = "<br/>")
    
    if(length(na.omit(unitData$so2ControlInfo[unitData$facilityId == facilityId])) != 0){
      so2Controls <- "Yes"
    }
    else{so2Controls <- "No"}
    if(length(na.omit(unitData$noxControlInfo[unitData$facilityId == facilityId])) != 0){
      noxControls <- "Yes"
    }
    else{noxControls <- "No"}
    if(length(na.omit(unitData$pmControlInfo[unitData$facilityId == facilityId])) != 0){
      pmControls <- "Yes"
    }
    else{pmControls <- "No"}
    if(length(na.omit(unitData$hgControlInfo[unitData$facilityId == facilityId])) != 0){
      hgControls <- "Yes"
    }
    else{hgControls <- "No"}
    
    content <- 
      tagList(
        tags$h4(selectedFac$facilityName),
        tags$strong("Facility ID:"),sprintf(" %s", as.character(facilityId)), tags$br(),
        tags$strong("State:"),sprintf(" %s", selectedFac$stateName), tags$br(),
        tags$h5(tags$u("Primary Fuel Type")),
        sprintf("%s", fuelTypesStg), tags$br(),
        tags$h5(tags$u("Pollutant Controls")),
        tags$strong("SO2 Controls Installed:"),sprintf("%s", so2Controls), tags$br(),
        tags$strong("NOx Controls Installed:"),sprintf("%s", noxControls), tags$br(),
        tags$strong("Particulate Matter Controls Installed:"),sprintf("%s", pmControls), tags$br(),
        tags$strong("Mercury Controls Installed:"),sprintf("%s", hgControls), tags$br(), 
        div(style="margin-top:5px;","For more information on pollutants, please visit:"),
        tags$a(href="https://www.epa.gov/criteria-air-pollutants", 
               "https://www.epa.gov/criteria-air-pollutants",
               target="_blank"),
        tags$a(href="https://www.epa.gov/mercury", 
               "https://www.epa.gov/mercury",
               target="_blank"),
        tags$h5(tags$u("Unit Operating Statuses:")),
        HTML(operatingStatuses)
      )
  }
  
  get_compliance_info_for_side_panel <- function(facilityId){
    
    selectedFac <- unique(programFacilityData[programFacilityData$facilityId == facilityId,c("facilityName","county","stateCode","stateName")])
    
    complianceYears <- as.list(unique(na.omit(unlist(programInfo$allCompliancePrograms$complianceYears))))
    allYearComplianceFacilityData <- get_allow_comp_data(complianceYears,facilities=c(facilityId))
    
    accountInfo <- get_account_info_data(facilities=c(facilityId))
    
    accountNumber <- as.character(accountInfo$accountNumber[accountInfo$accountNumber %like% 'FACLTY'][1])
    if (length(accountNumber) == 0){
      accountNumber <- "No compliance account associated with this facility."
    }
    
    if(length(allYearComplianceFacilityData)==0){
      subjectedPrograms <- "This facility does not have a history of being subjected to CAMD's emissions trading programs."
      complianceDisplayTable <- ""
      outOfComplianceTable <- ""
    }
    
    else{
      latestComplianceFacilityData <- allYearComplianceFacilityData[allYearComplianceFacilityData$year == latestComplianceYear,]
      subjectedPrograms <- paste0(unique(latestComplianceFacilityData$programCodeInfo),collapse = ", ")
      if (subjectedPrograms == ""){
        subjectedPrograms <- "This facility is currently not subjected to CAMD's emissions trading programs."
        complianceDisplayTable <- ""
      }
      else{
        compTableForLatestYear <- bind_rows(lapply(latestComplianceFacilityData$programCodeInfo, function(prg){
          if (is.na(latestComplianceFacilityData$excessEmissions[latestComplianceFacilityData$programCodeInfo == prg])){
            compStr <- "Yes"
          }
          else{compStr <- "No"}
          c("Program"=prg, "In compliance?"=compStr)
        }))
        
        complianceDisplayTable <- tagList(
          tags$h5(tags$u(paste0("Compliance for ",latestComplianceYear,":"))),
          HTML(getHTML(compTableForLatestYear))
        )
      }
      
      previousComplianceFacilityData <- allYearComplianceFacilityData[allYearComplianceFacilityData$year < latestComplianceYear,]
      
      if(length(na.omit(previousComplianceFacilityData$excessEmissions)) == 0){
        outOfComplianceTable <- tagList(
          tags$h5(tags$u("Non-Compliant Years:")),
          HTML("This facility has no record of non-compliance.")
        )
      }
      else{
        compYearsOutOfComp <- bind_rows(lapply(1:nrow(previousComplianceFacilityData), function(row){
          if (!is.na(previousComplianceFacilityData[row,"excessEmissions"])){
            c("Program"=previousComplianceFacilityData$programCodeInfo[row], 
              "Year"=previousComplianceFacilityData$year[row],
              "In compliance?"="No")
          }
        }))
        outOfComplianceTable <- tagList(
          tags$h5(tags$u("Non-Compliant Years:")),
          HTML(getHTML(compYearsOutOfComp)),
          tags$p("This facility has no other record of non-compliance.")
        )
      }
    }
    
    content <- 
      tagList(
        tags$h4(selectedFac$facilityName),
        tags$strong("Account Number:"),sprintf(" %s", accountNumber),
        tags$h5(tags$u("Subjected Programs:")),sprintf(" %s", subjectedPrograms), tags$br(),
        complianceDisplayTable,
        outOfComplianceTable
      )
    
    content
    
  }
  
}

shiny::runApp(shinyApp(ui,server))
# switch to below while deploying
#shiny::runApp(shinyApp(ui,server), host="0.0.0.0", port=strtoi(Sys.getenv("PORT")))

