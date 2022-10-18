# App

# clear quotes below while deploying

## CHANGE ALL URLS TO CORRECT REPO 
# cluster pngs

library_path <- paste("Library Path: ", Sys.getenv(c("LD_LIBRARY_PATH")))
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

print(list.files(lib_dir))

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
library(curl)

install.packages(paste0(getwd(),'/tmp/epaRShinyTemplate_0.0.1.tar.gz'), repos=NULL)

library(epaRShinyTemplate)

load_dot_env(".env")

# src scripts
source("./src/API-calls.R")
source("./src/static.R")
source("./src/reactive.R")
source("./src/functions.R")

# modules
source("./modules/search.R")
source("./modules/display-table.R")
source("./modules/display-list.R")

# enabling js function after modal popup (download interuption)
enableElementsJs <- "shinyjs.enableElements = function() {
    const keyboardfocusableElements = document.querySelectorAll(
      'a, button, input, select'
    );
    
    if ($('#map').length) {
      const map = document.getElementById('map');
      map.setAttribute('tabindex', 0);
      const mapElements = document.getElementById('map').getElementsByClassName('leaflet-marker-pane')[0];
      if (typeof mapElements !== 'undefined') {
        const mapfocusableElements = mapElements.querySelectorAll(
          'div, img'
        );
        for (let i=0; i < mapfocusableElements.length; i++){
          mapfocusableElements[i].setAttribute('tabindex', 0);
        }
      }
    }
    
    for (let i=0; i < keyboardfocusableElements.length; i++){
      
      if(keyboardfocusableElements[i].tagName == 'A') {
        keyboardfocusableElements[i].removeAttribute('tabindex');
      }
      else {
        keyboardfocusableElements[i].disabled = false;
      }
    }
  }
  shinyjs.focusOnFacilityDownload = function() {
    document.getElementById('init_facility_data').focus();
  }
  shinyjs.focusOnComplianceDownload = function() {
    document.getElementById('init_compliance_data').focus();
  }
"

ui <- tags$main(
  
  # load in shiny dashboard - use this first so epaSlimHeader can override some styling
  useShinydashboard(),
  
  # easey design template
  epaSlimHeader("epaNav",'performance'),
  
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
      extendShinyjs(text = enableElementsJs, functions = c("enableElements","focusOnFacilityDownload",
                                                           "focusOnComplianceDownload")),
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
            p("Use the download buttons below to save the data available in the map."),
            div(class="grid-container download-buttons",
                div(class="grid-row",
                    div(class="grid-col-auto",
                        downloadButton(class="usa-button","download_facility_data","Download Facility Data CSV"),
                        actionButton(class="usa-button","init_compliance_data", "Download Compliance Data CSV", icon = icon("download")),
                        downloadButton(style = "visibility: hidden;","download_compliance_data",""))
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
                            
                            div(class="select-clear", actionButton(class="usa-button","init_clearSearch","Clear Search")),
                            div(actionButton(style = "visibility: hidden;","clearSearch",""))
                            
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
                                                              label=facilityMapLabelConversion$label[facilityMapLabelConversion$columnName == "programCode"],
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
            epaSlimFooter("epaFoot", appVersion = "v0.0.0", appPublished = "local")
        )
    )
    
    #################################
    
  )
)

server <- function(input, output, session) {
  
  observe({
    
    load_global_vars()
    load_map_data()
    
    updateSelectizeInput(session, "programSelection",
                         choices=c("Select All",unique(na.omit(programInfo$currentPrograms$programShorthandDescription))))
    
    output$gettingStartedText <- renderUI({ 
      div(
        p("This map currently uses ",tags$strong(as.character(latestComplianceYear)),
            " data to show the location of facilities along with basic information on 
            their facility/unit attributes and compliance performance as part of 
            EPA’s emissions trading programs (this does not include non-allowance programs 
            such as Acid Rain Program NOx or state programs such as Regional Greenhouse Gas Initiative).
            More resources on these programs can be found at ",
            tags$a(class="usa-link",href="https://www.epa.gov/airmarkets/programs", 
                   "EPA's Clean Air Markets Programs web area",
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
                        tags$img(src='https://raw.githubusercontent.com/USEPA/campd-viz-gallery-data-files/main/images/cluster.png', 
                                 width='27px',height='27px',alt="facility cluster marker (9)",
                                 style="min-width:27px;"),
                        " icons on the map represent an area where two or more facilities are located. 
                        The number in the middle represents the number of facilities in that general area."),
                tags$li("You can zoom in on the map to reveal the individual facilities, marked by ",
                        tags$img(src='https://unpkg.com/leaflet@1.3.1/dist/images/marker-icon-2x.png', 
                                 width='20px',height='35px',alt = "facility marker"),
                        "."),
                tags$li("Hover your mouse pointer over the icon to see the facility name."),
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
                tags$li('Use the facility filter to narrow down facilities by trading program.'),
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
    
  })
  
  output$download_facility_data <- downloadHandler(
    filename =  function() { paste0("facility-data-",as.character(latestComplianceYear),".csv") },
    content = function(file) {
      #write.csv(facility_download_data(), file, row.names = FALSE)
      GET("https://github.com/USEPA/campd-viz-gallery-data-files/raw/main/data/facility-map/facilityDataTableForDownload.csv", write_disk(file))
    }
  )
  
  compliance_download_data <- reactiveVal(NULL)
  
  observeEvent(input$init_compliance_data, {
    
    tryCatch({
      compliance_download_data(load_compliance_download_file())
    },
    error = function(e) {
      debugging('Compliance data download crash - %s', e)
      
      return(NULL)
    })
    
    
    if (is.null(compliance_download_data())) {
      showModal(
        modalDialog(
          div(class="font-sans-xs text-base-darkest text-ls-1 line-height-sans-5",
              h3(class="font-sans-md text-bold","Download Interrupted"),
              p("The download has been interrupted or failed. Try again or reach out to",tags$a(class="usa-link" ,
                                                                      href="mailto:campd-support@camdsupport.com?subject=CAMPD Visualization Gallery - Facility Map" ,
                                                                      target="_blank", rel="noopener noreferrer",
                                                                      `aria-label`="Contact Support for Facility Map",
                                                                      "campd-support@camdsupport.com"),
                "if you continue to encounter this error."
                )
              
          ),
          footer = tagList(actionButton(class="usa-button modal-close-button","closeCompModal", "OK")),
          easyClose = FALSE
        )
      )
    } else {
      shinyjs::runjs("document.getElementById('download_compliance_data').click();")
    }
  })
  
  observeEvent(input$closeCompModal, {
    removeModal()
    js$enableElements()
    js$focusOnComplianceDownload()
  },ignoreNULL = TRUE)
  
  output$download_compliance_data <- downloadHandler(
    filename =  function() { paste0("compliance-data.csv") },
    content = function(file) {
      write.csv(compliance_download_data(), file, row.names = FALSE)
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
  }, ignoreNULL = TRUE)
  observeEvent(coutSearch(),{
    if (nrow(coutSearch()) != 0){
      # County search selected is filled to pass to state search in order to clear the
      # state search
      countyFilterVal(NULL)
      countyFilterVal(coutSearch()$stateName)
      # display county outline and zoom to location
      update_map_search(markerData(),countyStateSf,coutSearch(),'countyns','countyOutline')
    }
  },ignoreNULL = TRUE)
  # If something is selected in one of the searches, the map is cleared and
  # re-rendered, otherwise nothing happens
  observeEvent(input$init_clearSearch,{
    print(countyFilterVal())
    print(stateFilterVal())
    shinyjs::runjs("document.getElementById('clearSearch').click();")
  })
  
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
        programSelected <- programInfo$currentPrograms$programCode[programInfo$currentPrograms$programShorthandDescription == input$programSelection]
        markerData(filter_facility_latlong_data(programFacilityData[programFacilityData$programCode == programSelected,]))
        
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
    mapData <- filter_facility_latlong_data(programFacilityData)
    legendText <- tagList(div(class="display-table font-sans-xs line-height-sans-2 maxw-mobile padding-x-1",
                              div(class="display-table-row padding-bottom-1",
                                  div(class="display-table-cell padding-right-1 text-middle text-center",
                                      tags$img(src='https://raw.githubusercontent.com/USEPA/campd-viz-gallery-data-files/main/images/cluster.png', 
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
    selectedFac <- unique(programFacilityData[programFacilityData$facilityId == fac_id,c("facilityName","county","stateCode","stateName")])
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
    
    selectedUnitFac <- unitData[unitData$facilityId == facilityId,]
    selectedFac <- unique(programFacilityData[programFacilityData$facilityId == facilityId,c("facilityName","county","stateCode","stateName")])
    fuelTypesStg <- paste0(unique(selectedUnitFac$primaryFuelInfo),collapse = ", ")
    operatingStatuses <- paste0(lapply(sort(selectedUnitFac$unitId),function(unit){
      paste0("<strong>",unit,"</strong>",": ",selectedUnitFac$operatingStatus[selectedUnitFac$unitId == unit])
    }),collapse = "<br/>")
    
    subjectedPrograms <- unlist(lapply(unique(str_split(unique(selectedUnitFac$programCodeInfo),", ")),function(cell){
      unlist(str_split(cell,","))
    }))
    
    if ((length(intersect(subjectedPrograms, programInfo$currentCompliancePrograms$programCode)) == 0) &&
        (is.na(unique(selectedUnitFac$primaryFuelInfo)))){
      fuelTypesStg <- "Not reported"
      so2Controls <- "Not reported"
      noxControls <- "Not reported"
      pmControls <- "Not reported"
      hgControls <- "Not reported"

    }
    
    else {
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
    }
    
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
               target="_blank"),
        tags$a(class="usa-link",href="https://www.epa.gov/mercury", 
               "https://www.epa.gov/mercury",
               target="_blank"),
        tags$h5(class="font-sans-md text-bold",tags$u("Unit Operating Statuses:")),
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
      accountNumber <- "No CAMD compliance account associated with this facility."
    }
    
    if(length(allYearComplianceFacilityData)==0){
      subjectedPrograms <- tagList(sprintf(" %s", "This facility has no historical compliance record for CAMD's emissions trading programs."),
                                   tags$br())
      complianceDisplayTable <- ""
      outOfComplianceTable <- ""
    }
    
    else{
      latestComplianceFacilityData <- allYearComplianceFacilityData[allYearComplianceFacilityData$year == latestComplianceYear,]
      subjectedProgramsCodes <- unique(latestComplianceFacilityData$programCodeInfo)
      subjectedPrograms <- programInfo$allPrograms$programShorthandDescription[programInfo$allPrograms$programCode %in% subjectedProgramsCodes]
      #subjectedPrograms <- paste0(subjectedPrograms,collapse = ", ")
      if (length(subjectedPrograms) == 0){
        subjectedPrograms <-tagList(sprintf(" %s",  "CAMD's emissions trading programs are not currently applicable to this facility."),
                                    tags$br())
        complianceDisplayTable <- ""
      }
      else{
        subjectedPrograms <- tagList(HTML(getListHTML(subjectedPrograms)))
        compTableForLatestYear <- bind_rows(lapply(latestComplianceFacilityData$programCodeInfo, function(prg){
          if (is.na(latestComplianceFacilityData$excessEmissions[latestComplianceFacilityData$programCodeInfo == prg])){
            compStr <- "Yes"
          }
          else{compStr <- "No"}
          programShorthandDescription <- programInfo$allPrograms$programShorthandDescription[programInfo$allPrograms$programCode == prg]
          c("Program"=programShorthandDescription, "In compliance?"=compStr)
        }))
        
        complianceDisplayTable <- tagList(
          tags$h5(class="font-sans-md text-bold",tags$u(paste0("Compliance for ",latestComplianceYear,":"))),
          HTML(getTableHTML(compTableForLatestYear))
        )
      }
      
      previousComplianceFacilityData <- allYearComplianceFacilityData[allYearComplianceFacilityData$year < latestComplianceYear,]
      
      if(length(na.omit(previousComplianceFacilityData$excessEmissions)) == 0){
        outOfComplianceTable <- tagList(
          tags$h5(class="font-sans-md text-bold",tags$u("Non-Compliant Years:")),
          HTML("This facility has no record of non-compliance for applicable CAMD emissions trading programs.")
        )
      }
      else{
        compYearsOutOfComp <- bind_rows(lapply(1:nrow(previousComplianceFacilityData), function(row){
          if (!is.na(previousComplianceFacilityData[row,"excessEmissions"])){
            programShorthandDescription <- programInfo$allPrograms$programShorthandDescription[programInfo$allPrograms$programCode == previousComplianceFacilityData$programCodeInfo[row]]
            c("Program"=programShorthandDescription, 
              "Year"=previousComplianceFacilityData$year[row],
              "In compliance?"="No")
          }
        }))
        outOfComplianceTable <- tagList(
          tags$h5(class="font-sans-md text-bold",tags$u("Non-Compliant Years:")),
          HTML(getTableHTML(compYearsOutOfComp)),
          tags$p("This facility has no other record of non-compliance for applicable CAMD emissions trading programs.")
        )
      }
    }
    
    content <- 
      tagList(
        tags$h4(class="font-sans-lg text-bold",selectedFac$facilityName),
        tags$strong("Account Number:"),sprintf(" %s", accountNumber),
        tags$h5(class="font-sans-md text-bold",tags$u("Applicable Programs:")),
        subjectedPrograms,
        complianceDisplayTable,
        outOfComplianceTable
      )
    
    content
    
  }
  
}

#shiny::runApp(shinyApp(ui,server))
# switch to below while deploying
shiny::runApp(shinyApp(ui,server), host="0.0.0.0", port=strtoi(Sys.getenv("PORT")))

