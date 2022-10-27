# Clean Air Market Program Data (CAMPD) Viz Gallery Facility Map
An R Shiny App found at (url). This app uses lealfet to display a map of facilities that exist in the Clean Air Market Division (CAMD) database. It also provides users based data on the facility attributes and compliance performance.

## Getting started

### Running local
After cloning the repo, run
``` r
shiny::runApp()
```

### Deploying to the cloud

### 1) Vendor r packages
The r packages that are installed for this app are found at [cflinuxfs3 CRAN](https://github.com/USEPA/cflinuxfs3-CRAN). Clone this repo and copy everything from [/src/contrib](https://github.com/USEPA/cflinuxfs3-CRAN/tree/master/cflinuxfs3/src/contrib) into /vendor_r/src/contrib in this directory.

### 2) Prep for pushing to the cloud

- Add an enviroment variable for the application named APP_VERSION. This is what will show up in the bottom bar of the page.
- Uncomment/switch out these lines in their respective locations
``` r
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
```
``` r
# get application environment variables
APPLICATION_ENV_VARIABLES <- fromJSON(Sys.getenv(c("VCAP_APPLICATION")))
```
``` r
epaSlimHeader("epaNav",APPLICATION_ENV_VARIABLES$space_name),
#epaSlimHeader("epaNav","local"),
```
``` r
epaSlimFooter("epaFoot", appVersion = Sys.getenv(c("APP_VERSION")), appPublished = format(Sys.Date(),"%a %b %d %Y"))
#epaSlimFooter("epaFoot", appVersion = "v1.0.0", appPublished = format(Sys.Date(),"%a %b %d %Y"))
```
``` r
#shiny::runApp(shinyApp(ui,server))
# switch to below while deploying
shiny::runApp(shinyApp(ui,server), host="0.0.0.0", port=strtoi(Sys.getenv("PORT")))
```

### 3) Push the App
In your terminal `cf push APPNAME`

Find Cloud Foundry's [doc page](https://docs.cloudfoundry.org/) for more info.

## Other information

### Data Source
This map pulls data hosted on [CAMPD R Shiny Data Source](https://github.com/USEPA/campdRShinyDataSource) which is updated nightly using the [CAM APIs](https://www.epa.gov/airmarkets/cam-api-portal).

### R Shiny One EPA Template
This [EPA R Shiny Template](https://github.com/USEPA/epaRShinyTemplate) uses the react components from the [EASEY Design System](https://github.com/US-EPA-CAMD/easey-design-system).

### USWDS Styling
By using the EPA R Shiny Template, you also incorporate USWDS stying. Find documentation in the [Utilities](https://designsystem.digital.gov/utilities/) section.

### Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.
