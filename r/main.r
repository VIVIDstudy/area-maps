library(data.table)
source("r/downloadArcGISGeoPackage.r")
source("r/downloadExtractZipFile.r")
source("r/downloadSourceData.r")
source("r/getUserAgent.r")
source("r/prepareGeoData.r")
source("r/prepareInteractiveData.r")

source("r/getNhsCatchmentAreas.r")
source("r/getCatchmentAreaGeoms.r")

source("r/calcCatchmentPopulations.r")
source("r/plotSites.r")


downloadSourceData()
prepareInteractiveData(sites_csv = "data-raw/sites.csv")
calcCatchmentPopulations(sites_csv = "data-raw/sites.csv")

geom_data <- prepareGeoData(sites_csv = "data-raw/sites.csv")


plotSites(sites_csv = "data-raw/sites.csv",
          geom_data,
          img_name = "img/sites_map",
          img_type = "png",
          img_width = 8.5,
          img_height = 8.5,
          img_dim_units = "cm")
