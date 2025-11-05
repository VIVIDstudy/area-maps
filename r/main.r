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


downloadSourceData()
prepareGeoData(sites_csv = "data-raw/sites.csv")
prepareInteractiveData(sites_csv = "data-raw/sites.csv")
calcCatchmentPopulations(sites_csv = "data-raw/sites.csv")

plotSites(sites_csv = "data-raw/sites.csv",
          data_directory = "data",
          img_name = "img/sites_map",
          img_type = "png",
          img_width = 8.5,
          img_height = 10.2,
          img_dim_units = "cm")
