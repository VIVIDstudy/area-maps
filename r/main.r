library(data.table)
source("R/downloadArcGISGeoPackage.r")
source("R/downloadExtractZipFile.r")
source("R/downloadSourceData.r")
source("R/getUserAgent.r")
source("R/prepareGeoData.r")


downloadSourceData()
prepareGeoData(sites_csv = "data-raw/sites.csv")

plotSites(sites_csv = "data-raw/sites.csv",
          data_directory = "data",
          img_name = "img/sites_map",
          img_type = "png",
          img_width = 8.5,
          img_height = 10.2,
          img_dim_units = "cm")
