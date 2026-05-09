library(data.table)
source("r/getNhsCatchmentAreas.r")

vivid_catchment_areas <- getNhsCatchmentAreas("data-raw/sites.csv")

setorder(vivid_catchment_areas, site_name, ods_name)
fwrite(vivid_catchment_areas[, .(`Laboratory network` = site_name,
                                 `NHS England Organisation Data Service (ODS) name` = ods_name,
                                 `Census 2011 MSOA code` = msoa11)],
       file = "data-out/vivid_included_msoas.csv")
