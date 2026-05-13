#' Title
#'
#' @param sites_csv
#' @param data_directory
#'
#' @return
#' @export
#'
#' @examples
getNhsCatchmentAreas <- function(sites_csv,
                                 data_directory = "data") {

  sites <- data.table::fread(sites_csv,
                             select = c("site_name",
                                        "ods_name",
                                        "ods_code"))

  #   ###########################################################################
  #   Fix issue with OHID excluding NORTH MIDDLESEX UNIVERSITY HOSPITAL NHS TRUST
  sites <- sites[ods_name != "NORTH MIDDLESEX UNIVERSITY HOSPITAL NHS TRUST"]
  #   ###########################################################################

  nhs_trusts_catchment_areas <- readRDS(file = paste0(data_directory,
                                                      "/nhs_trusts_catchment_areas_2026.rds"))

  nhs_trusts_catchment_areas <- nhs_trusts_catchment_areas[year == 2024]

  catchment_areas <- merge(sites,
                           nhs_trusts_catchment_areas,
                           by = "ods_code",
                           all.x = TRUE)

  if(catchment_areas[is.na(trust_name), .N] > 0) {
    warning("Not all sites found in catchment area data.")
  }

  catchment_areas <- catchment_areas[(largest_share_in_msoa), .(site_name,
                                                                ods_name,
                                                                msoa21)]

  # Frimley failed to submit data for half of 2022/23,
  #  use 2022 catchment areas to include (potentially) missing MSOAs

  frimley_catchment_area <- readRDS(file = paste0(data_directory,
                                                  "/nhs_trusts_catchment_areas_2022.rds"))

  msoa11_to_msoa21_lookup <- readRDS(file = paste0(data_directory,
                                                   "/msoa11_to_msoa21_lookup.rds"))

  frimley_catchment_area <- frimley_catchment_area[
    year == 2020 &
      trust_name == "Frimley Health NHS Foundation Trust" &
      (largest_share_in_msoa)
  ]

  frimley_catchment_area <- merge(frimley_catchment_area,
                                  msoa11_to_msoa21_lookup,
                                  by = "msoa11",
                                  all.x = TRUE)

  if(frimley_catchment_area[is.na(msoa21), .N] > 0)
    stop("Missing 2021 MSOA value for 2011 MSOA in lookup.")

  frimley_catchment_area[, msoa11 := NULL]
  frimley_catchment_area <- unique(frimley_catchment_area)

  frimley_catchment_area <- frimley_catchment_area[!(msoa21 %in% catchment_areas$msoa21)]

  frimley_catchment_area <- merge(frimley_catchment_area,
                                  sites,
                                  by = "ods_code",
                                  all.x = TRUE)

  if(frimley_catchment_area[is.na(site_name), .N] > 0) {
    warning("Site not found in sites data.")
  }

  frimley_catchment_area <- frimley_catchment_area[, .(site_name,
                                                       ods_name,
                                                       msoa21)]

  catchment_areas <- rbind(catchment_areas,
                           frimley_catchment_area)

  return(catchment_areas)
}
