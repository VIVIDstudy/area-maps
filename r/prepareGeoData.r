
#' Prepare Geometry Data
#'
#' Prepare geometry data for plot
#'
#' @param sites_csv Character vector of length one, the full path to CSV file of the sites to be plotted (and their postcodes)
#' @param data_directory Character vector of length one, the path to the data directory
#'
#' @return List of geometry data for plot
prepareGeoData <- function(sites_csv,
                           data_directory = "data") {

  sites <- data.table::fread(sites_csv,
                             select = c("site_name",
                                        "ods_name",
                                        "ods_code"))


  nhs_trusts_catchment_areas <- readRDS(file = paste0(data_directory,
                                                      "/nhs_trusts_catchment_areas.rds"))

  nhs_trusts_catchment_areas <- nhs_trusts_catchment_areas[year == 2020 & (largest_share_in_msoa)]
  nhs_trusts_catchment_areas[, c("year",
                                 "largest_share_in_msoa") := NULL]

  catchment_areas <- merge(sites,
                           nhs_trusts_catchment_areas,
                           by = "ods_code",
                           all = FALSE)

  catchment_areas[, c("ods_code",
                      "site_name",
                      "trust_name") := NULL]

  rm(sites,
     nhs_trusts_catchment_areas)


  england_wales_msoa11_goem <- readRDS(file = paste0(data_directory,
                                                     "/england_wales_msoa11_goem.rds"))

  catchment_areas_geom_27700 <- merge(england_wales_msoa11_goem,
                                      catchment_areas,
                                      by.x = "MSOA11CD",
                                      by.y = "msoa11",
                                      all = FALSE) |>
    sf::st_union() |>
    sf::st_sf()

  site_catchment_areas_4326 <- sf::st_transform(catchment_areas_geom_27700,
                                                4326)

  saveRDS(site_catchment_areas_4326,
          paste0(data_directory,
                 "/site_catchment_areas_4326.rds"))

  rm(england_wales_msoa11_goem,
     site_catchment_areas_4326)


  postcode_to_bng_msoa11_lookup <- readRDS(file = paste0(data_directory,
                                                         "/postcode_to_bng_msoa11_lookup.rds"))

  postcode_to_bng_msoa11_lookup <- postcode_to_bng_msoa11_lookup[substr(postcode, 1, 4) != "ZZ99"]

  postcode_geom_4326 <- sf::st_as_sf(postcode_to_bng_msoa11_lookup[!is.na(oseast1m),
                                                                   .(postcode,
                                                                     oseast1m,
                                                                     osnrth1m)],
                                     coords = c("oseast1m",
                                                "osnrth1m"),
                                     crs = 27700) |>
    sf::st_transform(4326)

  postcode_latlong <- postcode_geom_4326 |>
    sf::st_coordinates() |>
    data.table::data.table() |>
    cbind(postcode = postcode_geom_4326$postcode)

  data.table::setnames(postcode_latlong,
                       c("X",
                         "Y"),
                       c("longitude",
                         "latitude"))

  rm(postcode_geom_4326)


  catchment_area_postcode_lookup <- merge(postcode_to_bng_msoa11_lookup,
                                         catchment_areas,
                                         by = "msoa11",
                                         all.x = TRUE)

  catchment_area_postcode_lookup <- merge(catchment_area_postcode_lookup,
                                          postcode_latlong,
                                          by = "postcode",
                                          all.x = TRUE)

  rm(postcode_to_bng_msoa11_lookup,
     postcode_latlong)

  gc()

  catchment_area_postcode_lookup[, in_catchment_area := !is.na(ods_name)]
  catchment_area_postcode_lookup[, c("msoa11",
                                     "oseast1m",
                                     "osnrth1m",
                                     "ods_name") := NULL]




        return(list(sites_geom = sites_geom,
                    england_wales_population_density_geom = england_wales_population_density_geom,
                    england_regions_goem = england_regions_goem))

}
