#' Prepare Data for Interactive Map
#'
#' @param sites_csv
#' @param data_directory
#'
#' @return
#' @export
#'
#' @examples
prepareInteractiveData <- function(sites_csv,
                                   data_directory = "data",
                                   dataout_directory = "data-out") {

  if(!dir.exists(dataout_directory)) dir.create(dataout_directory)

  catchment_areas <- getNhsCatchmentAreas(sites_csv,
                                          data_directory)

  # Catchment area boundaries
  england_wales_msoa11_goem <- readRDS(file = paste0(data_directory,
                                                     "/england_wales_msoa11_goem.rds"))

  site_catchment_areas_4326 <- merge(england_wales_msoa11_goem,
                                     catchment_areas,
                                     by.x = "MSOA11CD",
                                     by.y = "msoa11",
                                     all = FALSE) |>
    sf::st_buffer(20) |>
    sf::st_union() |>
    sf::st_sf()|>
    sf::st_transform(4326)

  saveRDS(site_catchment_areas_4326,
          paste0(dataout_directory,
                 "/site_catchment_areas_4326.rds"))

  rm(england_wales_msoa11_goem,
     site_catchment_areas_4326)

  # postcode lookups
  postcode_to_bng_msoa11_lookup <- readRDS(file = paste0(data_directory,
                                                         "/postcode_to_bng_msoa11_lookup.rds"))

  ## Remove NHS pseudo-postcodes
  postcode_to_bng_msoa11_lookup <- postcode_to_bng_msoa11_lookup[substr(postcode, 1, 4) != "ZZ99"]

  postcode_gb_geom_4326 <- postcode_to_bng_msoa11_lookup[!is.na(oseast1m) & substr(postcode, 1, 2) != "BT",
                                                         .(postcode,
                                                           oseast1m,
                                                           osnrth1m)] |>
    sf::st_as_sf(coords = c("oseast1m",
                            "osnrth1m"),
                 crs = 27700) |>
    sf::st_transform(4326)

  postcode_ni_geom_4326 <- postcode_to_bng_msoa11_lookup[!is.na(oseast1m) & substr(postcode, 1, 2) == "BT",
                                                         .(postcode,
                                                           oseast1m,
                                                           osnrth1m)] |>
    sf::st_as_sf(coords = c("oseast1m",
                            "osnrth1m"),
                 crs = 29902) |>
    sf::st_transform(4326)

  postcode_uk_geom_4326 <- rbind(postcode_gb_geom_4326,
                                 postcode_ni_geom_4326)

  rm(postcode_gb_geom_4326,
     postcode_ni_geom_4326)
  gc()

  postcode_latlong <- postcode_uk_geom_4326 |>
    sf::st_coordinates() |>
    data.table::data.table() |>
    cbind(postcode = postcode_uk_geom_4326$postcode)

  data.table::setnames(postcode_latlong,
                       c("X",
                         "Y"),
                       c("longitude",
                         "latitude"))

  postcode_latlong[, ':=' (longitude = round(longitude, 5),
                           latitude = round(latitude, 5))]

  postcode_catchment_area_lookup <- merge(postcode_to_bng_msoa11_lookup,
                                          catchment_areas,
                                          by = "msoa11",
                                          all.x = TRUE)

  postcode_catchment_area_lookup <- merge(postcode_catchment_area_lookup,
                                          postcode_latlong,
                                          by = "postcode",
                                          all.x = TRUE)

  rm(postcode_to_bng_msoa11_lookup,
     postcode_latlong,
     catchment_areas)
  gc()

  postcode_catchment_area_lookup[, ':=' (in_catchment_area = !is.na(ods_name),
                                         postcode_district = substr(postcode,
                                                                    1,
                                                                    nchar(postcode) - 4))]

  postcode_catchment_area_lookup[, district_in_catchment_area := any(in_catchment_area),
                                 by = postcode_district]

  postcode_district_catchment_area_lookup <- unique(postcode_catchment_area_lookup[, .(postcode_district,
                                                                                       in_catchment_area = district_in_catchment_area)])

  postcode_catchment_area_lookup <- postcode_catchment_area_lookup[(district_in_catchment_area)]

  postcode_catchment_area_lookup[, c("msoa11",
                                     "oseast1m",
                                     "osnrth1m",
                                     "ods_name",
                                     "district_in_catchment_area") := NULL]

  postcode_uk_geom_4326$postcode_district = substr(postcode_uk_geom_4326$postcode,
                                                1,
                                                nchar(postcode_uk_geom_4326$postcode) - 4)

  postcode_districts <- unique(postcode_uk_geom_4326$postcode_district)

  postcode_districts_matrix <- sapply(postcode_districts,
                                      function(district, postcode_points_geom) {
                                        return(postcode_points_geom[postcode_points_geom$postcode_district == district,] |>
                                                 sf::st_union() |>
                                                 sf::st_centroid() |>
                                                 sf::st_coordinates())
                                      },
                                      postcode_points_geom = postcode_uk_geom_4326)

  postcode_districts_coords <- t(postcode_districts_matrix) |>
    data.table::data.table()

  data.table::setnames(postcode_districts_coords,
                       c("longitude",
                         "latitude"))

  postcode_districts_coords[, ':=' (longitude = round(longitude, 5),
                                    latitude = round(latitude, 5),
                                    postcode_district = colnames(postcode_districts_matrix))]

  postcode_district_catchment_area_lookup <- merge(postcode_district_catchment_area_lookup,
                                                   postcode_districts_coords,
                                                   by = "postcode_district",
                                                   all.x = TRUE)

  rm(postcode_uk_geom_4326,
     postcode_districts_matrix,
     postcode_districts_coords)

  saveRDS(postcode_catchment_area_lookup,
          file = paste0(dataout_directory,
                        "/postcode_catchment_area_lookup.rds"))

  saveRDS(postcode_district_catchment_area_lookup,
          file = paste0(dataout_directory,
                        "/postcode_district_catchment_area_lookup.rds"))

}
