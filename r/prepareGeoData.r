
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

# Catchment area lookup
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

  # Catchment area boundaries
  england_wales_msoa11_goem <- readRDS(file = paste0(data_directory,
                                                     "/england_wales_msoa11_goem.rds"))

  catchment_areas_geom_27700 <- merge(england_wales_msoa11_goem,
                                      catchment_areas,
                                      by.x = "MSOA11CD",
                                      by.y = "msoa11",
                                      all = FALSE) |>
    sf::st_buffer(20) |>
    sf::st_union() |>
    sf::st_sf()

  site_catchment_areas_4326 <- catchment_areas_geom_27700 |>
    sf::st_transform(4326)

  saveRDS(site_catchment_areas_4326,
          paste0(data_directory,
                 "/site_catchment_areas_4326.rds"))

  rm(england_wales_msoa11_goem,
     site_catchment_areas_4326)

  # postcode lookups
  postcode_to_bng_msoa11_lookup <- readRDS(file = paste0(data_directory,
                                                         "/postcode_to_bng_msoa11_lookup.rds"))

  postcode_to_bng_msoa11_lookup <- postcode_to_bng_msoa11_lookup[substr(postcode, 1, 4) != "ZZ99"]

  postcode_geom_4326 <- postcode_to_bng_msoa11_lookup[!is.na(oseast1m),
                                                      .(postcode,
                                                        oseast1m,
                                                        osnrth1m)] |>
    sf::st_as_sf(coords = c("oseast1m",
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
                                         in_england = (substr(msoa11, 1, 1) == "E"))]

  postcode_district_catchment_area_lookup <- postcode_catchment_area_lookup[, .(in_catchment_area,
                                                                                in_england,
                                                                                postcode_district = substr(postcode,
                                                                                                           1,
                                                                                                           nchar(postcode) - 4))][, .(in_catchment_area = any(in_catchment_area),
                                                                                                                                      in_england = any(in_england)),
                                                                                                                                  by = postcode_district]

  postcode_catchment_area_lookup <- postcode_catchment_area_lookup[(in_england)]

  postcode_catchment_area_lookup[, c("msoa11",
                                     "oseast1m",
                                     "osnrth1m",
                                     "ods_name",
                                     "in_england") := NULL]

  postcode_geom_4326$postcode_district = substr(postcode_geom_4326$postcode,
                                                1,
                                                nchar(postcode_geom_4326$postcode) - 4)

  postcode_districts <- unique(postcode_geom_4326$postcode_district)

  postcode_districts_matrix <- sapply(postcode_districts,
                                    function(district, postcode_points_geom) {
                                      return(postcode_points_geom[postcode_points_geom$postcode_district == district,] |>
                                               sf::st_union() |>
                                               sf::st_convex_hull() |>
                                               sf::st_centroid() |>
                                               sf::st_coordinates())
  },
  postcode_points_geom = postcode_geom_4326)

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

  rm(postcode_geom_4326,
     postcode_districts_matrix,
     postcode_districts_coords)

  saveRDS(postcode_catchment_area_lookup,
          file = paste0(data_directory,
                        "/postcode_catchment_area_lookup.rds"))

  saveRDS(postcode_district_catchment_area_lookup,
          file = paste0(data_directory,
                        "/postcode_district_catchment_area_lookup.rds"))

  rm(postcode_catchment_area_lookup,
     postcode_district_catchment_area_lookup)


  # Country boundaries
  uk_countries_goem <- readRDS(paste0(data_directory,
                                      "/uk_countries_goem.rds"))

  uk_countries_goem <- uk_countries_goem[uk_countries_goem$CTRY24NM != "Northern Ireland",]

  # Town and city boundaries
  towns_cities_goem <- readRDS(paste0(data_directory,
                                      "/towns_cities_goem.rds"))

  towns_cities_centroids_geom <- towns_cities_goem |>
    sf::st_centroid()

  towns_cities_centroids_bng <- towns_cities_centroids_geom |>
    sf::st_coordinates() |>
    data.table::data.table() |>
    cbind(TCITY15NM = towns_cities_centroids_geom$TCITY15NM)

  data.table::setnames(towns_cities_centroids_bng,
                       c("X",
                         "Y"),
                       c("easting",
                         "northing"))

  towns_cities_centroids_bng <- towns_cities_centroids_bng[TCITY15NM %in% c("London",
                                                                            "Birmingham",
                                                                            "Bristol",
                                                                            "Cambridge",
                                                                            "Bracknell",
                                                                            "Leeds",
                                                                            "Leicester",
                                                                            "Liverpool",
                                                                            "Manchester",
                                                                            "Newcastle upon Tyne",
                                                                            "Nottingham",
                                                                            "Southampton",
                                                                            "Plymouth",
                                                                            "Sheffield")]

  towns_cities_goem <- merge(towns_cities_goem,
                             towns_cities_centroids_bng,
                             by = "TCITY15NM",
                             all.x = TRUE)

  geom_data <- list(uk_countries_goem = uk_countries_goem,
                    catchment_areas_geom_27700 = catchment_areas_geom_27700,
                    towns_cities_goem = towns_cities_goem)

  return(geom_data)

}
