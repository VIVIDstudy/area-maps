
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

  catchment_areas <- getNhsCatchmentAreas(sites_csv,
                                          data_directory)

  # Catchment area boundaries
  catchment_areas_geom_4326 <- getCatchmentAreaGeoms(data_directory,
                                                     catchment_areas,
                                                     crs = 4326,
                                                     save = TRUE)

  catchment_areas_geom_27700 <- getCatchmentAreaGeoms(data_directory,
                                                      catchment_areas,
                                                      crs = 27700,
                                                      save = FALSE)

  # Country boundaries
  uk_countries_goem <- readRDS(paste0(data_directory,
                                      "/uk_countries_goem.rds"))

  uk_countries_goem <- uk_countries_goem[uk_countries_goem$CTRY24NM != "Northern Ireland",]

  # Town and city boundaries
  towns_cities_goem <- readRDS(paste0(data_directory,
                                      "/towns_cities_goem.rds"))

  towns_cities_goem <- towns_cities_goem |>
    sf::st_convex_hull()

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
