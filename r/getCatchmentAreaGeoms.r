#' Save Catchment Area Geoms
#'
#' @param data_directory
#' @param catchment_areas
#' @param crs
#'
#' @return
#' @export
#'
#' @examples
getCatchmentAreaGeoms <- function(data_directory,
                                  catchment_areas,
                                  crs = 27700,
                                  save = FALSE) {

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

  if(crs == 27700) {
    site_catchment_areas <- catchment_areas_geom_27700
  } else {
    site_catchment_areas <- catchment_areas_geom_27700 |>
      sf::st_transform(crs)
  }

  if(save) {
    saveRDS(site_catchment_areas,
            paste0(data_directory,
                   "/site_catchment_areas_",
                   crs,
                   ".rds"))
    return(TRUE)
  } else {
    return(site_catchment_areas)
  }
}
