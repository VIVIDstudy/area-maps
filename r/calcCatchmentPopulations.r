#' Calculate Catchment Populations
#'
#' @param sites_csv
#' @param data_directory
#'
#' @return
#' @export
#'
#' @examples
calcCatchmentPopulations <- function(sites_csv,
                                     data_directory = "data",
                                     dataout_directory = "data-out") {

  if(!dir.exists(dataout_directory)) dir.create(dataout_directory)

  catchment_areas <- getNhsCatchmentAreas(sites_csv,
                                          data_directory)

  msoa11_2020_population <- readRDS(paste0(data_directory,
                                      "/msoa11_2020_population.rds"))

  catchment_areas <- merge(catchment_areas,
                                          msoa11_2020_population,
                                          by = "msoa11",
                                          all.x = TRUE)

  catchment_areas_2020_populations <- catchment_areas[,
                                                     .(population = sum(population)),
                                                     by = ods_name]

  catchment_areas_2020_populations <- rbind(catchment_areas_2020_populations,
                                           catchment_areas_2020_populations[,
                                                                           .(ods_name = "TOTAL",
                                                                             population = sum(population))])

  data.table::fwrite(catchment_areas_2020_populations,
                     file = "data-out/catchment_areas_2020_populations.csv")
}
