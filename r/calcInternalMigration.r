#' Calculate Internal Migration
#'
#' @param sites_csv
#' @param data_directory
#'
#' @return
#' @export
#'
#' @examples
calcInternalMigration <- function(sites_csv,
                                  data_directory = "data",
                                  dataout_directory = "data-out") {

  if(!dir.exists(dataout_directory)) dir.create(dataout_directory)

  catchment_areas <- getNhsCatchmentAreas(sites_csv,
                                          data_directory)

  msoa11_lad20_lookup <- readRDS(paste0(data_directory,
                                        "/msoa11_to_lad20_lookup.rds"))

  lad23_2024_internal_migration <- readRDS(file = paste0(data_directory,
                                                         "/lad23_2024_internal_migration.rds"))

  lad20_areas <- msoa11_lad20_lookup[msoa11 %in% catchment_areas$msoa11, unique(lad20)]

  lad23_2024_internal_migration[lad23_in %in% lad20_areas &
                                  !(lad23_out %in% lad20_areas),
                                formatC(round(sum(people),
                                              -3),
                                        format = "d")]
}
