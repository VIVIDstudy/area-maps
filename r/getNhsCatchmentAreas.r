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

  return(catchment_areas)

}
