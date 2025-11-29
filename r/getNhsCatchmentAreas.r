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

  nhs_trusts_catchment_areas <- nhs_trusts_catchment_areas[year == 2020]

  catchment_areas <- merge(sites,
                           nhs_trusts_catchment_areas,
                           by = "ods_code",
                           all.x = TRUE)

  if(catchment_areas[is.na(trust_name), .N] > 0) {
    warning("Not all sites found in catchment area data.")
  }

  catchment_areas <- catchment_areas[(largest_share_in_msoa), .(site_name,
                                                                ods_name,
                                                                msoa11)]

  return(catchment_areas)
}
