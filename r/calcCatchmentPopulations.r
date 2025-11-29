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

  nhs_trusts_patients <- readRDS(file = paste0(data_directory,
                                                      "/nhs_trusts_patients.rds"))

  site_patients_ex_catchments <- merge(sites,
                                       nhs_trusts_patients[year == 2020 &
                                                             !(msoa11 %in% catchment_areas$msoa11)],
                                       by = "ods_code",
                                       all.x = TRUE)

  if(site_patients_ex_catchments[is.na(trust_name), .N] > 0) {
    warning("Not all sites found in NHS Trust patient count data.")
  }

  site_patients_ex_catchments <- site_patients_ex_catchments[, .(patients_3years_outside_catchment_areas = sum(msoa_trust_patients_3years)),
                                                             by = .(site_name,
                                                                    ods_name)]


  catchment_area_populations <- merge(catchment_areas,
                                      msoa11_2020_population,
                                      by = "msoa11",
                                      all.x = TRUE)


  if(catchment_area_populations[is.na(population), .N] > 0) {
    warning("Not all MSOAs found in population data.")
  }

  catchment_area_populations <- catchment_area_populations[,
                                                           .(catchment_population = sum(population)),
                                                           by = .(site_name,
                                                                  ods_name)]


  catchment_area_populations_site_patients <- merge(site_patients_ex_catchments,
                                                    catchment_area_populations,
                                                    by = c("site_name",
                                                           "ods_name"),
                                                    all = TRUE)

  catchment_area_populations_site_patients <- rbind(catchment_area_populations_site_patients[, .(site_name,
                                                                                                 ods_name,
                                                                                                 catchment_population,
                                                                                                 patients_3years_outside_catchment_areas)],
                                                    catchment_area_populations_site_patients[,
                                                                                             .(ods_name = "TOTAL",
                                                                                               catchment_population = sum(catchment_population,
                                                                                                                          na.rm = TRUE),
                                                                                               patients_3years_outside_catchment_areas = sum(patients_3years_outside_catchment_areas,
                                                                                                                                             na.rm = TRUE)),
                                                                                             by = site_name],
                                                    catchment_area_populations_site_patients[,
                                                                           .(site_name = "TOTAL",
                                                                             ods_name = "TOTAL",
                                                                             catchment_population = sum(catchment_population,
                                                                                                        na.rm = TRUE),
                                                                             patients_3years_outside_catchment_areas = sum(patients_3years_outside_catchment_areas,
                                                                                                        na.rm = TRUE))])

  data.table::fwrite(catchment_area_populations_site_patients,
                     file = "data-out/catchment_area_2020_populations_site_patients_2018-2021.csv")
}
