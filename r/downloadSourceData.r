
#' Download Source Data
#'
#' Download the source datasets to build the VIVID Site Map
#'
#' @param data_raw_directory Character vector of length one, the path of the data-raw directory
#' @param data_directory Character vector of length one, the path of the data directory
#'
#' @return Logical vector of length one with value TRUE on success.
#' @export
downloadSourceData <- function(data_raw_directory = "data-raw",
                               data_directory = "data") {

  if(!dir.exists(data_raw_directory)) dir.create(data_raw_directory)
  if(!dir.exists(data_directory)) dir.create(data_directory)

  # postcode lookup (NHS 2025 August)

  postcode_to_bng_msoa11_lookup_filepath <- downloadExtractZipFile(url = "https://www.arcgis.com/sharing/rest/content/items/402152391b1f459598a8ce5bed1b6cf8/data",
                                                            unzip_directory = data_raw_directory,
                                                            unzip_files = "Data/nhg25aug.csv")

  postcode_to_bng_msoa11_lookup <- data.table::fread(postcode_to_bng_msoa11_lookup_filepath,
                                              header = FALSE,
                                              select = c(2,37:38,41),
                                              col.names = c("postcode",
                                                            "oseast1m",
                                                            "osnrth1m",
                                                            "msoa11"))

  saveRDS(postcode_to_bng_msoa11_lookup,
          file = paste0(data_directory,
                        "/postcode_to_bng_msoa11_lookup.rds"))

  rm(postcode_to_bng_msoa11_lookup,
     postcode_to_bng_msoa11_lookup_filepath)



  # MSOA 2011 boundaries

  england_wales_msoa11_goem_filepath <- downloadArcGISGeoPackage("54bc14349a5543d7996e7a1e3565dd06",
                                                               directory_path = data_raw_directory)

  england_wales_msoa11_goem <- sf::st_read(england_wales_msoa11_goem_filepath,
                                         query = "SELECT * FROM MSOA_2011_EW_BGC_V3")

  saveRDS(england_wales_msoa11_goem,
          file = paste0(data_directory,
                        "/england_wales_msoa11_goem.rds"))

  rm(england_wales_msoa11_goem,
     england_wales_msoa11_goem_filepath)

  # UK Countries (December 2024) boundaries

  uk_countries_goem_filepath <- downloadArcGISGeoPackage("6f18dfc308d04372929dea6afa44b2c7",
                                                         directory_path = data_raw_directory)

  uk_countries_goem <- sf::st_read(uk_countries_goem_filepath,
                                   query = "SELECT * FROM CTRY_DEC_2024_UK_BSC")

  saveRDS(uk_countries_goem,
          file = paste0(data_directory,
                        "/uk_countries_goem.rds"))

  rm(uk_countries_goem,
     uk_countries_goem_filepath)

  # Towns and Cities (December 2015) boundaries

  towns_cities_goem_filepath <- downloadArcGISGeoPackage("63a109c64a64410488d39c886152c162",
                                                         directory_path = data_raw_directory)

  towns_cities_goem <- sf::st_read(towns_cities_goem_filepath,
                                   query = "SELECT * FROM TCITY_2015_EW_BGG")

  saveRDS(towns_cities_goem,
          file = paste0(data_directory,
                        "/towns_cities_goem.rds"))

  rm(towns_cities_goem,
     towns_cities_goem_filepath)



  # OHID MSOA catchment populations
  # Download from https://app.box.com/s/qh8gzpzeo1firv1ezfxx2e6c4tgtrudl/file/976234504165

  nhs_trusts_catchment_areas_filename <- paste0(data_raw_directory,
                                                "/2022 Trust Catchment Populations_Supplementary MSOA Analysis.xlsx")

  nhs_trusts_catchment_areas <- openxlsx::read.xlsx(nhs_trusts_catchment_areas_filename,
                                                    sheet = "Emergency",
                                                    cols = c(1,3:5,11)) |>
    data.table::setDT()

  data.table::setnames(nhs_trusts_catchment_areas,
                       c("CatchmentYear",
                         "msoa",
                         "TrustCode",
                         "TrustName",
                         "FPTP"),
                       c("year",
                         "msoa11",
                         "ods_code",
                         "trust_name",
                         "largest_share_in_msoa"))

  saveRDS(nhs_trusts_catchment_areas,
          file = paste0(data_directory,
                        "/nhs_trusts_catchment_areas.rds"))

  rm(nhs_trusts_catchment_areas,
     nhs_trusts_catchment_areas_filename)


  return(TRUE)
}
